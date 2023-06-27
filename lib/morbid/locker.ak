use aiken/builtin.{add_integer}
use aiken/list.{filter, find, has, map, reduce, unique, unzip}
use aiken/time.{PosixTime}
use aiken/transaction.{InlineDatum, ScriptContext, ScriptPurpose, Spend}
use aiken/transaction/credential.{
  Address, ScriptCredential, VerificationKeyCredential,
}
use aiken/transaction/value.{lovelace_of}
use morbid/action.{Action, DelayUnlock, UnlockChest}
use morbid/alias.{PubKeyHash, TxHash}
use morbid/chain.{and}
use time_util/cheap/posixtime.{is_after}
use time_util/unsafe/bound as unsafe_unwrap

/// When delaying unlock of a UTxO, the transaction must include a `ChestKey` NFT
/// (it will be used to be matched against the `ChestLock`), the sum of input and
/// output ADA must be balance, and it must re-send the `ChestLock` NFT with a
/// later deadline as the datum.
/// When unlocking the chest of a UTxO, the transaction validity range must be
/// after the deadline.
pub fn validate(
  chest_name: AssetName,
  policy_id: PolicyId,
  datum _maybe_deadline: Data,
  redeemer: Data,
  context ctx: ScriptContext,
) -> Bool {
  expect Spend(_): ScriptPurpose = ctx.purpose
  expect action: Action = redeemer
  when action is {
    DelayUnlock -> ctx |> validate_delay_unlock(policy_id, chest_name)
    UnlockChest -> ctx |> validate_unlock_chest()
  }
}

pub fn mint_chest(
  utxo_ref: OutputReference,
  chest_name: AssetName,
  redeemer action: Data,
  context ctx: ScriptContext,
) {
  todo @"implement `locker.mint_chest` validation fn"
}

// //////////////////////////////// VALIDATOR HELPER FUNCTIONS //////////////////////////////// //
//      This section contains high-level validation requirements for each type of redeemer      //

fn validate_delay_unlock(
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_name: AssetName,
) -> Bool {
  must_not_drain(ctx) && must_resend_chest_lock(ctx, policy_id, chest_name)
}

fn validate_unlock_chest(ctx: ScriptContext) -> Bool {
  must_unlock_after_deadline(ctx)
}

// ////////////////////////////////////// TRACE FUNCTIONS ////////////////////////////////////// //
//            This section contains functions for tracing during the validation steps            //

/// Validate delay unlock: Input and Output Lovelace must be balance.
fn must_not_drain(_, context ctx: ScriptContext) -> Bool {
  let not_drained = ctx |> is_not_drained()
  not_drained?
}

/// `DelayUnlock`: Must re-create the chest.
fn must_recreate_chest(_, context ctx: ScriptContext) -> Bool {
  let recreated_chest = ctx |> recreate_chest()
  recreated_chest?
}

/// Must re-create chest: Transaction must be signed by the new owner
/// when transferring the ownership of the chest.
fn must_be_signed_by_new_creator(
  _,
  context ctx: ScriptContext,
  new_creator: PubKeyHash,
) -> Bool {
  let signed_by_new_creator = ctx |> is_signed_by(new_creator)
  signed_by_new_creator?
}

/// Must re-create chest: Disallow setting the past as the new deadline.
fn must_postpone_after_now(
  _,
  context ctx: ScriptContext,
  new_deadline: PosixTime,
) -> Bool {
  let postponed_after_now =
    (unsafe_unwrap.finite_start_of(ctx.transaction.validity_range) < new_deadline)?
  postponed_after_now?
}

// ///////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////// //
//       This section contains the functions to support the validation and chain functions      //

fn is_signed_by(context ctx: ScriptContext, signer: PubKeyHash) -> Bool {
  ctx.transaction.extra_signatories |> has(signer)
}

fn is_not_drained(context ctx: ScriptContext) -> Bool {
  let txn = ctx.transaction
  //
  // Anonymous fn to filter out non script addresses:
  let script_address =
    fn(address: Address) {
      when address.payment_credential is {
        ScriptCredential(_) -> True
        VerificationKeyCredential(_) -> False
      }
    }
  //
  // Anonymous fn to collect values only from script address:
  let lovelace_of_script_address =
    fn(output: transaction.Output) {
      if output.address |> script_address() {
        lovelace_of(output.value)
      } else {
        0
      }
    }
  //
  // Anonymous fn to map `transaction.Output` to (lovelace, address):
  let lovelace_address =
    fn(output: transaction.Output) {
      (output |> lovelace_of_script_address(), output.address)
    }
  //
  // Sum Input Lovelace, Validate Input Address:
  let (input_lovelace, input_address) =
    txn.inputs
      |> map(fn(input) { input.output |> lovelace_address() })
      |> unzip()
  expect [input_script_address] =
    input_address
      |> filter(script_address)
      |> unique()
  let input_lovelace = input_lovelace |> sum_ints()
  //
  // Sum Output Lovelace, Validate Output Address:
  let (output_lovelace, output_address) =
    txn.outputs
      |> map(lovelace_address)
      |> unzip()
  expect [output_script_address] =
    output_address
      |> filter(script_address)
      |> unique()
  let output_lovelace = output_lovelace |> sum_ints()
  //
  // Return:
  (input_lovelace == output_lovelace)? && (input_script_address == output_script_address)?
}

fn recreate_chest(context ctx: ScriptContext) -> Bool {
  let outputs =
    ctx.transaction.outputs
      |> filter(
           fn(output) {
             when output.datum is {
               InlineDatum(_) -> True
               _ -> False
             }
           },
         )
  when outputs is {
    [output] ->
      when output.datum is {
        InlineDatum(datum) -> {
          expect CreateChest(new_deadline, new_creator): ChestDatum = datum
          chain.validate()
            |> and(must_be_signed_by_new_creator(_, ctx, new_creator))
            |> and(must_postpone_after_now(_, ctx, new_deadline))
        }
        // couldn't destructure datum:
        _ -> False
      }
    // invalid number of datum(s)
    _ -> False
  }
}

fn find_ref_datum(
  context ctx: ScriptContext,
  ref_txn: TxHash,
) -> Option<ChestDatum> {
  //
  // find the referenced tx-input
  let tx_in: Option<transaction.Input> =
    ctx.transaction.inputs
      |> find(fn(tx_in) { tx_in.output_reference.transaction_id.hash == ref_txn })
  //
  // the UTxO of the referenced tx-input must contain a datum of type ChestDatum
  when tx_in is {
    Some(tx_in) ->
      when tx_in.output.datum is {
        InlineDatum(datum) -> {
          expect datum: ChestDatum = datum
          Some(datum)
        }
        // couldn't destructure datum:
        _ -> None
      }
    // referenced tx-input not found:
    None -> None
  }
}

// ///////////////////////////////////// UTILITY FUNCTIONS ///////////////////////////////////// //
//            This section contains support function(s) not specific to the contract             //

fn sum_ints(ints: List<Int>) -> Int {
  ints |> reduce(0, add_integer)
}
