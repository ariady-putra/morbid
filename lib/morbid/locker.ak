// use aiken/builtin.{add_integer}
use aiken/list.{all, filter, find, has, map, reduce, unique, unzip}
use aiken/time.{PosixTime}
use aiken/transaction.{InlineDatum, ScriptContext, Spend}
use aiken/transaction/credential.{
  Address, ScriptCredential, VerificationKeyCredential,
}
use aiken/transaction/value.{flatten, merge, zero}
use morbid/alias.{FlattenValues, PubKeyHash, TxHash}
use morbid/chain.{and}
use morbid/struct/datum.{AddTreasure, ChestDatum, CreateChest}
use morbid/struct/redeemer.{ChestRedeemer, DelayUnlock, UnlockChest}
use time_util/cheap/posixtime.{is_after}
use time_util/unsafe/bound as unsafe_unwrap

/// Depending on the `ChestDatum`, either `CreateChest` or `AddTreasure`,
/// the validator may need to search for the referenced `TxHash`.
/// `ChestRedeemer` determines what action is to be done to the UTxO(s),
/// either `DelayUnlock` or `UnlockChest`.
/// When delaying unlock of a UTxO, the transaction must be signed by the
/// chest creator, the sum of input and output assets must be balance,
/// and it must re-create a `CreateChest` datum with a later deadline.
/// When unlocking the chest of a UTxO, the transaction validity range
/// must be after the deadline.
pub fn validate(
  datum: ChestDatum,
  redeemer action: ChestRedeemer,
  context ctx: ScriptContext,
) -> Bool {
  when (action, datum, ctx.purpose) is {
    //
    // When delaying unlock for a UTxO with the datum of type CreateChest:
    (DelayUnlock, CreateChest(_, creator), Spend(_)) ->
      ctx |> validate_delay_unlock(creator)
    //
    // When delaying unlock for a UTxO with the datum of type AddTreasure:
    (DelayUnlock, AddTreasure(ref_txn), Spend(_)) ->
      //
      // ref_txn must refer to a UTxO with the datum of type CreateChest
      when ctx |> find_ref_datum(ref_txn) is {
        //
        // referenced-datum found:
        Some(CreateChest(_, creator)) -> ctx |> validate_delay_unlock(creator)
        //
        // referenced-datum not found:
        _ -> False
      }
    //
    // When unlocking the chest for a UTxO with the datum of type CreateChest:
    (UnlockChest, CreateChest(deadline, _), Spend(_)) ->
      ctx |> validate_unlock_chest(deadline)
    //
    // When unlocking the chest for a UTxO with the datum of type AddTreasure:
    (UnlockChest, AddTreasure(ref_txn), Spend(_)) ->
      //
      // ref_txn must refer to a UTxO with the datum of type CreateChest
      when ctx |> find_ref_datum(ref_txn) is {
        //
        // referenced-datum found:
        Some(CreateChest(deadline, _)) -> ctx |> validate_unlock_chest(deadline)
        //
        // referenced-datum not found:
        _ -> False
      }
    //
    // Any other actions/purpose:
    _ -> False
  }
}

// //////////////////////////////// VALIDATOR HELPER FUNCTIONS //////////////////////////////// //
//      This section contains high-level validation requirements for each type of redeemer      //

fn validate_delay_unlock(
  context ctx: ScriptContext,
  creator: PubKeyHash,
) -> Bool {
  chain.validate()
    |> and(must_be_signed_by_creator(_, ctx, creator))
    |> and(must_not_drain(_, ctx))
    |> and(must_recreate_chest(_, ctx))
}

fn validate_unlock_chest(
  context ctx: ScriptContext,
  deadline: PosixTime,
) -> Bool {
  //
  // anyone can unlock after deadline
  ctx.transaction.validity_range |> is_after(deadline)
}

// ////////////////////////////////////// CHAIN FUNCTIONS ////////////////////////////////////// //
//             This section contains functions to be chained using `chain.validate()`            //

/// `DelayUnlock`: Transaction must be signed by the creator.
fn must_be_signed_by_creator(
  _,
  context ctx: ScriptContext,
  creator: PubKeyHash,
) -> Bool {
  let signed_by_creator = ctx |> is_signed_by(creator)
  signed_by_creator?
}

/// `DelayUnlock`: Input and Output Lovelace must be balance.
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

/// Must not drain: All values has value.
fn all_values_has_value(
  _,
  l_values: FlattenValues,
  r_values: FlattenValues,
) -> Bool {
  let all_values_has_value =
    l_values |> all(fn(l_value) { r_values |> has(l_value) })
  all_values_has_value?
}

/// Must not drain: Must be equal.
fn must_be_equal(_, l, r) {
  (l == r)?
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
  let value_of_script_address =
    fn(output: transaction.Output) {
      if output.address |> script_address() {
        output.value
      } else {
        zero()
      }
    }
  //
  // Anonymous fn to map `transaction.Output` to (value, address):
  let value_address =
    fn(output: transaction.Output) {
      (output |> value_of_script_address(), output.address)
    }
  //
  // (Input Values, Input Address):
  let (input_value, input_address) =
    txn.inputs
      |> map(fn(input) { input.output |> value_address() })
      |> unzip()
  expect [input_script_address] =
    input_address
      |> filter(script_address)
      |> unique()
  let input_value =
    input_value
      |> reduce(zero(), merge)
      |> flatten()
  //
  // (Output Values, Output Address):
  let (output_value, output_address) =
    txn.outputs
      |> map(value_address)
      |> unzip()
  expect [output_script_address] =
    output_address
      |> filter(script_address)
      |> unique()
  let output_value =
    output_value
      |> reduce(zero(), merge)
      |> flatten()
  //
  // Return:
  chain.validate()
    |> and(all_values_has_value(_, input_value, output_value))
    |> and(all_values_has_value(_, output_value, input_value))
    |> and(must_be_equal(_, input_script_address, output_script_address))
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

// /// Deprecated
// fn sum_ints(ints: List<Int>) -> Int {
//   ints |> reduce(0, add_integer)
// }
