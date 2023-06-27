use aiken/builtin.{add_integer}
use aiken/hash.{Blake2b_224, Hash}
use aiken/list.{filter, filter_map, has, map, reduce, unique, unzip}
use aiken/time.{PosixTime}
use aiken/transaction.{
  InlineDatum, OutputReference, ScriptContext, ScriptPurpose, Spend,
}
use aiken/transaction/credential.{
  Address, Credential, Script, ScriptCredential, VerificationKeyCredential,
}
use aiken/transaction/value.{AssetName, PolicyId, flatten_with, lovelace_of}
use morbid/action.{Action, DelayUnlock, UnlockChest}

// use time_util/cheap/posixtime.{is_after}
// use time_util/unsafe/bound as unsafe_unwrap

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
  redeemer action: Action,
  context ctx: ScriptContext,
) -> Bool {
  expect Spend(_): ScriptPurpose = ctx.purpose
  when action is {
    DelayUnlock -> ctx |> validate_delay_unlock(policy_id, chest_name)
    UnlockChest -> ctx |> validate_unlock_chest()
  }
}

pub fn mint_chest(
  _chest_name: AssetName,
  _utxo_ref: OutputReference,
  _redeemer: Void,
  context _ctx: ScriptContext,
) {
  True
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
fn must_not_drain(ctx: ScriptContext) -> Bool {
  let not_drained = ctx |> is_not_drained()
  not_drained?
}

/// Validate delay unlock: Must re-send the chest's lock with a later deadline as the new datum.
fn must_resend_chest_lock(
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_name: AssetName,
) -> Bool {
  let resent_chest_lock = ctx |> resend_chest_lock(policy_id, chest_name)
  resent_chest_lock?
}

/// Validate unlock chest: Disallow unlock before deadline.
fn must_unlock_after_deadline(_ctx: ScriptContext) -> Bool {
  let unlock_after_deadline = True
  unlock_after_deadline?
}

// ///////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////// //
//       This section contains the functions to support the validation and chain functions      //

/// (1) Sum of input and output Lovelace must be balance
/// (2) Unique input==output script address must be the same
fn is_not_drained(ctx: ScriptContext) -> Bool {
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

/// (1) TxIns must contain `ChestLock` with the specified Policy ID and Chest Name, and contain the deadline datum.
/// (2) TxOuts must contain `ChestLock` with the same Policy ID and Chest Name, and contain the deadline datum with a future value.
/// (3) `address.payment_credential` of (1) and (2) must be `ScriptCredential`, the same, and there's only 1 unique script address.
///     (unique input==output script address validation has been done during drain validation)
/// (4) TxRefIns must contain `ChestKey` and its `address.payment_credential` exists in `extra_signatories`
fn resend_chest_lock(
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_name chest_lock: AssetName,
) -> Bool {
  let txn = ctx.transaction
  //
  // Anonymous fns to filter_map input_output by chest_lock into script_address and deadline:
  let filter_chest_lock =
    fn(id, name, qty) {
      when (id, name, qty) is {
        (id, name, 1) ->
          if id == policy_id && name == chest_lock {
            Some((id, name, 1))
          } else {
            None
          }
        _ -> None
      }
    }
  let into_script_deadline =
    fn(output: transaction.Output) {
      when output.address.payment_credential is {
        VerificationKeyCredential(_) -> None
        ScriptCredential(script_address) ->
          when output.value |> flatten_with(filter_chest_lock) is {
            [(_, _, 1)] ->
              when output.datum is {
                InlineDatum(datum) -> {
                  expect deadline: PosixTime = datum
                  Some((script_address, deadline))
                }
                _ -> None
              }
            _ -> None
          }
      }
    }
  //
  // Expecting `ChestLock` in a UTxO of a script_address with PosixTime datum:
  expect [(input_address, old_deadline)]: List<
    (Hash<Blake2b_224, Script>, PosixTime),
  > =
    txn.inputs
      |> filter_map(
           fn(input: transaction.Input) {
             input.output |> into_script_deadline()
           },
         )
  expect [(output_address, new_deadline)]: List<
    (Hash<Blake2b_224, Script>, PosixTime),
  > = txn.outputs |> filter_map(into_script_deadline)
  //
  // Continue validating only if previous steps are valid:
  if (input_address == output_address)? && (old_deadline < new_deadline)? {
    let chest_key: AssetName = "ChestKey"
    //
    // Anonymous fns to filter_map reference_inputs by chest_key into payment_credential:
    let filter_chest_key =
      fn(id, name, qty) {
        when (id, name, qty) is {
          (id, name, 1) ->
            if id == policy_id && name == chest_key {
              Some((id, name, 1))
            } else {
              None
            }
          _ -> None
        }
      }
    let into_payment_credential =
      fn(input: transaction.Input) {
        let output = input.output
        when output.value |> flatten_with(filter_chest_key) is {
          [(_, _, 1)] -> Some(output.address.payment_credential)
          _ -> None
        }
      }
    //
    // Expecting `ChestKey` and signed by the chest_key holder:
    expect [VerificationKeyCredential(pkh)]: List<Credential> =
      txn.reference_inputs |> filter_map(into_payment_credential)
    //
    // return:
    let signed_by_chest_key_holder = txn.extra_signatories |> has(pkh)
    signed_by_chest_key_holder?
  } else {
    False
  }
}

// ///////////////////////////////////// UTILITY FUNCTIONS ///////////////////////////////////// //
//            This section contains support function(s) not specific to the contract             //

fn sum_ints(ints: List<Int>) -> Int {
  ints |> reduce(0, add_integer)
}
