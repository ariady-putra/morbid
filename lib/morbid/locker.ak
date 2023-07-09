use aiken/builtin.{fst_pair}
use aiken/dict
use aiken/list.{
  all, any, filter, filter_map, find, has, map, reduce, unique, unzip,
}
use aiken/time.{PosixTime}
use aiken/transaction.{
  Datum, InlineDatum, Mint, OutputReference, ScriptContext, ScriptPurpose, Spend,
}
use aiken/transaction/credential.{
  Address, Credential, ScriptCredential, VerificationKeyCredential,
}
use aiken/transaction/value.{
  AssetName, PolicyId, flatten, flatten_with, from_minted_value, merge,
  quantity_of, tokens, zero,
}
use morbid/action.{Action, DelayUnlock, UnlockChest}
use morbid/alias.{FlattenValues, Qty, ValidatorHash}
use morbid/chain.{and}
use morbid/constant.{chest_key}
use string_util/cbor.{print}
use time_util/unsafe/bound as unsafe_unwrap

/// When delaying unlock of a UTxO, the transaction must include any `ChestKey`
/// token(s) signed by the holder(s) (it will be used to be matched against the
/// `ChestLock`), the sum of input and output assets must be balance,
/// and it must re-send the `ChestLock` NFT with a new deadline as the datum.
/// When unlocking the chest of a UTxO, the transaction validity range must be
/// after the deadline.
pub fn validate(
  chest_name chest_lock: AssetName,
  policy_id: PolicyId,
  datum maybe_deadline: Data,
  redeemer action: Action,
  context ctx: ScriptContext,
) -> Bool {
  expect Spend(_): ScriptPurpose = ctx.purpose
  trace "MaybeDeadline" |> print(maybe_deadline)
  when action is {
    DelayUnlock -> ctx |> validate_delay_unlock(policy_id, chest_lock)
    UnlockChest -> ctx |> validate_unlock_chest(policy_id, chest_lock)
  }
}

pub fn mint_chest(
  chest_name chest_lock: AssetName,
  utxo_ref: OutputReference,
  redeemer action: Void,
  context ctx: ScriptContext,
) {
  expect Mint(policy_id): ScriptPurpose = ctx.purpose
  trace "Redeemer" |> print(action)
  ctx |> validate_mint_chest(utxo_ref, policy_id, chest_lock)
}

// //////////////////////////////// VALIDATOR HELPER FUNCTIONS //////////////////////////////// //
//       This section contains high-level validation requirements for each type of action       //

fn validate_mint_chest(
  ctx: ScriptContext,
  utxo_ref: OutputReference,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Bool {
  when ctx |> minted_values_of(policy_id) is {
    // [ChestLock, ChestKey] or [ChestKey, ChestLock]
    [asset_1, asset_2] ->
      chain.validate()
        |> and(must_mint_once(_, ctx, utxo_ref))
        |> and(must_mint_chest_lock(_, [asset_1, asset_2], chest_lock))
        |> and(must_mint_chest_key(_, [asset_1, asset_2], chest_key))
        |> and(must_send_chest_lock(_, ctx, policy_id, chest_lock))
    _ -> {
      let valid_number_of_asset = False
      valid_number_of_asset?
    }
  }
}

fn validate_delay_unlock(
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Bool {
  chain.validate()
    |> and(must_not_drain(_, ctx))
    |> and(must_resend_chest_lock(_, ctx, policy_id, chest_lock))
}

fn validate_unlock_chest(
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Bool {
  ctx |> must_unlock_after_deadline(policy_id, chest_lock)
}

// ////////////////////////////////////// TRACE FUNCTIONS ////////////////////////////////////// //
//            This section contains functions for tracing during the validation steps            //

/// Validate mint chest: Must mint once only.
fn must_mint_once(_, ctx: ScriptContext, utxo_ref: OutputReference) -> Bool {
  let one_shot_minting =
    ctx.transaction.inputs
      |> any(fn(input) { input.output_reference == utxo_ref })
  one_shot_minting?
}

/// Validate mint chest: Must mint exactly one `ChestLock`
fn must_mint_chest_lock(
  _,
  assets: List<(AssetName, Qty)>,
  chest_lock: AssetName,
) -> Bool {
  let minted_chest_lock = assets |> has((chest_lock, 1))
  minted_chest_lock?
}

/// Validate mint chest: Must mint `ChestKey` of any qty.
fn must_mint_chest_key(
  _,
  assets: List<(AssetName, Qty)>,
  chest_key: AssetName,
) -> Bool {
  let minted_chest_key =
    assets |> any(fn(asset) { fst_pair(asset) == chest_key })
  minted_chest_key?
}

/// Validate mint chest: Must send the `ChestLock`
fn must_send_chest_lock(
  _,
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Bool {
  let sent_chest_lock = ctx |> send_chest_lock(policy_id, chest_lock)
  sent_chest_lock?
}

/// Validate delay unlock: Input and Output assets must be balance.
fn must_not_drain(_, ctx: ScriptContext) -> Bool {
  let not_drained = ctx |> is_not_drained()
  not_drained?
}

/// Validate delay unlock: Must re-send the chest's lock with a later deadline as the new datum.
fn must_resend_chest_lock(
  _,
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Bool {
  let resent_chest_lock = ctx |> resend_chest_lock(policy_id, chest_lock)
  resent_chest_lock?
}

/// Validate unlock chest: Disallow unlock before deadline.
fn must_unlock_after_deadline(
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Bool {
  let unlock_after_deadline =
    ctx |> unlock_after_deadline(policy_id, chest_lock)
  unlock_after_deadline?
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
//           This section contains the functions to support the validation functions            //

fn minted_values_of(
  context ctx: ScriptContext,
  policy_id: PolicyId,
) -> List<(AssetName, Qty)> {
  ctx.transaction.mint
    |> from_minted_value()
    |> tokens(policy_id)
    |> dict.to_list()
}

/// (1) TxOuts must contain `ChestLock` with the specified Policy ID and Chest Name and its
///     `address.payment_credential` must be of type `ScriptCredential`, and contain the
///     deadline datum with a future value.
/// PS: It is, however, possible that the script address of the output (destination) is not
///     the address of the chest. Since there is no way of knowing the runtime value of the
///     chest address beforehand. But there is no incentive for the chest creator to do so.
fn send_chest_lock(
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Bool {
  let txn = ctx.transaction
  expect [(_, deadline)]: List<(ValidatorHash, PosixTime)> =
    txn.outputs |> filter_map(into_script_deadline(_, policy_id, chest_lock))
  (unsafe_unwrap.finite_start_of(txn.validity_range) < deadline)?
}

/// (1) Sum of input and output assets must be balance
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

/// A function to help filter_map transaction.Outputs by the existence of `ChestLock`
/// into (script_address, deadline)
fn into_script_deadline(
  output: transaction.Output,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Option<(ValidatorHash, PosixTime)> {
  let filter_chest_lock =
    fn(id, name, qty) {
      if id == policy_id && name == chest_lock && qty == 1 {
        Some((id, name, 1))
      } else {
        None
      }
    }
  when output.address.payment_credential is {
    VerificationKeyCredential(_) -> None
    ScriptCredential(script_address) ->
      when output.value |> flatten_with(filter_chest_lock) is {
        // there must be at max 1 Chest Lock:
        [_] ->
          when output.datum is {
            InlineDatum(datum) -> {
              expect deadline: PosixTime = datum
              Some((script_address, deadline))
            }
            // invalid datum or no datum:
            _ -> None
          }
        // does not contain the Chest Lock:
        _ -> None
      }
  }
}

/// (1) TxIns must contain `ChestLock` with the specified Policy ID and Chest Name, and contain the deadline datum.
/// (2) TxOuts must contain `ChestLock` with the same Policy ID and Chest Name, and contain the deadline datum with a newer value.
/// (3) `address.payment_credential` of (1) and (2) must be `ScriptCredential`, same value, and there's only 1 unique script address.
///     (unique input==output script address validation has been done during drain validation)
/// (4) TxRefIns must contain `ChestKey` and its `address.payment_credential` must exist in `extra_signatories`
fn resend_chest_lock(
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Bool {
  let txn = ctx.transaction
  //
  // Expecting `ChestLock` in a UTxO of a script_address with PosixTime datum:
  expect [(input_address, old_deadline)]: List<(ValidatorHash, PosixTime)> =
    txn.inputs
      |> filter_map(
           fn(input: transaction.Input) {
             input.output |> into_script_deadline(policy_id, chest_lock)
           },
         )
  expect [(output_address, new_deadline)]: List<(ValidatorHash, PosixTime)> =
    txn.outputs |> filter_map(into_script_deadline(_, policy_id, chest_lock))
  //
  // Continue validating only if previous steps are valid:
  if (input_address == output_address)? && (old_deadline < new_deadline)? {
    //
    // Anonymous fns to filter_map reference_inputs by chest_key into signed_by_chest_key_holder:
    let filter_chest_key =
      fn(id, name, qty) {
        if id == policy_id && name == chest_key {
          Some((id, name, qty))
        } else {
          None
        }
      }
    let into_signed_by_chest_key_holder =
      fn(input: transaction.Input) {
        let output = input.output
        when output.value |> flatten_with(filter_chest_key) is {
          //
          // value does not contain chest_key:
          [] -> None
          //
          // value contains chest_key:
          _ ->
            Some(
              txn.extra_signatories
                |> has(output.address.payment_credential |> unwrap_credential),
            )
        }
      }
    //
    // return:
    when
      txn.reference_inputs |> filter_map(into_signed_by_chest_key_holder)
    is {
      [] -> {
        let found_chest_key_in_ref_inputs = False
        found_chest_key_in_ref_inputs?
      }
      signed_by_all_chest_key_holder ->
        list.and(signed_by_all_chest_key_holder)?
    }
  } else {
    False
  }
}

fn unwrap_credential(credential: Credential) -> ByteArray {
  when credential is {
    VerificationKeyCredential(pub_key_hash) -> pub_key_hash
    ScriptCredential(validator_hash) -> validator_hash
  }
}

/// (1) Find a UTxO from ctx.transaction.inputs where its output.value has (policy_id, chest_lock, 1)
/// (2) That UTxO must have an InlineDatum(PosixTime) where unsafe_unwrap.finite_start_of(ctx.transaction.validity_range) > PosixTime
fn unlock_after_deadline(
  ctx: ScriptContext,
  policy_id: PolicyId,
  chest_lock: AssetName,
) -> Bool {
  let txn = ctx.transaction
  expect Some(input): Option<transaction.Input> =
    txn.inputs
      |> find(
           fn(input) {
             quantity_of(input.output.value, policy_id, chest_lock) == 1
           },
         )
  expect InlineDatum(datum): Datum = input.output.datum
  expect deadline: PosixTime = datum
  (unsafe_unwrap.finite_start_of(txn.validity_range) > deadline)?
}
// ///////////////////////////////////// UTILITY FUNCTIONS ///////////////////////////////////// //
//            This section contains support function(s) not specific to the contract             //

// /// Deprecated
// fn sum_ints(ints: List<Int>) -> Int {
//   ints |> reduce(0, builtin.add_integer)
// }
