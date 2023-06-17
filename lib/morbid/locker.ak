use aiken/builtin.{add_integer}
use aiken/list.{filter, find, has, map, reduce}
use aiken/time.{PosixTime}
use aiken/transaction.{InlineDatum, ScriptContext}
use aiken/transaction/value.{lovelace_of}
use morbid/alias.{PubKeyHash, TxHash}
use morbid/struct/datum.{AddTreasure, ChestDatum, CreateChest}
use morbid/struct/redeemer.{ChestRedeemer, DelayUnlock, UnlockChest}
use time_util/cheap/posixtime.{is_after}

/// Depending on the `ChestDatum`, either `CreateChest` or `AddTreasure`,
/// the validation may need to search for the referenced `TxHash`.
/// `ChestRedeemer` determines what action is to be done to the UTxO(s),
/// either `DelayUnlock` or `UnlockChest`.
/// When delaying unlock of a UTxO, the transaction must be signed by the
/// chest creator, the sum of input ADA must not be less than output ADA,
/// and it must re-create a `CreateChest` datum with a later deadline and
/// the same creator.
/// When unlocking the chest of a UTxO, the transaction validity range
/// must be after the deadline.
pub fn validate(
  datum: ChestDatum,
  redeemer: ChestRedeemer,
  context: ScriptContext,
) -> Bool {
  when (redeemer, datum) is {
    //
    // When delaying unlock for a UTxO with the datum of type CreateChest:
    (DelayUnlock, CreateChest(deadline, creator)) ->
      context |> validate_delay_unlock(deadline, creator)
    //
    // When delaying unlock for a UTxO with the datum of type AddTreasure:
    (DelayUnlock, AddTreasure(ref_txn)) ->
      //
      // ref_txn must refer to a UTxO with the datum of type CreateChest
      when context |> must_find_ref_datum(ref_txn) is {
        //
        // referenced-datum found:
        Some(CreateChest(deadline, creator)) ->
          context |> validate_delay_unlock(deadline, creator)
        //
        // referenced datum not found:
        _ -> False
      }
    //
    // When unlocking the chest for a UTxO with the datum of type CreateChest:
    (UnlockChest, CreateChest(deadline, _)) ->
      context |> validate_unlock_chest(deadline)
    //
    // When unlocking the chest for a UTxO with the datum of type AddTreasure:
    (UnlockChest, AddTreasure(ref_txn)) ->
      //
      // ref_txn must refer to a UTxO with the datum of type CreateChest
      when context |> must_find_ref_datum(ref_txn) is {
        //
        // referenced-datum found:
        Some(CreateChest(deadline, _)) ->
          context |> validate_unlock_chest(deadline)
        //
        // referenced datum not found:
        _ -> False
      }
    //
    // Any other actions:
    _ -> False
  }
}

fn validate_delay_unlock(
  context ctx: ScriptContext,
  deadline: PosixTime,
  creator: PubKeyHash,
) -> Bool {
  //
  // transaction must be signed by the creator
  let txn_signed_by_creator =
    fn() { ctx |> must_be_signed_by(creator) }
  //
  // input ADA must not be less than output
  let txn_must_not_drain_ada =
    fn() { ctx |> must_not_be_drained() }
  //
  // must re-create the chest
  let txn_must_recreate_chest =
    fn() { ctx |> must_recreate_chest(deadline, creator) }
  //
  // return:
  txn_signed_by_creator()? && txn_must_not_drain_ada()? && txn_must_recreate_chest()?
}

fn validate_unlock_chest(
  context ctx: ScriptContext,
  deadline: PosixTime,
) -> Bool {
  //
  // anyone can unlock after deadline
  let txn_unlock_after_deadline =
    fn() { ctx.transaction.validity_range |> is_after(deadline) }
  //
  // return:
  txn_unlock_after_deadline()?
}

fn must_be_signed_by(context ctx: ScriptContext, signer: PubKeyHash) -> Bool {
  ctx.transaction.extra_signatories |> has(signer)
}

fn must_not_be_drained(context ctx: ScriptContext) -> Bool {
  let txn = ctx.transaction
  let input_ada =
    txn.inputs
      |> map(fn(input) { lovelace_of(input.output.value) })
      |> sum_ints()
  let output_ada =
    txn.outputs
      |> map(fn(output) { lovelace_of(output.value) })
      |> sum_ints()
  //
  // it's okay to receive more inputs,
  // since output is deducted by fees:
  (input_ada >= output_ada)?
}

fn sum_ints(ints: List<Int>) -> Int {
  ints |> reduce(0, add_integer)
}

fn must_recreate_chest(
  context ctx: ScriptContext,
  deadline: PosixTime,
  creator: PubKeyHash,
) -> Bool {
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
          (new_creator == creator)? && (new_deadline > deadline)?
        }
        // couldn't destructure datum:
        _ -> False
      }
    // invalid number of datum(s)
    _ -> False
  }
}

fn must_find_ref_datum(
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
