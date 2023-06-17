use aiken/list.{find}
use aiken/transaction.{InlineDatum, ScriptContext}
use morbid/alias.{PubKeyHash, TxHash}
use morbid/struct/datum.{AddTreasure, ChestDatum, CreateChest}
use morbid/struct/redeemer.{ChestRedeemer, DelayUnlock, UnlockChest}
use time_util/cheap/posixtime.{is_after}

/// Depending on the `ChestDatum`, either `CreateChest` or `AddTreasure`,
/// the validation may need to search for the referenced `TxHash`.
/// `ChestRedeemer` determines what action is to be done to the UTxO(s),
/// either `DelayUnlock` or `UnlockChest`.
/// When delaying unlock of a UTxO, the transaction must be signed only
/// by the chest creator.
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
    (DelayUnlock, CreateChest(_, creator)) ->
      // transaction must be signed only by the creator
      context |> signed_only_by(creator)
    //
    // When delaying unlock for a UTxO with the datum of type AddTreasure:
    (DelayUnlock, AddTreasure(ref_txn)) ->
      // ref_txn must refer to a UTxO with the datum of type CreateChest
      when context |> find_ref_datum(ref_txn) is {
        Some(CreateChest(_, creator)) ->
          // transaction must be signed only by the creator
          context |> signed_only_by(creator)
        _ -> False
      }
    //
    // When unlocking the chest for a UTxO with the datum of type CreateChest:
    (UnlockChest, CreateChest(deadline, _)) ->
      // anyone can unlock after deadline
      context.transaction.validity_range |> is_after(deadline)
    //
    // When unlocking the chest for a UTxO with the datum of type AddTreasure:
    (UnlockChest, AddTreasure(ref_txn)) ->
      // ref_txn must refer to a UTxO with the datum of type CreateChest
      when context |> find_ref_datum(ref_txn) is {
        Some(CreateChest(deadline, _)) ->
          // anyone can unlock after deadline
          context.transaction.validity_range |> is_after(deadline)
        _ -> False
      }
    //
    // Any other actions:
    _ -> False
  }
}

fn signed_only_by(context: ScriptContext, creator: PubKeyHash) -> Bool {
  when context.transaction.extra_signatories is {
    [signed_by] -> (signed_by == creator)?
    _ -> False
  }
}

fn find_ref_datum(context: ScriptContext, ref_txn: TxHash) -> Option<ChestDatum> {
  // find the referenced tx-input
  let tx_in: Option<transaction.Input> =
    context.transaction.inputs
      |> find(fn(tx_in) { tx_in.output_reference.transaction_id.hash == ref_txn })
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
