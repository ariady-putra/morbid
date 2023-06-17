use aiken/builtin.{add_integer}
use aiken/list.{find, map, reduce}
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
/// When delaying unlock of a UTxO, the transaction must be signed only
/// by the chest creator and the sum of input/output ADA must be balanced.
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
    (DelayUnlock, CreateChest(_, creator)) -> {
      //
      // transaction must be signed only by the creator
      let txn_signed_only_by_creator =
        fn() { context |> signed_only_by(creator) }
      //
      // input/output ADA must be balanced
      let txn_io_ada_balanced =
        fn() { context |> io_ada_balanced() }
      //
      // return:
      txn_signed_only_by_creator()? && txn_io_ada_balanced()?
    }
    //
    // When delaying unlock for a UTxO with the datum of type AddTreasure:
    (DelayUnlock, AddTreasure(ref_txn)) ->
      //
      // ref_txn must refer to a UTxO with the datum of type CreateChest
      when context |> find_ref_datum(ref_txn) is {
        //
        // referenced-datum found:
        Some(CreateChest(_, creator)) -> {
          //
          // transaction must be signed only by the creator
          let txn_signed_only_by_creator =
            fn() { context |> signed_only_by(creator) }
          //
          // input/output ADA must be balanced
          let txn_io_ada_balanced =
            fn() { context |> io_ada_balanced() }
          //
          // return:
          txn_signed_only_by_creator()? && txn_io_ada_balanced()?
        }
        //
        // referenced datum not found:
        _ -> False
      }
    //
    // When unlocking the chest for a UTxO with the datum of type CreateChest:
    (UnlockChest, CreateChest(deadline, _)) -> {
      //
      // anyone can unlock after deadline
      let txn_unlock_after_deadline =
        fn() { context.transaction.validity_range |> is_after(deadline) }
      //
      // return:
      txn_unlock_after_deadline()?
    }
    //
    // When unlocking the chest for a UTxO with the datum of type AddTreasure:
    (UnlockChest, AddTreasure(ref_txn)) ->
      //
      // ref_txn must refer to a UTxO with the datum of type CreateChest
      when context |> find_ref_datum(ref_txn) is {
        //
        // referenced-datum found:
        Some(CreateChest(deadline, _)) -> {
          //
          // anyone can unlock after deadline
          let txn_unlock_after_deadline =
            fn() { context.transaction.validity_range |> is_after(deadline) }
          //
          // return:
          txn_unlock_after_deadline()?
        }
        //
        // referenced datum not found:
        _ -> False
      }
    //
    // Any other actions:
    _ -> False
  }
}

fn signed_only_by(context ctx: ScriptContext, signer: PubKeyHash) -> Bool {
  when ctx.transaction.extra_signatories is {
    [signatory] -> signatory == signer
    _ -> False
  }
}

fn io_ada_balanced(context ctx: ScriptContext) -> Bool {
  let txn = ctx.transaction
  let input_ada =
    txn.inputs
      |> map(fn(input) { input.output.value |> lovelace_of })
      |> sum_ints()
  let output_ada =
    txn.outputs
      |> map(fn(output) { output.value |> lovelace_of })
      |> sum_ints()
  input_ada == output_ada
}

fn sum_ints(ints: List<Int>) -> Int {
  ints |> reduce(0, add_integer)
}

fn find_ref_datum(
  context ctx: ScriptContext,
  ref_txn: TxHash,
) -> Option<ChestDatum> {
  // find the referenced tx-input
  let tx_in: Option<transaction.Input> =
    ctx.transaction.inputs
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
