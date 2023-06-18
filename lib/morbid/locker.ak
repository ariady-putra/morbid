use aiken/builtin.{add_integer}
use aiken/interval.{Finite}
use aiken/list.{filter, find, has, map, reduce}
use aiken/time.{PosixTime}
use aiken/transaction.{InlineDatum, ScriptContext}
use aiken/transaction/value.{lovelace_of}
use morbid/alias.{PubKeyHash, TxHash}
use morbid/chain.{and, break, continue, finish, start}
use morbid/struct/datum.{AddTreasure, ChestDatum, CreateChest}
use morbid/struct/redeemer.{ChestRedeemer, DelayUnlock, UnlockChest}
use time_util/cheap/posixtime.{is_after, start_of}

/// Depending on the `ChestDatum`, either `CreateChest` or `AddTreasure`,
/// the validator may need to search for the referenced `TxHash`.
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
    (DelayUnlock, CreateChest(_, creator)) ->
      context |> validate_delay_unlock(creator)
    //
    // When delaying unlock for a UTxO with the datum of type AddTreasure:
    (DelayUnlock, AddTreasure(ref_txn)) ->
      //
      // ref_txn must refer to a UTxO with the datum of type CreateChest
      when context |> must_find_ref_datum(ref_txn) is {
        //
        // referenced-datum found:
        Some(CreateChest(_, creator)) ->
          context |> validate_delay_unlock(creator)
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
  creator: PubKeyHash,
) -> Bool {
  //
  // transaction must be signed by the creator
  let must_be_signed_by_creator =
    fn(_) {
      let signed_by_creator = ctx |> must_be_signed_by(creator)
      if signed_by_creator? {
        continue()
      } else {
        break()
      }
    }
  //
  // input ADA must not be less than output
  let must_not_drain =
    fn(_) {
      let not_drained = ctx |> must_not_be_drained()
      if not_drained? {
        continue()
      } else {
        break()
      }
    }
  //
  // must re-create the chest
  let must_recreate_chest =
    fn(_) {
      let recreated_chest = ctx |> must_recreate_chest()
      if recreated_chest? {
        continue()
      } else {
        break()
      }
    }
  //
  // return:
  start()
    |> and(must_be_signed_by_creator(_))
    |> and(must_not_drain(_))
    |> and(must_recreate_chest(_))
    |> finish()
}

fn validate_unlock_chest(
  context ctx: ScriptContext,
  deadline: PosixTime,
) -> Bool {
  //
  // anyone can unlock after deadline
  ctx.transaction.validity_range |> is_after(deadline)
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

fn must_recreate_chest(context ctx: ScriptContext) -> Bool {
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
          let must_be_signed_by_new_creator =
            fn(_) {
              let signed_by_new_creator = ctx |> must_be_signed_by(new_creator)
              if signed_by_new_creator? {
                continue()
              } else {
                break()
              }
            }
          let must_postpone_after_now =
            fn(_) {
              let postponed_after_now = {
                let now = start_of(ctx.transaction.validity_range)
                when now.bound_type is {
                  Finite(time) -> {
                    let time =
                      if now.is_inclusive? {
                        time
                      } else {
                        time + 1
                      }
                    (time < new_deadline)?
                  }
                  // infinite bound type:
                  _ -> False
                }
              }
              if postponed_after_now? {
                continue()
              } else {
                break()
              }
            }
          start()
            |> and(must_be_signed_by_new_creator(_))
            |> and(must_postpone_after_now(_))
            |> finish()
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
