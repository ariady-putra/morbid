use aiken/builtin.{add_integer}
use aiken/interval.{Finite}
use aiken/list.{filter, find, has, map, reduce}
use aiken/time.{PosixTime}
use aiken/transaction.{InlineDatum, ScriptContext, Spend}
use aiken/transaction/value.{lovelace_of}
use morbid/alias.{PubKeyHash, TxHash}
use morbid/chain.{and}
use morbid/struct/datum.{AddTreasure, ChestDatum, CreateChest}
use morbid/struct/redeemer.{ChestRedeemer, DelayUnlock, UnlockChest}
use time_util/bound.{start_bound_type_of}
use time_util/cheap/posixtime.{is_after}

/// Depending on the `ChestDatum`, either `CreateChest` or `AddTreasure`,
/// the validator may need to search for the referenced `TxHash`.
/// `ChestRedeemer` determines what action is to be done to the UTxO(s),
/// either `DelayUnlock` or `UnlockChest`.
/// When delaying unlock of a UTxO, the transaction must be signed by the
/// chest creator, the sum of input ADA must not be less than output ADA,
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
// Currently CHAIN FUNCTIONS only contains the validation helper functions for `DelayUnlock`, as //
// during `UnlockChest` it only needs to validate the `deadline` against the time of transaction //

/// `DelayUnlock`: Transaction must be signed by the creator.
fn must_be_signed_by_creator(
  _,
  context ctx: ScriptContext,
  creator: PubKeyHash,
) -> Bool {
  let signed_by_creator = ctx |> is_signed_by(creator)
  signed_by_creator?
}

/// `DelayUnlock`: Input ADA must not be less than Output.
fn must_not_drain(_, context ctx: ScriptContext) -> Bool {
  let not_drained = ctx |> is_not_drained()
  not_drained?
}

/// `DelayUnlock`: Must re-create the chest.
fn must_recreate_chest(_, context ctx: ScriptContext) -> Bool {
  let recreated_chest = ctx |> recreate_chest()
  recreated_chest?
}

/// `DelayUnlock`: Transaction must be signed by the new owner
/// when transferring the ownership of the chest.
fn must_be_signed_by_new_creator(
  _,
  context ctx: ScriptContext,
  new_creator: PubKeyHash,
) -> Bool {
  let signed_by_new_creator = ctx |> is_signed_by(new_creator)
  signed_by_new_creator?
}

/// `DelayUnlock`: Disallow setting the past as the new deadline.
fn must_postpone_after_now(
  _,
  context ctx: ScriptContext,
  new_deadline: PosixTime,
) -> Bool {
  let postponed_after_now =
    when start_bound_type_of(ctx.transaction.validity_range) is {
      Finite(now) -> (now < new_deadline)?
      // infinite bound type:
      _ -> False
    }
  postponed_after_now?
}

// ///////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////// //
//       This section contains the functions to support the validation and chain functions      //

fn is_signed_by(context ctx: ScriptContext, signer: PubKeyHash) -> Bool {
  ctx.transaction.extra_signatories |> has(signer)
}

fn is_not_drained(context ctx: ScriptContext) -> Bool {
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
