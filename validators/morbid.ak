//// # Davy Jones' Locker
////
//// A dead-man's switch contract where you can Create Chest
//// and the chest can only be unlocked after a period of time
//// of not being postponed. You can also Add Treasure to the
//// chest. Anyone can redeem the treasure when the deadline
//// has passed.
////
//// `v0.2.1`: Accounts for NFT and tokens as well,
//// not just ADA.
////
//// `v0.2.0`: Now uses `ChestLock` and `ChestKey` mechanism
//// to validate actions.
////
//// ```
//// author      :   Kiki Ariady Putra
//// maintainer  :   aurodeus@gmail.com
//// ```
////
//// Jakarta 2024
////
//// _(re-compiled using `aiken v1.1.0`)_

use cardano/assets.{AssetName, PolicyId}
use cardano/script_context.{Minting, ScriptContext, Spending}
use cardano/transaction.{OutputReference, Transaction}
use morbid/action.{Action}
use morbid/locker

validator mint_chest(utxo_ref: OutputReference, chest_name: AssetName) {
  mint(redeemer: Void, policy_id: PolicyId, transaction: Transaction) {
    let info = Minting(policy_id)
    let context = ScriptContext(transaction, redeemer, info)

    chest_name |> locker.mint_chest(utxo_ref, redeemer, context)
  }

  else(_) {
    fail
  }
}

validator validate(policy_id: PolicyId, chest_name: AssetName) {
  spend(
    datum: Option<Data>,
    redeemer: Action,
    output_reference: OutputReference,
    transaction: Transaction,
  ) {
    expect Some(data) = datum

    let info = Spending(output_reference, datum)
    let context = ScriptContext(transaction, redeemer, info)

    chest_name |> locker.validate(policy_id, data, redeemer, context)
  }

  else(_) {
    fail
  }
}
