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
//// Jakarta 2023
////
//// _(re-compiled using [c169596c76](https://github.com/aiken-lang/aiken/tree/c169596c7652d6e2c5d5e918f6a11bbbfcbed1da))_

use aiken/transaction.{OutputReference, ScriptContext}
use aiken/transaction/value.{AssetName, PolicyId}
use morbid/action.{Action}
use morbid/locker

validator(utxo_ref: OutputReference, chest_name: AssetName) {
  fn mint_chest(redeemer: Void, context: ScriptContext) {
    chest_name |> locker.mint_chest(utxo_ref, redeemer, context)
  }
}

validator(policy_id: PolicyId, chest_name: AssetName) {
  fn validate(datum: Data, redeemer: Action, context: ScriptContext) -> Bool {
    chest_name |> locker.validate(policy_id, datum, redeemer, context)
  }
}
