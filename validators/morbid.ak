//// # Davy Jones' Locker
////
//// A dead-man's switch contract where you can Create Chest
//// and the chest can only be unlocked after a period of time
//// of not being postponed. You can also Add Treasure to the
//// chest. Anyone can redeem the treasure when the deadline
//// has passed.
////
//// ```
//// author      :   Kiki Ariady Putra
//// maintainer  :   aurodeus@gmail.com
//// ```
////
//// Jakarta 2023
////
//// _(built using Aiken v1.0.10-alpha)_

use aiken/transaction.{ScriptContext}
use morbid/locker
use morbid/struct/datum.{ChestDatum}
use morbid/struct/redeemer.{ChestRedeemer}

validator {
  fn validate(
    datum: ChestDatum,
    redeemer: ChestRedeemer,
    context: ScriptContext,
  ) -> Bool {
    locker.validate(datum, redeemer, context)
  }
}
