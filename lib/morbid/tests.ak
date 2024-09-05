//// ```
//// ┍━ morbid/tests ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//// │ PASS [mem: 2323978, cpu:  685174685] mint_chest
//// │ · with traces
//// │ | ScriptContext: 121([_ 121([_ [_ 121([_ 121([_ h'547848617368', 0]), 121([_ 121([_ 121([_ h'436865737443726561746F72']), 122([])]), {_ h'': {_ h'': 42000000 } }, 121([]), 122([])])])], [], [_ 121([_ 121([_ 122([_ h'436865737441646472657373']), 122([])]), {_ h'': {_ h'': 24000000 }, h'506F6C6963794944': {_ h'43686573744C6F636B': 1 } }, 123([_ 5]), 122([])])], 2000000, {_ h'506F6C6963794944': {_ h'43686573744B6579': 10, h'43686573744C6F636B': 1 } }, [], {}, 121([_ 121([_ 122([_ 1]), 122([])]), 121([_ 123([]), 122([])])]), [], {}, {}, h'', {}, [], 122([]), 122([])]), 121([]), 121([_ h'506F6C6963794944'])])
//// │ | Redeemer: 121([])
//// │ PASS [mem: 3892044, cpu: 1147079914] validate_delay_unlock
//// │ · with traces
//// │ | ScriptContext: 121([_ 121([_ [_ 121([_ 121([_ h'4368657374416464726573735F4F6C6443686573744C6F636B446561646C696E655F547848617368', 0]), 121([_ 121([_ 122([_ h'436865737441646472657373']), 122([])]), {_ h'': {_ h'': 24000000 }, h'506F6C6963794944': {_ h'43686573744C6F636B': 1 } }, 123([_ 5]), 122([])])])], [_ 121([_ 121([_ h'43686573744B6579486F6C6465725F547848617368', 0]), 121([_ 121([_ 121([_ h'43686573744B6579486F6C646572']), 122([])]), {_ h'': {_ h'': 42000000 }, h'506F6C6963794944': {_ h'43686573744B6579': 10 } }, 121([]), 122([])])])], [_ 121([_ 121([_ 122([_ h'436865737441646472657373']), 122([])]), {_ h'': {_ h'': 24000000 }, h'506F6C6963794944': {_ h'43686573744C6F636B': 1 } }, 123([_ 10]), 122([])])], 2000000, {}, [], {}, 121([_ 121([_ 122([_ 2]), 122([])]), 121([_ 123([]), 122([])])]), [_ h'43686573744B6579486F6C646572'], {}, {}, h'', {}, [], 122([]), 122([])]), 121([]), 122([_ 121([_ h'', 0]), 122([])])])
//// │ | MaybeDeadline: 121([])
//// │ PASS [mem: 4605772, cpu: 1358545301] invalidate_delay_unlock_with_imbalance_assets
//// │ · with traces
//// │ | ScriptContext: 121([_ 121([_ [_ 121([_ 121([_ h'4368657374416464726573735F4F6C6443686573744C6F636B446561646C696E655F547848617368', 0]), 121([_ 121([_ 122([_ h'436865737441646472657373']), 122([])]), {_ h'': {_ h'': 24000000 }, h'506F6C6963794944': {_ h'43686573744C6F636B': 1 }, h'547265617375726531': {_ h'4E4654': 1 }, h'547265617375726532': {_ h'546F6B656E': 100 } }, 123([_ 5]), 122([])])])], [_ 121([_ 121([_ h'43686573744B6579486F6C6465725F547848617368', 0]), 121([_ 121([_ 121([_ h'43686573744B6579486F6C646572']), 122([])]), {_ h'': {_ h'': 42000000 }, h'506F6C6963794944': {_ h'43686573744B6579': 10 } }, 121([]), 122([])])])], [_ 121([_ 121([_ 122([_ h'436865737441646472657373']), 122([])]), {_ h'': {_ h'': 24000000 }, h'506F6C6963794944': {_ h'43686573744C6F636B': 1 }, h'547265617375726531': {_ h'4E4654': 1 }, h'547265617375726532': {_ h'546F6B656E': 75 } }, 123([_ 10]), 122([])])], 2000000, {}, [], {}, 121([_ 121([_ 123([]), 122([])]), 121([_ 121([]), 122([])])]), [_ h'43686573744B6579486F6C646572'], {}, {}, h'', {}, [], 122([]), 122([])]), 121([]), 122([_ 121([_ h'', 0]), 122([])])])
//// │ | MaybeDeadline: 121([])
//// │ | all_values_has_value ? False
//// │ | not_drained ? False
//// │ PASS [mem: 2059586, cpu:  604424085] validate_unlock_chest
//// │ · with traces
//// │ | ScriptContext: 121([_ 121([_ [_ 121([_ 121([_ h'4368657374416464726573735F547848617368', 0]), 121([_ 121([_ 122([_ h'436865737441646472657373']), 122([])]), {_ h'': {_ h'': 24000000 }, h'506F6C6963794944': {_ h'43686573744C6F636B': 1 } }, 123([_ 10]), 122([])])])], [], [_ 121([_ 121([_ 121([_ h'4368657374556E6C6F636B6572']), 122([])]), {_ h'': {_ h'': 24000000 } }, 121([]), 122([])])], 2000000, {}, [], {}, 121([_ 121([_ 122([_ 20]), 122([])]), 121([_ 123([]), 122([])])]), [], {}, {}, h'', {}, [], 122([]), 122([])]), 121([]), 122([_ 121([_ h'', 0]), 122([])])])
//// │ | MaybeDeadline: 121([])
//// ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 4 tests | 4 passed | 0 failed
//// ```

use aiken/interval.{after}
use cardano/address.{from_script, from_verification_key}
use cardano/assets.{from_asset, merge}
use cardano/transaction.{InlineDatum, NoDatum, OutputReference}
use morbid/action.{DelayUnlock, UnlockChest}
use morbid/constant
use morbid/locker
use tx_util/builder.{
  add_asset_to_tx_output, add_signatory, add_tx_input, add_tx_output,
  add_tx_ref_input, build_txn_context, mint_assets, new_tx_input, new_tx_output,
  with_asset_of_tx_input,
}

test mint_chest() {
  // Arrange:
  let now = 1
  let deadline = 5
  let policy_id = "PolicyID"
  let chest_name = "ChestLock"
  let chest_lock = policy_id |> from_asset(chest_name, 1)
  let chest_key = policy_id |> from_asset(constant.chest_key, 10)
  let minted_assets = merge(chest_lock, chest_key)
  let tx_hash = "TxHash"
  let utxo_ref = OutputReference(tx_hash, 0)
  let tx_in =
    new_tx_input(
      tx_hash,
      from_verification_key("ChestCreator"),
      42_000_000,
      NoDatum,
    )
  let tx_out =
    new_tx_output(
      from_script("ChestAddress"),
      24_000_000,
      InlineDatum(deadline),
    )
      |> add_asset_to_tx_output(chest_lock)
  let tx =
    after(now)
      |> build_txn_context()
      |> mint_assets(policy_id, minted_assets)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)

  // Trace:
  trace @"ScriptContext": tx

  // Assert:
  locker.mint_chest(chest_name, utxo_ref, Void, tx)
}

test validate_delay_unlock() {
  // Arrange:
  let now = 2
  let deadline = 5
  let new_deadline = 10
  let policy_id = "PolicyID"
  let chest_name = "ChestLock"
  let chest_lock = policy_id |> from_asset(chest_name, 1)
  let chest_key = policy_id |> from_asset(constant.chest_key, 10)
  let tx_ref_in =
    new_tx_input(
      "ChestKeyHolder_TxHash",
      from_verification_key("ChestKeyHolder"),
      42_000_000,
      NoDatum,
    )
      |> with_asset_of_tx_input(chest_key)
  let tx_in =
    new_tx_input(
      "ChestAddress_OldChestLockDeadline_TxHash",
      from_script("ChestAddress"),
      24_000_000,
      InlineDatum(deadline),
    )
      |> with_asset_of_tx_input(chest_lock)
  let tx_out =
    new_tx_output(
      from_script("ChestAddress"),
      //   from_script("ChestAddress2"),
      24_000_000,
      //   0,
      InlineDatum(new_deadline),
    )
      |> add_asset_to_tx_output(chest_lock)
  let tx =
    after(now) // `interval.empty` is also allowed
      |> build_txn_context()
      |> add_tx_ref_input(tx_ref_in)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
      |> add_signatory("ChestKeyHolder")

  // Trace:
  trace @"ScriptContext": tx

  // Assert:
  locker.validate(chest_name, policy_id, Void, DelayUnlock, tx)
}

test invalidate_delay_unlock_with_imbalance_assets() {
  // Arrange:
  let deadline = 5
  let new_deadline = 10
  let policy_id = "PolicyID"
  let chest_name = "ChestLock"
  let chest_lock = policy_id |> from_asset(chest_name, 1)
  let chest_key = policy_id |> from_asset(constant.chest_key, 10)
  let treasure_1 = "Treasure1" |> from_asset("NFT", 1)
  let treasure_2 = "Treasure2" |> from_asset("Token", 100)
  let treasure_2_partial = "Treasure2" |> from_asset("Token", 75)
  let tx_ref_in =
    new_tx_input(
      "ChestKeyHolder_TxHash",
      from_verification_key("ChestKeyHolder"),
      42_000_000,
      NoDatum,
    )
      |> with_asset_of_tx_input(chest_key)
  let tx_in =
    new_tx_input(
      "ChestAddress_OldChestLockDeadline_TxHash",
      from_script("ChestAddress"),
      24_000_000,
      InlineDatum(deadline),
    )
      |> with_asset_of_tx_input(chest_lock)
      |> with_asset_of_tx_input(treasure_1)
      |> with_asset_of_tx_input(treasure_2)
  let tx_out =
    new_tx_output(
      from_script("ChestAddress"),
      //   from_script("ChestAddress2"),
      24_000_000,
      //   0,
      InlineDatum(new_deadline),
    )
      |> add_asset_to_tx_output(chest_lock)
      |> add_asset_to_tx_output(treasure_1) //   |> add_asset_to_tx_output(treasure_2)
      |> add_asset_to_tx_output(treasure_2_partial)
  let tx =
    build_txn_context(interval.empty)
      |> add_tx_ref_input(tx_ref_in)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
      |> add_signatory("ChestKeyHolder")

  // Trace:
  trace @"ScriptContext": tx

  // Assert:
  !locker.validate(chest_name, policy_id, Void, DelayUnlock, tx)
}

test validate_unlock_chest() {
  // Arrange:
  let now = 20
  let deadline = 10
  let policy_id = "PolicyID"
  let chest_name = "ChestLock"
  let chest_lock = policy_id |> from_asset(chest_name, 1)
  let tx_in =
    new_tx_input(
      "ChestAddress_TxHash",
      from_script("ChestAddress"),
      24_000_000,
      InlineDatum(deadline),
    )
      |> with_asset_of_tx_input(chest_lock)
  let tx_out =
    new_tx_output(from_verification_key("ChestUnlocker"), 24_000_000, NoDatum)
  let tx =
    after(now)
      |> build_txn_context()
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)

  // Trace:
  trace @"ScriptContext": tx

  // Assert:
  locker.validate(chest_name, policy_id, Void, UnlockChest, tx)
}
