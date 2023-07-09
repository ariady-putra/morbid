//// ```
//// ┍━ morbid/tests ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//// │ PASS [mem: 3502305, cpu: 1863748540] mint_chest
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f46547848617368ff00ffd8799fd8799fd8799f4c436865737443726561746f72ffd87a80ffa140a1401a0280de80d87980d87a80ffffff809fd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360048506f6c6963794944a14943686573744c6f636b01d87b9f05ffd87a80ffffa140a1401a001e8480a148506f6c6963794944a24843686573744b65790a4943686573744c6f636b0180a0d8799fd8799fd87a9f01ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd8799f48506f6c6963794944ffff"
//// │ ↳ Redeemer: "d87980"
//// │ PASS [mem: 5640967, cpu: 2958891359] validate_delay_unlock
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f58284368657374416464726573735f4f6c6443686573744c6f636b446561646c696e655f547848617368ff00ffd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360048506f6c6963794944a14943686573744c6f636b01d87b9f05ffd87a80ffffff9fd8799fd8799fd8799f5543686573744b6579486f6c6465725f547848617368ff00ffd8799fd8799fd8799f4e43686573744b6579486f6c646572ffd87a80ffa240a1401a0280de8048506f6c6963794944a14843686573744b65790ad87980d87a80ffffff9fd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360048506f6c6963794944a14943686573744c6f636b01d87b9f0affd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f02ffd87a80ffd8799fd87b80d87a80ffff9f4e43686573744b6579486f6c646572ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ MaybeDeadline: "d87980"
//// │ PASS [mem: 6445812, cpu: 3428394224] invalidate_delay_unlock_with_imbalance_assets
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f58284368657374416464726573735f4f6c6443686573744c6f636b446561646c696e655f547848617368ff00ffd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa440a1401a016e360048506f6c6963794944a14943686573744c6f636b0149547265617375726531a1434e46540149547265617375726532a145546f6b656e1864d87b9f05ffd87a80ffffff9fd8799fd8799fd8799f5543686573744b6579486f6c6465725f547848617368ff00ffd8799fd8799fd8799f4e43686573744b6579486f6c646572ffd87a80ffa240a1401a0280de8048506f6c6963794944a14843686573744b65790ad87980d87a80ffffff9fd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa440a1401a016e360048506f6c6963794944a14943686573744c6f636b0149547265617375726531a1434e46540149547265617375726532a145546f6b656e184bd87b9f0affd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff9f4e43686573744b6579486f6c646572ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ MaybeDeadline: "d87980"
//// │ ↳ all_values_has_value ? False
//// │ ↳ not_drained ? False
//// │ PASS [mem: 2954740, cpu: 1618306575] validate_unlock_chest
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f534368657374416464726573735f547848617368ff00ffd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360048506f6c6963794944a14943686573744c6f636b01d87b9f0affd87a80ffffff809fd8799fd8799fd8799f4d4368657374556e6c6f636b6572ffd87a80ffa140a1401a016e3600d87980d87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f14ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ MaybeDeadline: "d87980"
//// ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 4 tests | 4 passed | 0 failed
//// ```

use aiken/dict
use aiken/interval.{Interval, after, empty}
use aiken/list.{push}
use aiken/time.{PosixTime}
use aiken/transaction.{
  Datum, InlineDatum, Mint, NoDatum, Output, OutputReference, ScriptContext,
  Spend, Transaction, TransactionId,
}
use aiken/transaction/credential.{Address, from_script, from_verification_key}
use aiken/transaction/value.{
  MintedValue, PolicyId, Value, from_asset, from_lovelace, merge,
  to_minted_value, zero,
}
use morbid/action.{DelayUnlock, UnlockChest}
use morbid/alias.{PubKeyHash, TxHash}
use morbid/constant
use morbid/locker
use string_util/cbor.{print}

test mint_chest() {
  //
  // Arrange:
  let now = 1
  let deadline = 5
  let policy_id = "PolicyID"
  let chest_name = "ChestLock"
  let chest_lock = policy_id |> from_asset(chest_name, 1)
  let chest_key = policy_id |> from_asset(constant.chest_key, 10)
  let minted_assets = merge(chest_lock, chest_key) |> to_minted_value()
  let tx_hash = "TxHash"
  let utxo_ref = OutputReference(TransactionId(tx_hash), 0)
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
  //
  // Trace:
  trace "ScriptContext"
    |> print(tx)
  //
  // Assert:
  locker.mint_chest(chest_name, utxo_ref, Void, tx)
}

test validate_delay_unlock() {
  //
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
    after(now) // empty() is also allowed
      |> build_txn_context()
      |> add_tx_ref_input(tx_ref_in)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
      |> add_signatory("ChestKeyHolder")
  //
  // Trace:
  trace "ScriptContext"
    |> print(tx)
  //
  // Assert:
  locker.validate(chest_name, policy_id, Void, DelayUnlock, tx)
}

test invalidate_delay_unlock_with_imbalance_assets() {
  //
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
    empty()
      |> build_txn_context()
      |> add_tx_ref_input(tx_ref_in)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
      |> add_signatory("ChestKeyHolder")
  //
  // Trace:
  trace "ScriptContext"
    |> print(tx)
  //
  // Assert:
  !locker.validate(chest_name, policy_id, Void, DelayUnlock, tx)
}

test validate_unlock_chest() {
  //
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
  //
  // Trace:
  trace "ScriptContext"
    |> print(tx)
  //
  // Assert:
  locker.validate(chest_name, policy_id, Void, UnlockChest, tx)
}

fn build_txn_context(validity_range: Interval<PosixTime>) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: [],
      reference_inputs: [],
      outputs: [],
      fee: from_lovelace(2_000_000),
      mint: zero() |> to_minted_value(),
      certificates: [],
      withdrawals: dict.new(),
      validity_range,
      extra_signatories: [],
      redeemers: dict.new(),
      datums: dict.new(),
      id: TransactionId(""),
    },
    Spend(
      OutputReference { transaction_id: TransactionId(""), output_index: 0 },
    ),
  )
}

fn mint_assets(
  context: ScriptContext,
  policy_id: PolicyId,
  assets: MintedValue,
) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: context.transaction.inputs,
      reference_inputs: context.transaction.reference_inputs,
      outputs: context.transaction.outputs,
      fee: context.transaction.fee,
      mint: assets,
      certificates: context.transaction.certificates,
      withdrawals: context.transaction.withdrawals,
      validity_range: context.transaction.validity_range,
      extra_signatories: context.transaction.extra_signatories,
      redeemers: context.transaction.redeemers,
      datums: context.transaction.datums,
      id: context.transaction.id,
    },
    Mint(policy_id),
  )
}

fn add_signatory(context: ScriptContext, signatory: PubKeyHash) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: context.transaction.inputs,
      reference_inputs: context.transaction.reference_inputs,
      outputs: context.transaction.outputs,
      fee: context.transaction.fee,
      mint: context.transaction.mint,
      certificates: context.transaction.certificates,
      withdrawals: context.transaction.withdrawals,
      validity_range: context.transaction.validity_range,
      extra_signatories: context.transaction.extra_signatories
        |> push(signatory),
      redeemers: context.transaction.redeemers,
      datums: context.transaction.datums,
      id: context.transaction.id,
    },
    context.purpose,
  )
}

fn add_tx_ref_input(
  context: ScriptContext,
  tx_ref_in: transaction.Input,
) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: context.transaction.inputs,
      reference_inputs: context.transaction.reference_inputs
        |> push(tx_ref_in),
      outputs: context.transaction.outputs,
      fee: context.transaction.fee,
      mint: context.transaction.mint,
      certificates: context.transaction.certificates,
      withdrawals: context.transaction.withdrawals,
      validity_range: context.transaction.validity_range,
      extra_signatories: context.transaction.extra_signatories,
      redeemers: context.transaction.redeemers,
      datums: context.transaction.datums,
      id: context.transaction.id,
    },
    context.purpose,
  )
}

fn add_tx_input(
  context: ScriptContext,
  tx_in: transaction.Input,
) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: context.transaction.inputs
        |> push(tx_in),
      reference_inputs: context.transaction.reference_inputs,
      outputs: context.transaction.outputs,
      fee: context.transaction.fee,
      mint: context.transaction.mint,
      certificates: context.transaction.certificates,
      withdrawals: context.transaction.withdrawals,
      validity_range: context.transaction.validity_range,
      extra_signatories: context.transaction.extra_signatories,
      redeemers: context.transaction.redeemers,
      datums: context.transaction.datums,
      id: context.transaction.id,
    },
    context.purpose,
  )
}

fn new_tx_input(
  tx_hash: TxHash,
  address: Address,
  lovelace: Int,
  datum: Datum,
) -> transaction.Input {
  transaction.Input(
    OutputReference(TransactionId(tx_hash), 0),
    Output {
      address,
      value: lovelace |> from_lovelace,
      datum,
      reference_script: None,
    },
  )
}

fn with_asset_of_tx_input(
  input: transaction.Input,
  asset: Value,
) -> transaction.Input {
  transaction.Input(
    input.output_reference,
    Output {
      address: input.output.address,
      value: input.output.value
        |> merge(asset),
      datum: input.output.datum,
      reference_script: input.output.reference_script,
    },
  )
}

fn add_tx_output(
  context: ScriptContext,
  tx_out: transaction.Output,
) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: context.transaction.inputs,
      reference_inputs: context.transaction.reference_inputs,
      outputs: context.transaction.outputs
        |> push(tx_out),
      fee: context.transaction.fee,
      mint: context.transaction.mint,
      certificates: context.transaction.certificates,
      withdrawals: context.transaction.withdrawals,
      validity_range: context.transaction.validity_range,
      extra_signatories: context.transaction.extra_signatories,
      redeemers: context.transaction.redeemers,
      datums: context.transaction.datums,
      id: context.transaction.id,
    },
    context.purpose,
  )
}

fn new_tx_output(
  address: Address,
  lovelace: Int,
  datum: Datum,
) -> transaction.Output {
  transaction.Output {
    address,
    value: lovelace |> from_lovelace,
    datum,
    reference_script: None,
  }
}

fn add_asset_to_tx_output(
  output: transaction.Output,
  asset: Value,
) -> transaction.Output {
  transaction.Output {
    address: output.address,
    value: output.value
      |> merge(asset),
    datum: output.datum,
    reference_script: output.reference_script,
  }
}
