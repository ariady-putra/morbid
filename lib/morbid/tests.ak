use aiken/dict
use aiken/interval.{Interval, after}
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

pub fn build_txn_context(validity_range: Interval<PosixTime>) -> ScriptContext {
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

pub fn mint_assets(
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

pub fn add_signatory(
  context: ScriptContext,
  signatory: PubKeyHash,
) -> ScriptContext {
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

pub fn add_tx_input(
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

pub fn new_tx_input(
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

pub fn add_tx_output(
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

pub fn new_tx_output(
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

pub fn add_asset_to_tx_output(
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
