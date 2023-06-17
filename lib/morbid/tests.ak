//// ```
//// ┍━ morbid/tests ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//// │ PASS [mem: 1507546, cpu:  840504056] allow_delay_by_creator
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 1508779, cpu:  841072954] disallow_delay_by_others
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff9f51303768337227355075384b337948343568ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ signed_by == creator ? False
//// │ PASS [mem: 1050671, cpu:  627435355] disallow_delay_by_no_tx_signer
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2564916, cpu: 1404939075] allow_delay_by_referenced_datum
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd8799f40ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2566149, cpu: 1405507973] disallow_delay_by_incorrect_referenced_datum_tx_signer
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd8799f40ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff9f51303768337227355075384b337948343568ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ signed_by == creator ? False
//// │ PASS [mem: 2518463, cpu: 1383490244] disallow_delay_by_incorrect_referenced_datum_type
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd8799f40ffd87a80ffa0d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2496905, cpu: 1378730319] disallow_delay_by_incorrect_referenced_datum_tx_hash
//// │ ↳ Datum: "d87a9f513037683527355f5f37786e5f5f48343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd8799f40ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 1480511, cpu:  826122227] disallow_delay_by_no_referenced_datum
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 1116516, cpu:  658434375] allow_unlock_after_deadline
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 1104158, cpu:  653477435] disallow_unlock_before_deadline
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87980d87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 1141088, cpu:  673079016] disallow_unlock_during_deadline
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2173216, cpu: 1222540070] allow_unlock_by_referenced_datum
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd8799f40ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2091416, cpu: 1187171578] disallow_unlock_by_incorrect_referenced_datum_type
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd8799f40ffd87a80ffa0d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2069850, cpu: 1182407565] disallow_unlock_by_incorrect_referenced_datum_tx_hash
//// │ ↳ Datum: "d87a9f513037683527355f5f37786e5f5f48343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd8799f40ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2160858, cpu: 1217583130] disallow_unlock_by_referenced_datum_before_deadline
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd8799f40ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87980d87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2197860, cpu: 1237195231] disallow_unlock_by_referenced_datum_during_deadline
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd8799f40ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 16 tests | 16 passed | 0 failed
//// ```

use aiken/dict
use aiken/interval.{Interval, after, before, between, empty}
use aiken/list.{push}
use aiken/time.{PosixTime}
use aiken/transaction.{
  InlineDatum, Output, OutputReference, ScriptContext, Spend, Transaction,
  TransactionId,
}
use aiken/transaction/credential.{from_verification_key}
use aiken/transaction/value.{from_lovelace, to_minted_value, zero}
use morbid/alias.{PubKeyHash, TxHash}
use morbid/locker.{validate}
use morbid/struct/datum.{AddTreasure, CreateChest}
use morbid/struct/redeemer.{DelayUnlock, UnlockChest}
use string_util/cbor.{print}

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

fn new_tx_input(tx_hash: TxHash, datum: Data) -> transaction.Input {
  transaction.Input(
    OutputReference(TransactionId(tx_hash), 0),
    Output {
      address: from_verification_key(""),
      value: zero(),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  )
}

const deadline: PosixTime = 5

const chest_creator: PubKeyHash = "Ch357_Cr3470r_PKH"

const other_pkh: PubKeyHash = "07h3r'5Pu8K3yH45h"

const create_chest_tx_hash: TxHash = "Cr3473Ch3577xH45h"

const other_tx_hash: TxHash = "07h5'5__7xn__H45h"

test allow_delay_by_creator() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = DelayUnlock
  let context =
    empty()
      |> build_txn_context()
      |> add_signatory(chest_creator)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test disallow_delay_by_others() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = DelayUnlock
  let context =
    empty()
      |> build_txn_context()
      |> add_signatory(other_pkh)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_no_tx_signer() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = DelayUnlock
  let context =
    empty()
      |> build_txn_context()
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test allow_delay_by_referenced_datum() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = DelayUnlock
  let ref_txn = create_chest_tx_hash |> new_tx_input(ref_datum)
  let context =
    empty()
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test disallow_delay_by_incorrect_referenced_datum_tx_signer() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = DelayUnlock
  let ref_txn = create_chest_tx_hash |> new_tx_input(ref_datum)
  let context =
    empty()
      |> build_txn_context()
      |> add_signatory(other_pkh)
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_incorrect_referenced_datum_type() {
  //
  // arrange:
  let ref_datum = AddTreasure(create_chest_tx_hash)
  // ref_datum refers to own datum
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = DelayUnlock
  let ref_txn = create_chest_tx_hash |> new_tx_input(ref_datum)
  let context =
    empty()
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_incorrect_referenced_datum_tx_hash() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(other_tx_hash)
  // datum refers to another tx-hash
  let redeemer = DelayUnlock
  let ref_txn = create_chest_tx_hash |> new_tx_input(ref_datum)
  let context =
    empty()
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_no_referenced_datum() {
  //
  // arrange:
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = DelayUnlock
  let context =
    empty()
      |> build_txn_context()
      |> add_signatory(chest_creator)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test allow_unlock_after_deadline() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = UnlockChest
  let context =
    after(deadline + 1)
      |> build_txn_context()
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test disallow_unlock_before_deadline() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = UnlockChest
  let context =
    before(deadline + 1)
      |> build_txn_context()
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_unlock_during_deadline() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = UnlockChest
  let context =
    between(deadline, deadline + 1)
      |> build_txn_context()
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test allow_unlock_by_referenced_datum() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = UnlockChest
  let ref_txn = create_chest_tx_hash |> new_tx_input(ref_datum)
  let context =
    after(deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test disallow_unlock_by_incorrect_referenced_datum_type() {
  //
  // arrange:
  let ref_datum = AddTreasure(create_chest_tx_hash)
  // ref_datum refers to own datum
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = UnlockChest
  let ref_txn = create_chest_tx_hash |> new_tx_input(ref_datum)
  let context =
    after(deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_unlock_by_incorrect_referenced_datum_tx_hash() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(other_tx_hash)
  // datum refers to another tx-hash
  let redeemer = UnlockChest
  let ref_txn = create_chest_tx_hash |> new_tx_input(ref_datum)
  let context =
    after(deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_unlock_by_referenced_datum_before_deadline() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = UnlockChest
  let ref_txn = create_chest_tx_hash |> new_tx_input(ref_datum)
  let context =
    before(deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_unlock_by_referenced_datum_during_deadline() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = UnlockChest
  let ref_txn = create_chest_tx_hash |> new_tx_input(ref_datum)
  let context =
    between(deadline, deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}
