use aiken/hash.{Blake2b_224, Blake2b_256, Hash}
use aiken/transaction.{Transaction}
use aiken/transaction/credential.{VerificationKey}
use aiken/transaction/value.{AssetName, PolicyId}

pub type Qty =
  Int

pub type FlattenValue =
  (PolicyId, AssetName, Qty)

pub type FlattenValues =
  List<FlattenValue>

pub type PubKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type TxHash =
  Hash<Blake2b_256, Transaction>
