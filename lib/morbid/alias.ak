use aiken/crypto.{Blake2b_224, Blake2b_256, Hash, Script, VerificationKey}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{Transaction}

pub type Qty =
  Int

pub type FlattenValue =
  (PolicyId, AssetName, Qty)

pub type FlattenValues =
  List<FlattenValue>

pub type ValidatorHash =
  Hash<Blake2b_224, Script>

pub type PubKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type TxHash =
  Hash<Blake2b_256, Transaction>
