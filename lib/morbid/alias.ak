use aiken/hash.{Blake2b_224, Blake2b_256, Hash}
use aiken/transaction.{Transaction}
use aiken/transaction/credential.{Script, VerificationKey}

pub type ValidatorHash =
  Hash<Blake2b_224, Script>

pub type PubKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type TxHash =
  Hash<Blake2b_256, Transaction>
