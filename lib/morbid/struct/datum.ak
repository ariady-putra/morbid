use aiken/time.{PosixTime}
use morbid/alias.{PubKeyHash, TxHash}

pub type ChestDatum {
  /// Anyone can create their own chests. When they `AddTreasure`, they must refer to this UTxO, otherwise it cannot be unlocked later.
  CreateChest {
    /// Determines when the chest can be unlocked.
    deadline: PosixTime,
    /// Determines who can `DelayUnlock`
    creator: PubKeyHash,
  }
  /// Anyone can add treasures.
  AddTreasure {
    /// Reference hash that points to a UTxO containing the datum of type `CreateChest`
    ref_txn: TxHash,
  }
}
