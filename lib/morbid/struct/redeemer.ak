pub type ChestRedeemer {
  /// Only the chest creator is allowed to delay-unlock.
  DelayUnlock
  /// Anyone can unlock the chest when the deadline has passed.
  UnlockChest
}
