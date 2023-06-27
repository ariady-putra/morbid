pub type Action {
  /// Show `ChestKey` to delay-unlock.
  DelayUnlock
  /// Anyone can unlock the chest when the deadline has passed.
  UnlockChest
}
