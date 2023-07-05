pub type Action {
  /// Delay chest unlocking deadline by showing the `ChestKey` token(s)
  DelayUnlock
  /// Anyone can unlock the chest when the deadline has passed.
  UnlockChest
}
