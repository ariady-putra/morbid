use cardano/assets.{AssetName}

/// Used as the token name. Attach metadata when minting
/// to keep details such as chest name and address.
///
/// Give access for delaying the chest unlocking deadline
/// to anyone by sending them the `ChestKey` token.
pub const chest_key: AssetName = "ChestKey"
