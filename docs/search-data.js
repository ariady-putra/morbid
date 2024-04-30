window.Aiken.initSearch([{"doc":"morbid/alias","title":"FlattenValue","content":"FlattenValue = (PolicyId, AssetName, Qty)\n\n","url":"morbid/alias.html#FlattenValue"},{"doc":"morbid/alias","title":"FlattenValues","content":"FlattenValues = List&lt;FlattenValue&gt;\n\n","url":"morbid/alias.html#FlattenValues"},{"doc":"morbid/alias","title":"PubKeyHash","content":"PubKeyHash = Hash&lt;Blake2b_224, VerificationKey&gt;\n\n","url":"morbid/alias.html#PubKeyHash"},{"doc":"morbid/alias","title":"Qty","content":"Qty = Int\n\n","url":"morbid/alias.html#Qty"},{"doc":"morbid/alias","title":"TxHash","content":"TxHash = Hash&lt;Blake2b_256, Transaction&gt;\n\n","url":"morbid/alias.html#TxHash"},{"doc":"morbid/alias","title":"ValidatorHash","content":"ValidatorHash = Hash&lt;Blake2b_224, Script&gt;\n\n","url":"morbid/alias.html#ValidatorHash"},{"doc":"morbid/alias","title":"morbid/alias","content":"","url":"morbid/alias.html"},{"doc":"morbid/constant","title":"chest_key","content":"chest_key: ByteArray = &quot;ChestKey&quot;\n Used as the token name. Attach metadata when minting\n to keep details such as chest name and address.\n\n Give access for delaying the chest unlocking deadline\n to anyone by sending them the `ChestKey` token.","url":"morbid/constant.html#chest_key"},{"doc":"morbid/constant","title":"morbid/constant","content":"","url":"morbid/constant.html"},{"doc":"morbid/action","title":"Action","content":"Action {\n  DelayUnlock\n  UnlockChest\n}\n\nDelayUnlock\n Delay chest unlocking deadline by showing the `ChestKey` token(s)\nUnlockChest\n Anyone can unlock the chest when the deadline has passed.","url":"morbid/action.html#Action"},{"doc":"morbid/action","title":"morbid/action","content":"","url":"morbid/action.html"},{"doc":"morbid/locker","title":"mint_chest","content":"mint_chest(\n  chest_name: AssetName,\n  utxo_ref: OutputReference,\n  redeemer: Void,\n  context: ScriptContext,\n) -&gt; Bool\n","url":"morbid/locker.html#mint_chest"},{"doc":"morbid/locker","title":"validate","content":"validate(\n  chest_name: AssetName,\n  policy_id: PolicyId,\n  datum: Data,\n  redeemer: Action,\n  context: ScriptContext,\n) -&gt; Bool\n When delaying unlock of a UTxO, the transaction must include any `ChestKey`\n token(s) signed by the holder(s) (it will be used to be matched against the\n `ChestLock`), the sum of input and output assets must be balance,\n and it must re-send the `ChestLock` NFT with a new deadline as the datum.\n When unlocking the chest of a UTxO, the transaction validity range must be\n after the deadline.","url":"morbid/locker.html#validate"},{"doc":"morbid/locker","title":"morbid/locker","content":"","url":"morbid/locker.html"}]);