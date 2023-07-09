window.Aiken.initSearch([{"doc":"morbid/chain","title":"and","content":"and(continue: Bool, next: fn(Void) -&gt; Bool) -&gt; Bool\n","url":"morbid/chain.html#and"},{"doc":"morbid/chain","title":"validate","content":"validate() -&gt; Bool\n","url":"morbid/chain.html#validate"},{"doc":"morbid/chain","title":"morbid/chain","content":" # Lazy Short-circuit Validations\n\n Chain together many validations that may fail.\n\n For example,\n ```\n validator {\n   fn validate(datum: Data, redeemer: Data, context: ScriptContext) -&gt; Bool {\n     chain.validate()\n       |&gt; and(must_be_condition_1(_, context, datum))\n       |&gt; and(must_be_condition_2(_, context, redeemer))\n       |&gt; and(must_be_condition_3(_, context))\n   }\n }\n ```\n Where,\n ```\n fn must_be_condition_1(_, context, datum) {\n   let condition_1 = context |&gt; must_validate_datum(datum) // call another fn somewhere\n   condition_1? // useful for tracing\n }\n \n fn must_be_condition_2(_, context, redeemer) {\n   let condition_2 = context |&gt; must_validate_redeemer(redeemer) // call another fn somewhere\n   condition_2? // useful for tracing\n }\n \n fn must_be_condition_3(_, context) {\n   let condition_3 = context |&gt; must_validate_other_things() // call another fn somewhere\n   condition_3? // useful for tracing\n }\n ```\n As of `Aiken v1.0.10-alpha` it&#39;s not possible to directly chain `|&gt;` with `?` and `&amp;&amp;` operators,\n for example:\n ```\n (context |&gt; must_be_condition_1())? &amp;&amp; (context |&gt; must_be_condition_2())? &amp;&amp; (context |&gt; must_be_condition_3())?\n ```\n Calling `|&gt;` indirectly by declaring anonymous functions could give weird non-short-circuit behavior.\n Using `list.and([fn(..) {..}])` or `reduce` or `foldr` and `foldl` loses the lazyness behavior for some reason.\n Hence this solution; internally it mimics the `option.and_then` function.","url":"morbid/chain.html"},{"doc":"morbid/tests","title":"morbid/tests","content":" ```\n ┍━ morbid/tests ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n │ PASS [mem: 3502305, cpu: 1863748540] mint_chest\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f46547848617368ff00ffd8799fd8799fd8799f4c436865737443726561746f72ffd87a80ffa140a1401a0280de80d87980d87a80ffffff809fd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360048506f6c6963794944a14943686573744c6f636b01d87b9f05ffd87a80ffffa140a1401a001e8480a148506f6c6963794944a24843686573744b65790a4943686573744c6f636b0180a0d8799fd8799fd87a9f01ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd8799f48506f6c6963794944ffff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ PASS [mem: 5640967, cpu: 2958891359] validate_delay_unlock\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f58284368657374416464726573735f4f6c6443686573744c6f636b446561646c696e655f547848617368ff00ffd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360048506f6c6963794944a14943686573744c6f636b01d87b9f05ffd87a80ffffff9fd8799fd8799fd8799f5543686573744b6579486f6c6465725f547848617368ff00ffd8799fd8799fd8799f4e43686573744b6579486f6c646572ffd87a80ffa240a1401a0280de8048506f6c6963794944a14843686573744b65790ad87980d87a80ffffff9fd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360048506f6c6963794944a14943686573744c6f636b01d87b9f0affd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f02ffd87a80ffd8799fd87b80d87a80ffff9f4e43686573744b6579486f6c646572ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ MaybeDeadline: &quot;d87980&quot;\n │ PASS [mem: 8955072, cpu: 4783784852] invalidate_delay_unlock_with_imbalance_assets\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f58234368657374416464726573735f4f6c644164645472656173757265325f547848617368ff00ffd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360049547265617375726532a145546f6b656e1864d87b9fd87980ffd87a80ffffd8799fd8799fd8799f58234368657374416464726573735f4f6c644164645472656173757265315f547848617368ff00ffd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a0280de8049547265617375726531a1434e465401d87b9fd87980ffd87a80ffffd8799fd8799fd8799f58284368657374416464726573735f4f6c6443686573744c6f636b446561646c696e655f547848617368ff00ffd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360048506f6c6963794944a14943686573744c6f636b01d87b9f05ffd87a80ffffff9fd8799fd8799fd8799f5543686573744b6579486f6c6465725f547848617368ff00ffd8799fd8799fd8799f4e43686573744b6579486f6c646572ffd87a80ffa240a1401a0280de8048506f6c6963794944a14843686573744b65790ad87980d87a80ffffff9fd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa440a1401a055d4a8048506f6c6963794944a14943686573744c6f636b0149547265617375726531a1434e46540149547265617375726532a145546f6b656e184bd87b9f0affd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87b80d87a80ffd8799fd87980d87a80ffff9f4e43686573744b6579486f6c646572ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ MaybeDeadline: &quot;d87980&quot;\n │ ↳ all_values_has_value ? False\n │ ↳ not_drained ? False\n │ PASS [mem: 2954740, cpu: 1618306575] validate_unlock_chest\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f534368657374416464726573735f547848617368ff00ffd8799fd8799fd87a9f4c436865737441646472657373ffd87a80ffa240a1401a016e360048506f6c6963794944a14943686573744c6f636b01d87b9f0affd87a80ffffff809fd8799fd8799fd8799f4d4368657374556e6c6f636b6572ffd87a80ffa140a1401a016e3600d87980d87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f14ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ MaybeDeadline: &quot;d87980&quot;\n ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 4 tests | 4 passed | 0 failed\n ```","url":"morbid/tests.html"},{"doc":"morbid/action","title":"Action","content":"Action {\n  DelayUnlock\n  UnlockChest\n}\n\nDelayUnlock\n Delay chest unlocking deadline by showing the `ChestKey` token(s)\nUnlockChest\n Anyone can unlock the chest when the deadline has passed.","url":"morbid/action.html#Action"},{"doc":"morbid/action","title":"morbid/action","content":"","url":"morbid/action.html"},{"doc":"morbid/alias","title":"FlattenValue","content":"FlattenValue = (PolicyId, AssetName, Qty)\n\n","url":"morbid/alias.html#FlattenValue"},{"doc":"morbid/alias","title":"FlattenValues","content":"FlattenValues = List&lt;FlattenValue&gt;\n\n","url":"morbid/alias.html#FlattenValues"},{"doc":"morbid/alias","title":"PubKeyHash","content":"PubKeyHash = Hash&lt;Blake2b_224, VerificationKey&gt;\n\n","url":"morbid/alias.html#PubKeyHash"},{"doc":"morbid/alias","title":"Qty","content":"Qty = Int\n\n","url":"morbid/alias.html#Qty"},{"doc":"morbid/alias","title":"TxHash","content":"TxHash = Hash&lt;Blake2b_256, Transaction&gt;\n\n","url":"morbid/alias.html#TxHash"},{"doc":"morbid/alias","title":"ValidatorHash","content":"ValidatorHash = Hash&lt;Blake2b_224, Script&gt;\n\n","url":"morbid/alias.html#ValidatorHash"},{"doc":"morbid/alias","title":"morbid/alias","content":"","url":"morbid/alias.html"},{"doc":"morbid","title":"morbid","content":" # Davy Jones&#39; Locker\n\n A dead-man&#39;s switch contract where you can Create Chest\n and the chest can only be unlocked after a period of time\n of not being postponed. You can also Add Treasure to the\n chest. Anyone can redeem the treasure when the deadline\n has passed.\n\n `v0.2.1`: Accounts for NFT and tokens as well,\n not just ADA.\n `v0.2.0`: Now uses `ChestLock` and `ChestKey` mechanism\n to validate actions.\n\n ```\n author      :   Kiki Ariady Putra\n maintainer  :   aurodeus@gmail.com\n ```\n\n Jakarta 2023\n\n _(re-built using Aiken v1.0.11-alpha)_","url":"morbid.html"},{"doc":"morbid/locker","title":"mint_chest","content":"mint_chest(\n  chest_name: AssetName,\n  utxo_ref: OutputReference,\n  redeemer: Void,\n  context: ScriptContext,\n) -&gt; Bool\n","url":"morbid/locker.html#mint_chest"},{"doc":"morbid/locker","title":"validate","content":"validate(\n  chest_name: AssetName,\n  policy_id: PolicyId,\n  datum: Data,\n  redeemer: Action,\n  context: ScriptContext,\n) -&gt; Bool\n When delaying unlock of a UTxO, the transaction must include any `ChestKey`\n token(s) signed by the holder(s) (it will be used to be matched against the\n `ChestLock`), the sum of input and output assets must be balance,\n and it must re-send the `ChestLock` NFT with a new deadline as the datum.\n When unlocking the chest of a UTxO, the transaction validity range must be\n after the deadline.","url":"morbid/locker.html#validate"},{"doc":"morbid/locker","title":"morbid/locker","content":"","url":"morbid/locker.html"},{"doc":"morbid/constant","title":"chest_key","content":"chest_key: ByteArray = &quot;ChestKey&quot;\n Used as the token name. Attach metadata when minting\n to keep details such as chest name and address.\n\n Give access for delaying the chest unlocking deadline\n to anyone by sending them the `ChestKey` token.","url":"morbid/constant.html#chest_key"},{"doc":"morbid/constant","title":"morbid/constant","content":"","url":"morbid/constant.html"}]);