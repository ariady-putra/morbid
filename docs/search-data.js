window.Aiken.initSearch([{"doc":"morbid/struct/datum","title":"ChestDatum","content":"ChestDatum {\n  CreateChest { deadline: PosixTime, creator: PubKeyHash }\n  AddTreasure { ref_txn: TxHash }\n}\n\nCreateChest { deadline: PosixTime, creator: PubKeyHash }\n Anyone can create their own chests. When they `AddTreasure`, they must refer to this UTxO, otherwise it cannot be unlocked later.\nAddTreasure { ref_txn: TxHash }\n Anyone can add treasures.","url":"morbid/struct/datum.html#ChestDatum"},{"doc":"morbid/struct/datum","title":"morbid/struct/datum","content":"","url":"morbid/struct/datum.html"},{"doc":"morbid/alias","title":"PubKeyHash","content":"PubKeyHash = Hash&lt;Blake2b_224, VerificationKey&gt;\n\n","url":"morbid/alias.html#PubKeyHash"},{"doc":"morbid/alias","title":"TxHash","content":"TxHash = Hash&lt;Blake2b_256, Transaction&gt;\n\n","url":"morbid/alias.html#TxHash"},{"doc":"morbid/alias","title":"morbid/alias","content":"","url":"morbid/alias.html"},{"doc":"morbid","title":"morbid","content":" # Davy Jones&#39; Locker\n\n A dead-man&#39;s switch contract where you can Create Chest\n and the chest can only be unlocked after a period of time\n of not being postponed. You can also Add Treasure to the\n chest. Anyone can redeem the treasure when the deadline\n has passed.\n\n ```\n author      :   Kiki Ariady Putra\n maintainer  :   aurodeus@gmail.com\n ```\n\n Jakarta 2023\n\n _(built using Aiken v1.0.10-alpha)_","url":"morbid.html"},{"doc":"morbid/struct/redeemer","title":"ChestRedeemer","content":"ChestRedeemer {\n  DelayUnlock\n  UnlockChest\n}\n\nDelayUnlock\n Only the chest creator is allowed to delay-unlock.\nUnlockChest\n Anyone can unlock the chest when the deadline has passed.","url":"morbid/struct/redeemer.html#ChestRedeemer"},{"doc":"morbid/struct/redeemer","title":"morbid/struct/redeemer","content":"","url":"morbid/struct/redeemer.html"},{"doc":"morbid/locker","title":"validate","content":"validate(\n  datum: ChestDatum,\n  redeemer: ChestRedeemer,\n  context: ScriptContext,\n) -&gt; Bool\n Depending on the `ChestDatum`, either `CreateChest` or `AddTreasure`,\n the validator may need to search for the referenced `TxHash`.\n `ChestRedeemer` determines what action is to be done to the UTxO(s),\n either `DelayUnlock` or `UnlockChest`.\n When delaying unlock of a UTxO, the transaction must be signed by the\n chest creator, the sum of input ADA must not be less than output ADA,\n and it must re-create a `CreateChest` datum with a later deadline.\n When unlocking the chest of a UTxO, the transaction validity range\n must be after the deadline.","url":"morbid/locker.html#validate"},{"doc":"morbid/locker","title":"morbid/locker","content":"","url":"morbid/locker.html"},{"doc":"morbid/tests","title":"morbid/tests","content":" ```\n ┍━ morbid/tests ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n │ PASS [mem: 4103857, cpu: 2199673275] allow_delay_by_creator\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 3762864, cpu: 2071753224] disallow_delay_by_others\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b337948343568ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ signed_by_creator ? False\n │ PASS [mem: 3302782, cpu: 1857373975] disallow_delay_by_no_tx_signer\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ signed_by_creator ? False\n │ PASS [mem: 3938028, cpu: 2136189008] disallow_delay_by_draining_ada_to_another_script_address\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f5a7affd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ input_script_address == output_script_address ? False\n │ ↳ not_drained ? False\n │ PASS [mem: 3845104, cpu: 2080594837] disallow_delay_with_draining_ada\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ input_ada &gt;= output_ada ? False\n │ ↳ not_drained ? False\n │ PASS [mem: 3749134, cpu: 2017304437] disallow_delay_without_recreate_chest\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87980d87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ recreated_chest ? False\n │ PASS [mem: 4107056, cpu: 2201310334] disallow_delay_by_recreate_chest_with_earlier_deadline\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f07ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ unsafe_unwrap.finite_start_of(ctx.transaction.validity_range) &lt; new_deadline ? False\n │ ↳ postponed_after_now ? False\n │ ↳ recreated_chest ? False\n │ PASS [mem: 4034687, cpu: 2172607928] disallow_delay_by_recreate_chest_with_unsigned_creator\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ signed_by_new_creator ? False\n │ ↳ recreated_chest ? False\n │ PASS [mem: 4556549, cpu: 2409370487] allow_delay_by_recreate_chest_with_signed_creator\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b3379483435685143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 5929559, cpu: 3139209075] allow_delay_by_referenced_datum_with_signed_creator\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b3379483435685143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 5763730, cpu: 3075724808] disallow_delay_by_referenced_datum_with_another_script_address\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f5a7affd87a80ffa140a1401a01312d00d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b3379483435685143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ input_script_address == output_script_address ? False\n │ ↳ not_drained ? False\n │ PASS [mem: 5475957, cpu: 2929086171] allow_delay_by_referenced_datum\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 5061910, cpu: 2773362886] disallow_delay_by_incorrect_referenced_datum_tx_signer\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b337948343568ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ ↳ signed_by_creator ? False\n │ PASS [mem: 5005812, cpu: 2749032473] disallow_delay_by_incorrect_referenced_datum_type\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 5002618, cpu: 2751500093] disallow_delay_by_incorrect_referenced_datum_tx_hash\n │ ↳ Datum: &quot;d87a9f513037683527355f5f37786e5f5f48343568ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f513037683527355f5f37786e5f5f48343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 3729544, cpu: 2052159563] disallow_delay_by_no_referenced_datum\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87980&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f5135306d3327355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 1136372, cpu:  664613030] allow_unlock_after_deadline\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87a80&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 1124014, cpu:  659656090] disallow_unlock_before_deadline\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87a80&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87980d87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 1160944, cpu:  679257671] disallow_unlock_during_deadline\n │ ↳ Datum: &quot;d8799f055143683335375f437233343730725f504b48ff&quot;\n │ ↳ Redeemer: &quot;d87a80&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 2348950, cpu: 1317296699] allow_unlock_by_referenced_datum\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87a80&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 2266842, cpu: 1281855119] disallow_unlock_by_incorrect_referenced_datum_type\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87a80&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 2245284, cpu: 1277095194] disallow_unlock_by_incorrect_referenced_datum_tx_hash\n │ ↳ Datum: &quot;d87a9f513037683527355f5f37786e5f5f48343568ff&quot;\n │ ↳ Redeemer: &quot;d87a80&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 2336592, cpu: 1312339759] disallow_unlock_by_referenced_datum_before_deadline\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87a80&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87980d87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n │ PASS [mem: 2373618, cpu: 1331990396] disallow_unlock_by_referenced_datum_during_deadline\n │ ↳ Datum: &quot;d87a9f514372333437334368333537377848343568ff&quot;\n │ ↳ Redeemer: &quot;d87a80&quot;\n │ ↳ ScriptContext: &quot;d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff&quot;\n ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 24 tests | 24 passed | 0 failed\n ```","url":"morbid/tests.html"},{"doc":"morbid/chain","title":"and","content":"and(continue: Bool, next: fn(Void) -&gt; Bool) -&gt; Bool\n","url":"morbid/chain.html#and"},{"doc":"morbid/chain","title":"validate","content":"validate() -&gt; Bool\n","url":"morbid/chain.html#validate"},{"doc":"morbid/chain","title":"morbid/chain","content":" # Lazy Short-circuit Validations\n\n Chain together many validations that may fail.\n\n For example,\n ```\n validator {\n   fn validate(datum: Data, redeemer: Data, context: ScriptContext) -&gt; Bool {\n     chain.validate()\n       |&gt; and(must_be_condition_1(_, context, datum))\n       |&gt; and(must_be_condition_2(_, context, redeemer))\n       |&gt; and(must_be_condition_3(_, context))\n   }\n }\n ```\n Where,\n ```\n fn must_be_condition_1(_, context, datum) {\n   let condition_1 = context |&gt; must_validate_datum(datum) // call another fn somewhere\n   condition_1? // useful for tracing\n }\n \n fn must_be_condition_2(_, context, redeemer) {\n   let condition_2 = context |&gt; must_validate_redeemer(redeemer) // call another fn somewhere\n   condition_2? // useful for tracing\n }\n \n fn must_be_condition_3(_, context) {\n   let condition_3 = context |&gt; must_validate_other_things() // call another fn somewhere\n   condition_3? // useful for tracing\n }\n ```\n As of `Aiken v1.0.10-alpha` it&#39;s not possible to directly chain `|&gt;` with `?` and `&amp;&amp;` operators,\n for example:\n ```\n (context |&gt; must_be_condition_1())? &amp;&amp; (context |&gt; must_be_condition_2())? &amp;&amp; (context |&gt; must_be_condition_3())?\n ```\n Calling `|&gt;` indirectly by declaring anonymous functions could give weird non-short-circuit behavior.\n Using `list.and([fn(..) {..}])` or `reduce` or `foldr` and `foldl` loses the lazyness behavior for some reason.\n Hence this solution; internally it mimics the `option.and_then` function.","url":"morbid/chain.html"}]);