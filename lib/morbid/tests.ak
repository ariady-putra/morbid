//// ```
//// ┍━ morbid/tests ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//// │ PASS [mem: 4103857, cpu: 2199673275] allow_delay_by_creator
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 3762864, cpu: 2071753224] disallow_delay_by_others
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b337948343568ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ signed_by_creator ? False
//// │ PASS [mem: 3302782, cpu: 1857373975] disallow_delay_by_no_tx_signer
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ signed_by_creator ? False
//// │ PASS [mem: 3939230, cpu: 2136750961] disallow_delay_by_draining_ada_to_another_script_address
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f5a7affd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ input_script_address == output_script_address ? False
//// │ ↳ not_drained ? False
//// │ PASS [mem: 3843902, cpu: 2078938780] disallow_delay_with_draining_ada
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ input_ada >= output_ada ? False
//// │ ↳ not_drained ? False
//// │ PASS [mem: 3749134, cpu: 2017304437] disallow_delay_without_recreate_chest
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87980d87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ recreated_chest ? False
//// │ PASS [mem: 4107056, cpu: 2201310334] disallow_delay_by_recreate_chest_with_earlier_deadline
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f07ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ unsafe_unwrap.finite_start_of(ctx.transaction.validity_range) < new_deadline ? False
//// │ ↳ postponed_after_now ? False
//// │ ↳ recreated_chest ? False
//// │ PASS [mem: 4034687, cpu: 2172607928] disallow_delay_by_recreate_chest_with_unsigned_creator
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ signed_by_new_creator ? False
//// │ ↳ recreated_chest ? False
//// │ PASS [mem: 4556549, cpu: 2409370487] allow_delay_by_recreate_chest_with_signed_creator
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b3379483435685143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 5929559, cpu: 3139209075] allow_delay_by_referenced_datum_with_signed_creator
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b3379483435685143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 5764932, cpu: 3076286761] disallow_delay_by_referenced_datum_with_another_script_address
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f5a7affd87a80ffa140a1401a01312d00d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b3379483435685143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ input_script_address == output_script_address ? False
//// │ ↳ not_drained ? False
//// │ PASS [mem: 5475957, cpu: 2929086171] allow_delay_by_referenced_datum
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 5061910, cpu: 2773362886] disallow_delay_by_incorrect_referenced_datum_tx_signer
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f0651303768337227355075384b337948343568ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f51303768337227355075384b337948343568ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ ↳ signed_by_creator ? False
//// │ PASS [mem: 5005812, cpu: 2749032473] disallow_delay_by_incorrect_referenced_datum_type
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 5002618, cpu: 2751500093] disallow_delay_by_incorrect_referenced_datum_tx_hash
//// │ ↳ Datum: "d87a9f513037683527355f5f37786e5f5f48343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f513037683527355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f513037683527355f5f37786e5f5f48343568ffffd87a80ffffd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a01312d00d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 3729544, cpu: 2052159563] disallow_delay_by_no_referenced_datum
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87980"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f5135306d3327355f5f37786e5f5f48343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffff809fd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa140a1401a00989680d87b9fd8799f065143683335375f437233343730725f504b48ffffd87a80ffffa140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87b80d87a80ffff9f5143683335375f437233343730725f504b48ffa0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 1136372, cpu:  664613030] allow_unlock_after_deadline
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 1124014, cpu:  659656090] disallow_unlock_before_deadline
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87980d87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 1160944, cpu:  679257671] disallow_unlock_during_deadline
//// │ ↳ Datum: "d8799f055143683335375f437233343730725f504b48ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f808080a140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2348950, cpu: 1317296699] allow_unlock_by_referenced_datum
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2266842, cpu: 1281855119] disallow_unlock_by_incorrect_referenced_datum_type
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd87a9f514372333437334368333537377848343568ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2245284, cpu: 1277095194] disallow_unlock_by_incorrect_referenced_datum_tx_hash
//// │ ↳ Datum: "d87a9f513037683527355f5f37786e5f5f48343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f06ffd87a80ffd8799fd87b80d87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2336592, cpu: 1312339759] disallow_unlock_by_referenced_datum_before_deadline
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87980d87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// │ PASS [mem: 2373618, cpu: 1331990396] disallow_unlock_by_referenced_datum_during_deadline
//// │ ↳ Datum: "d87a9f514372333437334368333537377848343568ff"
//// │ ↳ Redeemer: "d87a80"
//// │ ↳ ScriptContext: "d8799fd8799f9fd8799fd8799fd8799f514372333437334368333537377848343568ff00ffd8799fd8799fd87a9f513563723170375f346464723335355f2121ffd87a80ffa0d87b9fd8799f055143683335375f437233343730725f504b48ffffd87a80ffffff8080a140a1401a001e8480a080a0d8799fd8799fd87a9f05ffd87a80ffd8799fd87a9f06ffd87a80ffff80a0a0d8799f40ffffd87a9fd8799fd8799f40ff00ffffff"
//// ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 24 tests | 24 passed | 0 failed
//// ```

use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/interval.{Interval, after, before, between}
use aiken/list.{push}
use aiken/time.{PosixTime}
use aiken/transaction.{
  Datum, InlineDatum, NoDatum, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId,
}
use aiken/transaction/credential.{Address, Script, from_script}
use aiken/transaction/value.{from_lovelace, to_minted_value, zero}
use morbid/alias.{PubKeyHash, TxHash}
use morbid/locker.{validate}
use morbid/struct/datum.{AddTreasure, CreateChest}
use morbid/struct/redeemer.{DelayUnlock, UnlockChest}
use string_util/cbor.{print}

fn build_txn_context(validity_range: Interval<PosixTime>) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: [],
      reference_inputs: [],
      outputs: [],
      fee: from_lovelace(2_000_000),
      mint: zero() |> to_minted_value(),
      certificates: [],
      withdrawals: dict.new(),
      validity_range,
      extra_signatories: [],
      redeemers: dict.new(),
      datums: dict.new(),
      id: TransactionId(""),
    },
    Spend(
      OutputReference { transaction_id: TransactionId(""), output_index: 0 },
    ),
  )
}

fn add_signatory(context: ScriptContext, signatory: PubKeyHash) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: context.transaction.inputs,
      reference_inputs: context.transaction.reference_inputs,
      outputs: context.transaction.outputs,
      fee: context.transaction.fee,
      mint: context.transaction.mint,
      certificates: context.transaction.certificates,
      withdrawals: context.transaction.withdrawals,
      validity_range: context.transaction.validity_range,
      extra_signatories: context.transaction.extra_signatories
        |> push(signatory),
      redeemers: context.transaction.redeemers,
      datums: context.transaction.datums,
      id: context.transaction.id,
    },
    context.purpose,
  )
}

fn add_tx_input(
  context: ScriptContext,
  tx_in: transaction.Input,
) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: context.transaction.inputs
        |> push(tx_in),
      reference_inputs: context.transaction.reference_inputs,
      outputs: context.transaction.outputs,
      fee: context.transaction.fee,
      mint: context.transaction.mint,
      certificates: context.transaction.certificates,
      withdrawals: context.transaction.withdrawals,
      validity_range: context.transaction.validity_range,
      extra_signatories: context.transaction.extra_signatories,
      redeemers: context.transaction.redeemers,
      datums: context.transaction.datums,
      id: context.transaction.id,
    },
    context.purpose,
  )
}

fn new_tx_input(
  tx_hash: TxHash,
  address: Address,
  lovelace: Int,
  datum: Datum,
) -> transaction.Input {
  transaction.Input(
    OutputReference(TransactionId(tx_hash), 0),
    Output {
      address,
      value: lovelace |> from_lovelace,
      datum,
      reference_script: None,
    },
  )
}

fn add_tx_output(
  context: ScriptContext,
  tx_out: transaction.Output,
) -> ScriptContext {
  ScriptContext(
    Transaction {
      inputs: context.transaction.inputs,
      reference_inputs: context.transaction.reference_inputs,
      outputs: context.transaction.outputs
        |> push(tx_out),
      fee: context.transaction.fee,
      mint: context.transaction.mint,
      certificates: context.transaction.certificates,
      withdrawals: context.transaction.withdrawals,
      validity_range: context.transaction.validity_range,
      extra_signatories: context.transaction.extra_signatories,
      redeemers: context.transaction.redeemers,
      datums: context.transaction.datums,
      id: context.transaction.id,
    },
    context.purpose,
  )
}

fn new_tx_output(
  address: Address,
  lovelace: Int,
  datum: Datum,
) -> transaction.Output {
  transaction.Output {
    address,
    value: lovelace |> from_lovelace,
    datum,
    reference_script: None,
  }
}

const deadline: PosixTime = 5

const chest_creator: PubKeyHash = "Ch357_Cr3470r_PKH"

const other_pkh: PubKeyHash = "07h3r'5Pu8K3yH45h"

const create_chest_tx_hash: TxHash = "Cr3473Ch3577xH45h"

const some_tx_hash: TxHash = "50m3'5__7xn__H45h"

const other_tx_hash: TxHash = "07h5'5__7xn__H45h"

const script_address_1: Hash<Blake2b_224, Script> = "5cr1p7_4ddr355_!!"

const script_address_2: Hash<Blake2b_224, Script> = "5cr1p7_4ddr355_Zz"

test allow_delay_by_creator() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, chest_creator)
  let redeemer = DelayUnlock
  let tx_in =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 10_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test disallow_delay_by_others() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, other_pkh)
  let redeemer = DelayUnlock
  let tx_in =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 10_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(other_pkh)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_no_tx_signer() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, other_pkh)
  let redeemer = DelayUnlock
  let tx_in =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 10_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_draining_ada_to_another_script_address() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, chest_creator)
  let redeemer = DelayUnlock
  let tx_in =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_2),
      lovelace: 10_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_with_draining_ada() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, chest_creator)
  let redeemer = DelayUnlock
  let tx_in =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 0,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 10_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_without_recreate_chest() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = DelayUnlock
  let tx_in =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 10_000_000,
      datum: NoDatum,
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_recreate_chest_with_earlier_deadline() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = DelayUnlock
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, chest_creator)
  let tx_in =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 10_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline + 2)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_recreate_chest_with_unsigned_creator() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = DelayUnlock
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, other_pkh)
  let tx_in =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 10_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test allow_delay_by_recreate_chest_with_signed_creator() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = DelayUnlock
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, other_pkh)
  let tx_in =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 10_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_signatory(other_pkh)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test allow_delay_by_referenced_datum_with_signed_creator() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, other_pkh)
  let redeemer = DelayUnlock
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(ref_datum),
         )
  let tx_in =
    other_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 20_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_signatory(other_pkh)
      |> add_tx_input(ref_txn)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test disallow_delay_by_referenced_datum_with_another_script_address() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, other_pkh)
  let redeemer = DelayUnlock
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(ref_datum),
         )
  let tx_in =
    other_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_2),
      lovelace: 20_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_signatory(other_pkh)
      |> add_tx_input(ref_txn)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test allow_delay_by_referenced_datum() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, chest_creator)
  let redeemer = DelayUnlock
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(ref_datum),
         )
  let tx_in =
    other_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 20_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(ref_txn)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test disallow_delay_by_incorrect_referenced_datum_tx_signer() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, other_pkh)
  let redeemer = DelayUnlock
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(ref_datum),
         )
  let tx_in =
    other_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 20_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(other_pkh)
      |> add_tx_input(ref_txn)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_incorrect_referenced_datum_type() {
  //
  // arrange:
  let ref_datum = AddTreasure(create_chest_tx_hash)
  // ref_datum refers to own datum
  let datum = AddTreasure(create_chest_tx_hash)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, chest_creator)
  let redeemer = DelayUnlock
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(ref_datum),
         )
  let tx_in =
    other_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 20_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(ref_txn)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_incorrect_referenced_datum_tx_hash() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(other_tx_hash)
  // datum refers to another tx-hash
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, chest_creator)
  let redeemer = DelayUnlock
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(ref_datum),
         )
  let tx_in =
    other_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 20_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(ref_txn)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_delay_by_no_referenced_datum() {
  //
  // arrange:
  let datum = AddTreasure(create_chest_tx_hash)
  // re-create chest:
  let new_datum = CreateChest(deadline + 1, chest_creator)
  let redeemer = DelayUnlock
  let tx_in =
    some_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 10_000_000,
           datum: InlineDatum(datum),
         )
  let tx_out =
    new_tx_output(
      address: from_script(script_address_1),
      lovelace: 10_000_000,
      datum: InlineDatum(new_datum),
    )
  let context =
    after(deadline)
      |> build_txn_context()
      |> add_signatory(chest_creator)
      |> add_tx_input(tx_in)
      |> add_tx_output(tx_out)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test allow_unlock_after_deadline() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = UnlockChest
  let context =
    after(deadline + 1)
      |> build_txn_context()
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test disallow_unlock_before_deadline() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = UnlockChest
  let context =
    before(deadline + 1)
      |> build_txn_context()
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_unlock_during_deadline() {
  //
  // arrange:
  let datum = CreateChest(deadline, chest_creator)
  let redeemer = UnlockChest
  let context =
    between(deadline, deadline + 1)
      |> build_txn_context()
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test allow_unlock_by_referenced_datum() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = UnlockChest
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 0,
           datum: InlineDatum(ref_datum),
         )
  let context =
    after(deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  validate(datum, redeemer, context)
}

test disallow_unlock_by_incorrect_referenced_datum_type() {
  //
  // arrange:
  let ref_datum = AddTreasure(create_chest_tx_hash)
  // ref_datum refers to own datum
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = UnlockChest
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 0,
           datum: InlineDatum(ref_datum),
         )
  let context =
    after(deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_unlock_by_incorrect_referenced_datum_tx_hash() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(other_tx_hash)
  // datum refers to another tx-hash
  let redeemer = UnlockChest
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 0,
           datum: InlineDatum(ref_datum),
         )
  let context =
    after(deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_unlock_by_referenced_datum_before_deadline() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = UnlockChest
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 0,
           datum: InlineDatum(ref_datum),
         )
  let context =
    before(deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}

test disallow_unlock_by_referenced_datum_during_deadline() {
  //
  // arrange:
  let ref_datum = CreateChest(deadline, chest_creator)
  let datum = AddTreasure(create_chest_tx_hash)
  let redeemer = UnlockChest
  let ref_txn =
    create_chest_tx_hash
      |> new_tx_input(
           address: from_script(script_address_1),
           lovelace: 0,
           datum: InlineDatum(ref_datum),
         )
  let context =
    between(deadline, deadline + 1)
      |> build_txn_context()
      |> add_tx_input(ref_txn)
  //
  // trace:
  trace "Datum"
    |> print(datum)
  trace "Redeemer"
    |> print(redeemer)
  trace "ScriptContext"
    |> print(context)
  //
  // assert:
  !validate(datum, redeemer, context)
}
