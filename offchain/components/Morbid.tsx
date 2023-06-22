import {
  Constr,
  Data,
  Lucid,
  SpendingValidator,
  UTxO,
  assetsToValue,
} from "lucid-cardano";
import { useEffect, useState } from "react";
import { getDatumFields, getLatestBlockInfo } from "../utils/blockfrost";
import { hashDatum } from "../utils/cardano";
import { useStoreActions, useStoreState } from "../utils/chest";

const Morbid = (props: {
  lucid: Lucid;
  setActionResult: (actionResult: string) => void;
}) => {
  const lucid = props.lucid;
  const setActionResult = props.setActionResult;

  const chest = useStoreState((state) => state.chest);
  const setChest = useStoreActions((actions) => actions.setChest);

  const [loaded, setLoaded] = useState(false);

  useEffect(() => {
    setLoaded(true);
  }, []);

  const morbidScript: SpendingValidator = {
    type: "PlutusV2",
    script:
      "590bd9590bd6010000323232323232323232323232322223232533300b33332323232323232323232323232323232323232323232323232322223232323232533302d3370e900018160010a99981699b8748000c0b000c4c8c8c94ccc0c0cdc3a4004605e00e26466022014004605c00e0086eb8c0d4004c0d4004c0ac00c0040044c94ccc0b4cdc3a400060580042a66605a66e1d2002302c00313232533302f3370e90011817003099191919299981999b87480000084c8c94ccc0d4cdc3a400060680022646464660300220026eb8c0f4004c0f4004c0cc00400cc0e8004c0c400c00452818180009980680480118168030019bae3034001302b003001001132533302d3370e900118160010a99981699b8748000c0b000c4c8c94ccc0bccdc3a4004605c00c26466018012004605a00c0066eb4c0d0004c0ac00c0040044c94ccc0b4cdc3a400460580042a66605a66e1d2002302c00313232533302f3370e90011817003099191919299981999b87480000084c8c94ccc0d4cdc3a4000606800226464660260200026eb4c0f0004c0cc00400cc0e8004c0c400c00452818180009980680480118168030019bae3034001302b00300100114a0606200660606062004605e606060600026605a0046605a0066605a66048604c002900125eb8088c8c8c8c94ccc0a8cdc3a4004002264646466e200200054ccc0b0cdc3a4004605c6ea8cc0a0c0a80152002100113370000290011bad303100130280021533302a3370e90020008a5114a0605000266048604c00290001804000998111812198111812001240009007111919299981399b87480000044c8c8c8c94ccc0accdc3a4008002264646466e952000330330024bd701814000981900098148010a60103d87a80003029001330253027330253027001480092004302e001302500214c0103d87a80003025001323300400123371e6eb8cc08cc094cc08cc094cc08cc094005200048001200000337586604260466604260460049000240006002002444a6660500042980103d87a8000132325333026300300213374a90001981580125eb804ccc01401400400cc0b000cc0a800888c8c8cc0640048c94ccc0a800452809929998158008991919299981519b87480100044c8c94ccc0b0cdc3a40006056002264646464a666068606e0042646604c002466e20c8c8c8c94ccc0d8cdc3a400460706ea8cc0c8c0d000d2002100113370000290011bad303b00130323253330353370e9001181a00088008a9981b2492a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e001633030303200148000c050004cc0b8c0c0cc0b8c0c0049200048038014cc095289198118088010a99818a49334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c606a002606a0046eb4c0cc004c0a800454cc0b52412a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e00163031001302800214a0605000266048604c002900218168010a50302d001323300d0012323253330293370e9002000899251302700214a0604e00266046604a00290021bac33021302333021302300548001200433018001232323232323232533302f303200213232323232325333035303800213253330323371200200e266ebc02000852818098020a99819249334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564001630360013230150013301600100a375860620046eb0c0bc004c8c8c05c004cc0740048cdd81ba8301937566605460580029001198151816000a40006eb0cc0a0c0a80212004300d004153302c4901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016303000132300f00133010001004375860560046eb0c0a4004c8c8c044004cc05c0048cdd81ba83013375666048604c66048604c00290012400466048604c66048604c0029001240006eb0cc088c09000920002323253330283370e900100089925130260021324a0604c004604c0026604460480029000198101811002240006602e9448cc05400c0088cc88c94ccc08ccdc3a400000220062a66604666e1d20020011003100230213253330233370e90000008a6103d8798000153330233370e90020008a6103d87b80001533302353330233370e900118129baa3301f3021003480084cdc3a4004604a6ea8cc07cc084009200213330233370e900118129baa3301f3021002480092825114c0103d87a8000153330233370e900118129baa3301f302100348008530103d87b800014c103d8798000302133223253330253370e9000000899299981319b8748000004530103d87a800014c103d87980003024002153330253370e9002000899299981319b8748010004530103d87a800014c103d87b80003024002132323253330283370e90000008a60103d87b8000153330283370e90020008a6103d879800013232533302a337100080022980103d87980001533302a3370e0080022980103d87a800014c103d87b8000375a605e002604c00a604c0086eb4c0b0004c08c00cc08c008cc078c08000920003301e302000148000cc070c07800520003301c301e001480088ccc0080052000223370000200460020024444a666048006200426464666600c00c0026600600400800660500086eb4c09800cc004004894ccc08000452f5c02646466046004660080086600a002466603e66ebc00400d28251302400230220013001001222533301f00214bd7009919299980e9801801099811001199802802800801899980280280080198118019810801180080091299980e0008a5ef6c6101800001018000132323232323232337606e9ccc090dd40020011ba733024005001375860420046eb0c07c004cc01801800cdd6980e801980e8011810001180f0009191919299980c99b874800800452000132375a6040002602e004602e00264a66603066e1d200200114c103d87a80001323233006001489003756603e002602c004602c00266008002911003001001222533301a00214c103d87a8000132323232533301a3371e00a002266e9520003301f375000497ae01333007007003005375c60360066eb4c06c008c07800cc070008c0040048894ccc060008530103d87a800013232323253330183371e00a002266e9520003301d374c00497ae01333007007003005375c60320066eacc064008c07000cc068008c0040048894ccc05800852f5bded8c02646466032600600466600a00a002006603400660300044464660080020046eb0cc030c038cc030c038009200048040c0040048894ccc04c00852809919299980899b8f00200314a2266600a00a002006602e0066eb8c054008894ccc0340084c0052614a00020040062930b1900298028021900199299980519b874800000454ccc038c02000c526153300b4911d4578706563746564206e6f206669656c647320666f7220436f6e73747200161533300a3370e90010008a99980718040018a4c2a6601692011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300b4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300800223253330073370e9000000899191919299980798090010a4c2a66018921334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c602000260200046eb4c038004c01400854ccc01ccdc3a400400226464a66601a60200042930a99805249334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c601c002600a0042a660109212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300500133001001480008888cccc01ccdc38008018069199980280299b8000448008c03c0040080088c018dd5000918021baa0015734ae7155ceaab9e5573eae815d0aba21",
  };

  const createChest = async () => {
    try {
      console.log("CreateChest():");
      if (lucid) {
        const block = await getLatestBlockInfo();
        const time = block["time"] * 1_000; // ms
        console.log({ time: time });

        const userAddress = await lucid.wallet.address();
        console.log({ userAddress: userAddress });

        const morbidAddress = lucid.utils.validatorToAddress(morbidScript);
        console.log({ scriptAddress: morbidAddress });

        const deadline = time;
        const creator =
          lucid.utils.getAddressDetails(userAddress).paymentCredential?.hash;
        const createChest = Data.to(
          new Constr(0, [BigInt(deadline), String(creator)])
        );
        console.log({ datum: createChest });

        const tx = await lucid
          .newTx()
          .payToContract(
            morbidAddress,
            { inline: createChest, scriptRef: morbidScript },
            { lovelace: BigInt(42_000000) }
          )
          .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log({ txHash: txHash });

        setActionResult(`TxHash: ${txHash}`);
        setChest({ txHash: txHash });
        return txHash;
      }
      throw { error: "Invalid Lucid State!" };
    } catch (x) {
      setActionResult(JSON.stringify(x));
    }
  };

  const addTreasure = async () => {
    try {
      console.log(`AddTreasure(${chest.txHash}):`);
      if (lucid && chest.txHash.length) {
        const userAddress = await lucid.wallet.address();
        console.log({ userAddress: userAddress });

        const morbidAddress = lucid.utils.validatorToAddress(morbidScript);
        console.log({ scriptAddress: morbidAddress });

        const refTxn = chest.txHash;
        const addTreasure = Data.to(new Constr(1, [String(refTxn)]));
        console.log({ datum: addTreasure });

        const tx = await lucid
          .newTx()
          .payToContract(
            morbidAddress,
            { inline: addTreasure },
            { lovelace: BigInt(42_000000) }
          )
          .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log({ txHash: txHash });

        setActionResult(`TxHash: ${txHash}`);
        return txHash;
      }
      throw { error: "Invalid Lucid State!" };
    } catch (x) {
      setActionResult(JSON.stringify(x));
    }
  };

  const delayUnlock = async () => {
    try {
      console.log(`DelayUnlock(${chest.txHash}):`);
      if (lucid && chest.txHash.length) {
        const block = await getLatestBlockInfo();
        const time = block["time"] * 1_000; // ms
        console.log({ time: time });

        const userAddress = await lucid.wallet.address();
        console.log({ userAddress: userAddress });

        const morbidAddress = lucid.utils.validatorToAddress(morbidScript);
        console.log({ scriptAddress: morbidAddress });

        const utxos = await lucid.utxosAt(morbidAddress);
        console.log({ utxos: utxos });
        if (!utxos?.length) {
          throw { emptyScriptAddress: "No UTxO to redeem." };
        }

        let treasure = 0;
        const chestUTxOs: UTxO[] = [];
        const refScript: UTxO[] = [];

        const forEachAsync = async (
          utxos: UTxO[],
          callback: { (utxo: UTxO): Promise<void>; (arg0: any): any }
        ) => {
          for (const utxo of utxos) {
            await callback(utxo);
          }
        };

        const myAsyncCallback = async (utxo: UTxO) => {
          if (!utxo.datum) return;

          const datumFields = await getDatumFields(hashDatum(utxo.datum));

          switch (datumFields.length) {
            case 2: // CreateChest
              if (utxo.txHash === chest.txHash) {
                const deadline = datumFields[0];
                const deadlineValue = deadline["int"];
                if (deadlineValue < time) {
                  refScript.push(utxo);
                  chestUTxOs.push(utxo);
                  treasure += Number.parseInt(
                    assetsToValue(utxo.assets).to_js_value()["coin"]
                  );
                }
              }
              return;

            case 1: // AddTreasure
              const refTxn = datumFields[0];
              const refTxnValue = refTxn["bytes"];
              if (refTxnValue === chest.txHash) {
                chestUTxOs.push(utxo);
                treasure += Number.parseInt(
                  assetsToValue(utxo.assets).to_js_value()["coin"]
                );
              }
              return;

            default:
              return;
          }
        };

        await forEachAsync(utxos, myAsyncCallback);

        console.log({ chestUTxOs: chestUTxOs });
        if (!chestUTxOs.length) {
          throw { nothingToUnlock: "No valid UTxO to redeem." };
        }
        console.log({ refScript: refScript });
        if (!refScript.length) {
          throw { refScriptNotFound: "No reference script found." };
        }

        const delayUnlock = Data.to(new Constr(0, [])); // redeemer
        console.log({ redeemer: delayUnlock }); // DelayUnlock

        const deadline = time + 1_000; // +1sec
        const creator =
          lucid.utils.getAddressDetails(userAddress).paymentCredential?.hash;
        const createChest = Data.to(
          new Constr(0, [BigInt(deadline), String(creator)])
        );
        console.log({ datum: createChest });

        console.log({ treasure: treasure });
        const tx = await lucid
          .newTx()
          .collectFrom(chestUTxOs, delayUnlock)
          .readFrom(refScript) // reference script
          .addSigner(userAddress)
          .payToContract(
            morbidAddress, // re-create chest
            { inline: createChest, scriptRef: morbidScript },
            { lovelace: BigInt(treasure) }
          )
          //   .payToAddressWithData(
          //     userAddress, // drain to any address
          //     { inline: createChest },
          //     { lovelace: BigInt(treasure) }
          //   )
          .validFrom(time)
          .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log({ txHash: txHash });

        setActionResult(`TxHash: ${txHash}`);
        setChest({ txHash: txHash });
        return txHash;
      }
      throw { error: "Invalid Lucid State!" };
    } catch (x) {
      setActionResult(JSON.stringify(x));
    }
  };

  const unlockChest = async () => {
    try {
      console.log(`UnlockChest(${chest.txHash}):`);
      if (lucid && chest.txHash.length) {
        const block = await getLatestBlockInfo();
        const time = block["time"] * 1_000; // ms
        console.log({ time: time });

        const morbidAddress = lucid.utils.validatorToAddress(morbidScript);
        console.log({ scriptAddress: morbidAddress });

        const utxos = await lucid.utxosAt(morbidAddress);
        console.log({ utxos: utxos });
        if (!utxos?.length) {
          throw { emptyScriptAddress: "No UTxO to redeem." };
        }

        const chestUTxOs: UTxO[] = [];
        const refScript: UTxO[] = [];

        const forEachAsync = async (
          utxos: UTxO[],
          callback: { (utxo: UTxO): Promise<void>; (arg0: any): any }
        ) => {
          for (const utxo of utxos) {
            await callback(utxo);
          }
        };

        const myAsyncCallback = async (utxo: UTxO) => {
          if (!utxo.datum) return;

          const datumFields = await getDatumFields(hashDatum(utxo.datum));

          switch (datumFields.length) {
            case 2: // CreateChest
              if (utxo.txHash === chest.txHash) {
                const deadline = datumFields[0];
                const deadlineValue = deadline["int"];
                if (deadlineValue < time) {
                  refScript.push(utxo);
                  chestUTxOs.push(utxo);
                }
              }
              return;

            case 1: // AddTreasure
              const refTxn = datumFields[0];
              const refTxnValue = refTxn["bytes"];
              if (refTxnValue === chest.txHash) {
                chestUTxOs.push(utxo);
              }
              return;

            default:
              return;
          }
        };

        await forEachAsync(utxos, myAsyncCallback);

        console.log({ chestUTxOs: chestUTxOs });
        if (!chestUTxOs.length) {
          throw { nothingToUnlock: "No valid UTxO to redeem." };
        }
        console.log({ refScript: refScript });
        if (!refScript.length) {
          throw { refScriptNotFound: "No reference script found." };
        }

        const unlockChest = Data.to(new Constr(1, [])); // redeemer
        console.log({ redeemer: unlockChest }); // UnlockChest

        const tx = await lucid
          .newTx()
          .collectFrom(chestUTxOs, unlockChest)
          .readFrom(refScript) // reference script
          .validFrom(time)
          .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log({ txHash: txHash });

        setActionResult(`TxHash: ${txHash}`);
        setChest({ txHash: "" });
        return txHash;
      }
      throw { error: "Invalid Lucid State!" };
    } catch (x) {
      setActionResult(JSON.stringify(x));
    }
  };

  return !loaded ? (
    <></>
  ) : (
    <div>
      {/* CreateChest */}
      <button className="btn btn-primary m-5" onClick={createChest}>
        Create Chest
      </button>

      {chest.txHash.length ? (
        <>
          {/* AddTreasure */}
          <button className="btn btn-secondary m-5" onClick={addTreasure}>
            Add Treasure
          </button>

          {/* DelayUnlock */}
          <button className="btn btn-secondary m-5" onClick={delayUnlock}>
            Delay Unlock
          </button>

          {/* UnlockChest */}
          <button className="btn btn-secondary m-5" onClick={unlockChest}>
            Unlock Chest
          </button>

          {/* ChestInfo */}
          <div>{`Current ChestID: ${chest.txHash}`}</div>
        </>
      ) : (
        <div>I don't remember having any chest currently...</div>
      )}
    </div>
  );
};

export default Morbid;
