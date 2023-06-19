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
      "5909b15909ae010000323232323232323232323232322223232533300b333323232323232323232323232323232323232323232322223232323253330283370e900018138010a99981419b8748000c09c00c4c8c8c8cc03c020004dd7181800098180009813001800800899299981419b8748000c09c00854ccc0a0cdc3a4004604e00626464646464a66605a66e1d200000213232533302f3370e9000181700089919191980b0078009bae30370013037001302d0010033034001302b00300114a060540026601600e0026eb8c0bc004c09800c0040044c94ccc0a0cdc3a4004604e0042a66605066e1d20003027003132323300a007001375a605e002604c006002002264a66605066e1d20023027002153330283370e9001181380189919191919299981699b87480000084c8c94ccc0bccdc3a4000605c002264646602201c0026eb4c0d8004c0b400400cc0d0004c0ac00c0045281815000998058038009bae302f001302600300100114a06050004605000266ec000800c88c8c8c8c94ccc098cdc3a4004002264646466e200200054ccc0a0cdc3a400460546ea8cc090c0980152002100113370000290011bad302d0013024002153330263370e90020008a5114a06048002660406044002900018040009980f18101980f1810001240009007111919299981199b87480000044c8c8c8c94ccc09ccdc3a4008002264646466e9520003302f0024bd701812000981700098128010a60103d87a80003025001330213023330213023001480092004302a001302100214c0103d87a80003021001323300400123371e6eb8cc07cc084cc07cc084cc07cc084005200048001200000337586603a603e6603a603e0049000240006002002444a6660480042980103d87a8000132325333022300300213374a90001981380125eb804ccc01401400400cc0a000cc09800888c8c8cc0540048c94ccc09800452809929998138008991919299981319b87480100044c8c94ccc0a0cdc3a4000604e002264646464a6660606066004264660440024646464a66606266e1d2002001132323233710002016a66606666e1d2002303537546605e606200a90010800899b8000148008dd6981c00098178010a50302f0013302b302d00148000c04ccc0a8c0b0cc0a8c0b0049200048038cc0852891980f8088010a99816a481334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c606200260620046eb4c0bc004c09800454cc0a52412a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e0016302d001302400214a06048002660406044002900218148010a50302900132330080012323253330253370e9002000899251302300214a060460026603e604200290021bac3301d301f3301d301f0054800120043301400123232323371200200464646018002660260024601e6eaccc080c088005200237586603c604000490021919180580099809000918071bab3301f30213301f302100148009200237586603a603e00290001980e180f00224000660269448cc04400c0088cc88c94ccc07ccdc3a400000220062a66603e66e1d200200110031002301d32533301f3370e90000008a60103d87980001533301f3370e90020008a6103d87b80001533301f533301f3370e900118109baa3301b301d003480084cdc3a400460426ea8cc06cc0740092002133301f3370e900118109baa3301b301d002480092825114c0103d87a80001533301f3370e900118109baa3301b301d00348008530103d87b800014c103d8798000301d33223253330213370e9000000899299981119b8748000004530103d87a800014c103d87980003020002153330213370e9002000899299981119b8748010004530103d87a800014c103d87b80003020002132323253330243370e90000008a60103d87b8000153330243370e90020008a6103d8798000132325333026337100080022980103d8798000153330263370e0080022980103d87a800014c103d87b8000375a6056002604400a60440086eb4c0a0004c07c00cc07c008cc068c07000920003301a301c00148000cc060c068005200033018301a00148008c0040048894ccc08000852f5c026464a66603c600600426604600466600a00a002006266600a00a00200660480066044004466600400290001119b8000100230010012222533301e0031002132323333006006001330030020040033022004375a60400064646464a66603266e1d20020011480004c8dd69810000980b801180b80099299980c19b8748008004530103d87a80001323233006001489003756603e002602c004602c00266008002911003001001222533301a00214c103d87a8000132323232533301a3371e00a002266e9520003301f375000497ae01333007007003005375c60360066eb4c06c008c07800cc070008c0040048894ccc060008530103d87a800013232323253330183371e00a002266e9520003301d374c00497ae01333007007003005375c60320066eacc064008c07000cc068008c0040048894ccc05800852f5c026464660326ea0c00c008ccc01401400400cc06800cc06000888c8cc010004008dd6198061807198061807001240009008180080091129998098010a501323253330113371e00400629444ccc01401400400cc05c00cdd7180a80111299980680109800a5114a00020040062930b1900298028021900199299980519b874800000454ccc038c02000c526153300b49011d4578706563746564206e6f206669656c647320666f7220436f6e73747200161533300a3370e90010008a99980718040018a4c2a6601692011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300b4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300800223253330073370e9000000899191919299980798090010a4c2a66018921334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c602000260200046eb4c038004c01400854ccc01ccdc3a400400226464a66601a60200042930a99805249334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c601c002600a0042a660109212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300500133001001480008888cccc01ccdc38008018069199980280299b8000448008c03c0040080088c018dd5000918021baa0015734ae7155ceaab9e5573eae815d0aba21",
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
