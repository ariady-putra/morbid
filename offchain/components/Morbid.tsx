import {
  Assets,
  Constr,
  Data,
  Lucid,
  MintingPolicy,
  SpendingValidator,
  UTxO,
  assetsToValue,
  fromText,
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
      "590c3c590c390100003232323232323232323232322223232533300b33332323232323232323232323232323232323232323232323232323232323232323232323232323232322223232323232533303c3370e9000181d8010a99981e19b8748000c0ec00c4c8c8c94ccc0fccdc3a4004607c00e26466022014004607a00e0086eb8c10c004c10c004c0e800c0040044c94ccc0f0cdc3a400060760042a66607866e1d2002303b00313232533303e3370e9001181e803099191919299982119b87480000084c8c94ccc110cdc3a400060860022646464660300220026eb8c12c004c12c004c10800400cc120004c10000c004528181f80099806804801181e0030019bae3042001303a003001001132533303c3370e9001181d8010a99981e19b8748000c0ec00c4c8c94ccc0f8cdc3a4004607a00c26466018012004607800c0066eb4c108004c0e800c0040044c94ccc0f0cdc3a400460760042a66607866e1d2002303b00313232533303e3370e9001181e803099191919299982119b87480000084c8c94ccc110cdc3a4000608600226464660260200026eb4c128004c10800400cc120004c10000c004528181f80099806804801181e0030019bae3042001303a00300100114a0607e006607c607e004607a607c607c00266076004660760066607666066606a002900125eb8088c8c8c8c94ccc0e4cdc3a4004002264646466e200200054ccc0eccdc3a400460786ea8cc0dcc0e40152002100113370000290011bad303f0013037002153330393370e90020008a5114a0606e00266066606a00290001804000998189819998189819801240009007111919299981b19b87480000044c8c8c8c94ccc0e8cdc3a4008002264646466e952000330410024bd70181b8009820000981c0010a60103d87a80003038001330343036330343036001480092004303c001303400214c0103d87a80003034001323300400123371e6eb8cc0c8c0d0cc0c8c0d0cc0c8c0d0005200048001200000337586606060646606060640049000240006002002444a66606c0042980103d87a8000132325333035300300213374a90001981c80125eb804ccc01401400400cc0e800cc0e000888c8c8cc0a00048c94ccc0e0004528099299981c8008991919299981c99b87480100044c8c94ccc0eccdc3a40006074002264646464a666084608a0042646606a002466e20c8c8c8c94ccc114cdc3a4004608c6ea8cc104c10c00d2002100113370000290011bad304900130413253330443370e9001182180088008b1981f9820800a400060280026607a607e6607a607e02490002401c00a660689448cc0c804400858dd7182180098218011bad3041001303900116303f001303700214a0606e00266066606a0029002181d8010a50303b00132330210012323253330383370e9002000899251303600214a0606c00266064606800290021bac3303030323303030320054800120043302700123232323232323232533303e304100213232323232325333044304700213232323303900123375e01600a66070002466602e0020060126606e9448ccc058004020008c8c068004ccc0a00112f5bded8c003c2c608a002646054002660560020166eb0c100008dd6181f00099191816000998168008041bac33038303a00948010c8c050004ccc0880112f5bded8c00302c607e0026460480026604a00200a6eb0c0e8008dd6181c000991918130009981380091918020009981a181b000a40046eb0cc0c8c0d000d20002337606e994ccc0d8c8c00c004cc0c8c0d000520001375666064606800290010a5eb7bdb180cc0c8c0d000520002323253330373370e900100089925130350021324a0606a004606a0026606260660029000198179818802240006604c9448cc09000c0088cc88c94ccc0c8cdc3a400000220062a66606466e1d20020011003100230303253330323370e90000008a60103d8798000153330323370e90020008a6103d87b80001533303253330323370e900118199baa3302e3030003480084cdc3a400460666ea8cc0b8c0c0009200213330323370e900118199baa3302e3030002480092825114c0103d87a8000153330323370e900118199baa3302e303000348008530103d87b800014c103d8798000303033223253330343370e9000000899299981a99b8748000004530103d87a800014c103d87980003033002153330343370e9002000899299981a99b8748010004530103d87a800014c103d87b80003033002132323253330373370e90000008a60103d87b8000153330373370e90020008a6103d8798000132325333039337100080022980103d8798000153330393370e0080022980103d87a800014c103d87b8000375a607a002606a00a606a0086eb4c0e8004c0c800cc0c8008cc0b4c0bc00920003302d302f00148000cc0acc0b400520003302b302d00148008888cc0180088cc014008004c0040048894ccc0c400852809919299981819baf374e0046e9c00c5288999802802800801981a8019bac30330023001001222533302f00214a226464a66605c6006004266600a00a0020062940c0cc00cdd618188011199802000a5eb80888ccc014008004888cc0ccdd3998199ba9006330333752006660666ea00092f5c000260020024444a66605a006200426464646466600a002004666601001000600c00a6eb8c0b800cdd6981700118188021817801980080091112999815801880109919191919980280080119998040040018030029bae302c00337566058004605e008605a00644666601000400244464a666052a66605800229445280a60103d87a800013374a9000198169ba60014bd70199980380100091119299981619b87001480005300103d87a800013374a9000198181ba80014bd7019b8000200100600322533302433720004002298103d8798000153330243371e0040022980103d87a800014c103d87b800030010012222253330280041003132323232333330090090033333300a007001002006005006005375c60520066eb4c0a4008c0b0014c0a8010c0040048888894ccc09c0144cc0a0cdd81ba9004375000697adef6c6013232323253330283375e6600a01000298103d879800013302c337606ea4020dd40038048a99981419b8f00800113232533302a3370e9000000899191981819bb037520186ea000401cdd6981800098140010802981400099980300400380109981619bb037520026ea0008cccccc02802800c02001c018014dd718140019bad3028002302b006302900530010012222253330240041003132323232333330090090033333300a007001002006005006005375c604a0066eacc094008c0a0014c098010c0040048888894ccc08c0144cc090cdd81ba9004374c00697adef6c6013232323253330243375e6600a01000298103d8798000133028337606ea4020dd30038048a99981219b8f0080011323253330263370e9000000899191981619bb037520186e9800401cdd5981600098120010802981200099980300400380109981419bb037520026e98008cccccc02802800c02001c018014dd718120019bab302400230270063025005222333004003002223300300100230010012222533301e003100213232333300600600133003002004003302200437566040006600200244a666034002297ae0132323301d0023300400433005001233301a3375e00200694128980f001180e0009800800911299980c8010a5eb804c8c94ccc060c00c0084cc070008ccc01401400400c4ccc01401400400cc07400cc06c008c004004894ccc05800452f7b630101800001018000132323232323232337606e9ccc078dd30020011ba73301e005001375860360046eb0c064004cc01801800cdd5980b801980b801180d001180c0009800800911299980a8010a5eb7bdb1804c8c8cc060c00c008ccc01401400400cc06400cc05c00888c8cc010004008dd6198061807198061807001240009008180080091129998090010a501323253330113371e00400629444ccc01401400400cc05800cdd7180a00111299980680109800a4c294000400800c526163200530050043200332533300a3370e90000008a99980698040018a4c2c2a66601466e1d20020011533300d300800314985858c0200088c94ccc01ccdc3a4000002264646464a66601c60220042930b1bae300f001300f002375a601a002600a0042a66600e66e1d200200113232533300c300f002149858dd7180680098028010b180280099800800a40004444666600e66e1c00400c0308cccc014014cdc000224004601c0020040044600a6ea80048c00cdd5000ab9a5573aaae7955cfaba05742ae89",
  };

  const alwaysTrue: MintingPolicy = {
    type: "PlutusV2",
    script:
      "58635861010000323232323232322253330053370e900018031baa001153330054a22930b09912999803a51149858c020c01cdd500099800800a40004444666600a66e1c00400c0208cccc014014cdc0002240046014002004004ae6955ceaab9e5742ae89",
  };
  const alwaysTruePolicy = lucid.utils.validatorToScriptHash(alwaysTrue);
  const alwaysTrueToken1 = `${alwaysTruePolicy}${fromText("Token1")}`;
  const alwaysTrueToken2 = `${alwaysTruePolicy}${fromText("Token2")}`;
  const voidData = Data.to(new Constr(0, []));

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

          .attachMintingPolicy(alwaysTrue)
          .mintAssets(
            {
              [alwaysTrueToken1]: BigInt(3),
              [alwaysTrueToken2]: BigInt(4),
            },
            voidData
          )

          .payToContract(
            morbidAddress,
            { inline: createChest, scriptRef: morbidScript },
            {
              lovelace: BigInt(42_000000),
              [alwaysTrueToken1]: BigInt(1),
              [alwaysTrueToken2]: BigInt(1),
            }
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
            {
              lovelace: BigInt(42_000000),
              [alwaysTrueToken1]: BigInt(1),
              [alwaysTrueToken2]: BigInt(1),
            }
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

        // let treasure = 0;
        const chestUTxOs: UTxO[] = [];
        const refScript: UTxO[] = [];
        let assets: Assets = {};

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
                  for (const asset in utxo.assets) {
                    if (assets[asset]) {
                      assets[asset] += utxo.assets[asset];
                    } else {
                      assets[asset] = utxo.assets[asset];
                    }
                  }
                }
              }
              return;

            case 1: // AddTreasure
              const refTxn = datumFields[0];
              const refTxnValue = refTxn["bytes"];
              if (refTxnValue === chest.txHash) {
                chestUTxOs.push(utxo);
                for (const asset in utxo.assets) {
                  if (assets[asset]) {
                    assets[asset] += utxo.assets[asset];
                  } else {
                    assets[asset] = utxo.assets[asset];
                  }
                }
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

        console.log({ assets: assets });
        const tx = await lucid
          .newTx()
          .collectFrom(chestUTxOs, delayUnlock)
          .readFrom(refScript) // reference script
          .addSigner(userAddress)

          // normal case:
          .payToContract(
            morbidAddress, // re-create chest
            { inline: createChest, scriptRef: morbidScript },
            assets
          )

          // drain by recreate chest to another address:
          //   .payToAddressWithData(
          //     userAddress, // drain to any address
          //     { inline: createChest },
          //     assets
          //   )

          // drain by recreate chest to another address with multiple txOuts:
          //   .payToContract(
          //     morbidAddress, // re-create chest
          //     { inline: createChest, scriptRef: morbidScript },
          //     { lovelace: BigInt(0) }
          //   )
          //   .payToAddress(
          //     userAddress, // drain to any address
          //     assets
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
