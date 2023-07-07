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
      "5909a159099e0100003232323232323232323232322223232533300b333323232323232323232323232323232323232323232323232322223232323232533302c3370e900018158010a99981619b8748000c0ac00c4c8c8c94ccc0bccdc3a4004605c00e26466022014004605a00e0086eb8c0cc004c0cc004c0a800c0040044c94ccc0b0cdc3a400060560042a66605866e1d2002302b00313232533302e3370e90011816803099191919299981919b87480000084c8c94ccc0d0cdc3a400060660022646464660300220026eb8c0ec004c0ec004c0c800400cc0e0004c0c000c00452818178009980680480118160030019bae3032001302a003001001132533302c3370e900118158010a99981619b8748000c0ac00c4c8c94ccc0b8cdc3a4004605a00c26466018012004605800c0066eb4c0c8004c0a800c0040044c94ccc0b0cdc3a400460560042a66605866e1d2002302b00313232533302e3370e90011816803099191919299981919b87480000084c8c94ccc0d0cdc3a4000606600226464660260200026eb4c0e8004c0c800400cc0e0004c0c000c00452818178009980680480118160030019bae3032001302a00300100114a0605e006605c605e004605a605c605c00266056004660560066605666046604a002900125eb8088c8c8c8c94ccc0a4cdc3a4004002264646466e200200054ccc0accdc3a400460586ea8cc09cc0a40152002100113370000290011bad302f0013027002153330293370e90020008a5114a0604e00266046604a00290001804000998109811998109811801240009007111919299981319b87480000044c8c8c8c94ccc0a8cdc3a4008002264646466e952000330310024bd701813800981800098140010a60103d87a80003028001330243026330243026001480092004302c001302400214c0103d87a80003024001323300400123371e6eb8cc088c090cc088c090cc088c090005200048001200000337586604060446604060440049000240006002002444a66604c0042980103d87a8000132325333025300300213374a90001981480125eb804ccc01401400400cc0a800cc0a000888c8c8cc0600048c94ccc0a000452809929998148008991919299981499b87480100044c8c94ccc0accdc3a40006054002264646464a666064606a0042646604a002466e20c8c8c8c94ccc0d4cdc3a4004606c6ea8cc0c4c0cc00d2002100113370000290011bad303900130313253330343370e9001181980088008b198179818800a400060280026605a605e6605a605e02490002401c00a660489448cc08804400858dd7181980098198011bad3031001302900116302f001302700214a0604e00266046604a002900218158010a50302b001323300d0012323253330283370e9002000899251302600214a0604c00266044604800290021bac3302030223302030220054800120043301700123232323232323232533302e303100213232323232325333034303700213253330323370e00e002266ebc020008528180a0020b181a80099180b0009980b8008059bac30300023758605c00264646030002660320020106eb0cc0a0c0a80252004300e00416302f00132301000133011001005375860540046eb0c0a0004c8c8c048004cc04c0048c8c010004cc090c098005200237586604460480069000119bb03750a66604c6460060026604460480029000099191919299981519b874800800452000132375a60600026050004605000264a66605266e1d200200114c0103d87a80001323233019001489003756605e002604e004604e0026602e00291100375666044604800290010a4000660446048002900011919299981399b87480080044c92898128010992503025002302500133021302300148000cc07cc0840112000330164a24660280060044664464a66604466e1d20000011003153330223370e900100088018801181019299981119b8748000004530103d8798000153330223370e90020008a6103d87b80001533302253330223370e900118119baa3301e3020003480084cdc3a400460466ea8cc078c080009200213330223370e900118119baa3301e3020002480092825114c0103d87a8000153330223370e900118119baa3301e302000348008530103d87b800014c103d8798000302033223253330243370e9000000899299981299b8748000004530103d87a800014c103d87980003023002153330243370e9002000899299981299b8748010004530103d87a800014c103d87b80003023002132323253330273370e90000008a60103d87b8000153330273370e90020008a6103d8798000132325333029337100080022980103d8798000153330293370e0080022980103d87a800014c103d87b8000375a605a002604a00a604a0086eb4c0a8004c08800cc088008cc074c07c00920003301d301f00148000cc06cc07400520003301b301d001480088ccc0080052000223370000200460020024444a666044006200426464666600c00c00266006004008006604c0086eb4c09000cc004004894ccc07800452f5c02646466042004660080086600a002466603c66ebc00400d28251302200230200013001001222533301d00214bd7009919299980e180180109981000119980280280080189998028028008019810801980f801180080091299980d0008a5ef6c6101800001018000132323232323232337606e9ccc088dd40020011ba7330220050013758603e0046eb0c074004cc01801800cdd6980d801980d801180f001180e0009800800911299980c8010a5eb7bdb1804c8c8cc070c00c008ccc01401400400cc07400cc06c008c0040048894ccc05c0085300103d87a800013232323253330183371e00a002266e9520003301c375000497ae01333007007003005375c60300066eb4c060008c06c00cc064008c0040048894ccc054008530103d87a800013232323253330163371e00a002266e9520003301a374c00497ae01333007007003005375c602c0066eacc058008c06400cc05c00888c8cc010004008dd6198061807198061807001240009008180080091129998090010a501323253330113371e00400629444ccc01401400400cc05800cdd7180a00111299980680109800a4c294000400800c526163200530050043200332533300a3370e90000008a99980698040018a4c2c2a66601466e1d20020011533300d300800314985858c0200088c94ccc01ccdc3a4000002264646464a66601c60220042930b1bae300f001300f002375a601a002600a0042a66600e66e1d200200113232533300c300f002149858dd7180680098028010b180280099800800a40004444666600e66e1c00400c0308cccc014014cdc000224004601c0020040044600a6ea80048c00cdd5000ab9a5573aaae7955cfaba05742ae881",
  };

  const alwaysTrue: MintingPolicy = {
    type: "PlutusV2",
    script:
      "58635861010000323232323232322253330053370e900018031baa001153330054a22930b09912999803a51149858c020c01cdd500099800800a40004444666600a66e1c00400c0208cccc014014cdc0002240046014002004004ae6955ceaab9e5742ae89",
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

          .attachMintingPolicy(alwaysTrue)
          .mintAssets(
            {
              [`${alwaysTrue}${fromText("NFT")}`]: BigInt(1),
              [`${alwaysTrue}${fromText("Token")}`]: BigInt(2),
            },
            Data.to(new Constr(0, []))
          )

          .payToContract(
            morbidAddress,
            { inline: createChest, scriptRef: morbidScript },
            { lovelace: BigInt(42_000000), nft: BigInt(1) }
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
              [`${alwaysTrue}${fromText("Token")}`]: BigInt(1),
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
                  assets = { ...assets, ...utxo.assets };
                }
              }
              return;

            case 1: // AddTreasure
              const refTxn = datumFields[0];
              const refTxnValue = refTxn["bytes"];
              if (refTxnValue === chest.txHash) {
                chestUTxOs.push(utxo);
                assets = { ...assets, ...utxo.assets };
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
          //     { lovelace: BigInt(treasure) }
          //   )

          // drain by recreate chest to another address with multiple txOuts:
          //   .payToContract(
          //     morbidAddress, // re-create chest
          //     { inline: createChest, scriptRef: morbidScript },
          //     { lovelace: BigInt(0) }
          //   )
          //   .payToAddress(
          //     userAddress, // drain to any address
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
