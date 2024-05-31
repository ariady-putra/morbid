import { Constr, Data, Lucid, MintingPolicy, SpendingValidator, UTxO, applyDoubleCborEncoding, applyParamsToScript, fromText } from "lucid-cardano";
import { useEffect, useState } from "react";
import { getAssetMetadata, getLatestBlockInfo, getScriptCBOR, getTxMetadata, getTxUTxOs } from "../utils/blockfrost";

declare type TokenUTxO = {
  utxo: UTxO;
  initial_mint_tx_hash: string;
  policyID: string;
  assetName: string;
  metadata: any;
};

const MorbidV2 = (props: { lucid: Lucid; setActionResult: (actionResult: string) => void }) => {
  const lucid = props.lucid;
  const setActionResult = props.setActionResult;

  const [loaded, setLoaded] = useState(false);
  const [chestAddress, setChestAddress] = useState("");
  const [resendableChest, setResendableChest] = useState<TokenUTxO>(); // for resending chest lock
  const [keyTokenUTxO, setKeyTokenUTxO] = useState<TokenUTxO>(); // for delaying chest deadline

  useEffect(() => {
    lookForChestAddress();
    lookForResendableChest();
    setLoaded(true);
  }, []);

  const lookForChestAddress = async () => {
    const key = await lookForToken(chestKey.name);
    setKeyTokenUTxO(key);
    setChestAddress(key?.metadata["chest_address"]);
  };

  const lookForResendableChest = async () => {
    setResendableChest(await lookForToken(chestLock.name));
  };

  const lookForToken = async (name: string) => {
    let result: TokenUTxO | undefined;
    if (lucid) {
      const utxos = await lucid.wallet.getUtxos();
      console.log({ utxos: utxos });
      if (!utxos.length) {
        throw { emptyWalletAddress: "No UTxO to look for." };
      }

      const forEachAsync = async (utxos: UTxO[], callback: { (utxo: UTxO): Promise<void>; (arg0: any): any }) => {
        for (const utxo of utxos) {
          await callback(utxo);
        }
      };

      let foundToken = false;
      const myAsyncCallback = async (utxo: UTxO) => {
        if (foundToken) return;
        let hasToken = false;
        for (const asset in utxo.assets) {
          try {
            if (asset === "lovelace") continue;
            // console.log({ asset: asset, qty: utxo.assets[asset] });
            const assetMetadata = await getAssetMetadata(asset);
            console.log({ assetMetadata: assetMetadata });

            const initial_mint_tx_hash = assetMetadata["initial_mint_tx_hash"];
            const policyID = assetMetadata["policy_id"];
            const assetName = fromText(name);

            if (asset.substring(policyID.length) !== assetName) continue;

            const onchainMetadata = assetMetadata["onchain_metadata"] || (await getTxMetadata(initial_mint_tx_hash))["json_metadata"][policyID][assetName];
            console.log({ onchainMetadata: onchainMetadata });
            result = {
              utxo: utxo,
              initial_mint_tx_hash: initial_mint_tx_hash,
              policyID: policyID,
              assetName: assetName,
              metadata: onchainMetadata,
            };
            hasToken = true;
            break;
          } catch (x) {
            setActionResult(JSON.stringify(x));
          }
        }
        if (hasToken) {
          foundToken = true;
        }
      };
      await forEachAsync(utxos, myAsyncCallback);
    }
    return result;
  };

  const compiledCode = {
    mintChest:
      "590644010000323232323232322232232322533300832323232533300c3007300d3754002264646464a66602600229404c94ccc0500045280a99980a180b80089919299980999198008009bac300b301637546016602c6ea8034894ccc060004528099299980b19baf300d30183754603600402a29444cc00c00c004c06c00454ccc04cc8c8cc004004cc064010cc06400d2f5bded8c044a66603200229404c94ccc05ccdd79ba63301b301c0024bd6f7b6301ba63301b0044bd6f7b6300a51133003003001301c001337606ea4041300101010015333013323300100133018003330180024bd6f7b63011299980c0008a5013253330163371e6eb8c05cc06c00922010843686573744b65790014a22660060060026036002264646464a666034603a004266e20c8c94ccc064c030c068dd5000899299980d19299980f180e8008a99980d9807180e0008a511533301b3016301c00114a02c2c6ea8c048c070dd50018800899b8000148008dd6980f180d9baa00116300f301a375400264664464a666036602c00220062a666036601c0022006200460366ea8c94ccc06cc0580045300103d87980001533301b300700114c0103d87b80001533301b533301b32533301f301e0011533301c300f301d00114a22a666038602e603a00229405858dd51809980e9baa003132533301f301e0011533301c300f301d00114a22a666038602e603a00229405858dd51809980e9baa002133301b32533301f301e0011533301c300f301d00114a22a666038602e603a00229405858dd51809980e9baa0024a09445300103d87a80001533301b32533301f301e0011533301c300f301d00114a22a666038602e603a00229405858dd51809980e9baa00314c103d87b800014c103d8798000301b3754664464a66603a60300022a66603a6030603c6ea80085300103d87a800014c103d87980001533301d30090011533301d3009301e3754004298103d87a800014c103d87b800013232533301f301a00114c0103d87b80001533301f300b00114c0103d87980001325333020337100060022980103d8798000153330203370e0060022980103d87a800014c103d87b8000375a604860426ea8010c07cdd50019bad3022301f3754006603a6ea8008c044c070dd50011808980e1baa001300f301a3754002602060346ea8004c070c074c074c074c074c074c074c074c064dd50021bad301c301d001163758603600264660020026eb0c030c060dd500191299980d0008a5eb804c8c94ccc064c030c068dd500089980200200109980e980f180d9baa001330040040023232533301a3015301b37540022980103d87a800013232533301f00114c0103d87a80001533301f3022001132533301d3009301e37540022601e660426e9ccc084dd4801998109811180f9baa0014bd7025eb80530103d87a80003012301e37540082980103d87a8000323300100137566028603c6ea8010894ccc08000452f5c0264666444646600200200644a66604c002200626466644464a666050603660526ea800440084cc0b0c0b4c0a8dd50008012999813a99981399b8f00901b153330273371e0060482603400429405280980c998159ba73302b3752012660566ea400ccc0ad3010101004bd7025eb805300103d87a8000375c604a0026eb4c098004cc00c00cc0a8008c0a0004dd7180f8009bab30200013300300330240023022001375c603e60386ea8004c040c06cdd51808180d9baa001301d002301d001370e90021805180a9baa00c14a02940528180b801180b0010a503016001325333010300330113754002297adef6c6013756602a60246ea8004c8cc004004c8cc004004dd5980b180b980b980b980b98099baa30083013375401444a66602a002297adef6c60132323232533301633722910100002153330163371e9101000021003100513301a337606ea4008dd3000998030030019bab3017003375c602a0046032004602e00244a666028002298103d87a80001323232325333015337220120042a66602a66e3c0240084c01ccc064dd3000a5eb80530103d87a80001330060060033756602c0066eb8c050008c060008c058004dd2a40006e1d2002375c6022601c6ea800458c00cc034dd50021180818089808800918078009180718078008a4c26cac64a66600e600460100022a66601460120022930b0b1baa002370e90001bae0015734aae7555cf2ab9f5740ae855d11",
    validate:
      "590bf001000032323232323232232232223232322533300b3232323232323232323232323232323232323232533301f301730203754602860426ea805454ccc07cc060c080dd500c8a99980f9919191919191929998149816001099191919192999817181880109929998161980b0030008a9998161980b000803099baf00700214a02940c050ccc04800d2f5bded8c00262c605e002601c660346eb0c0b8c0bc008024dd618168009805198049bac30163029375401000c601e66601a00697adef6c6000e16302a0013009330153758605260540040086eb0c0a0004c014cc010dd6180b18121baa0032300230183025375400246604c6e994ccc088c008c058c090dd500089bab301730243754002297adef6c603302630163024375400297ae025333021301930223754602a60466ea80045288a5030133021375402a2646464a66604a6050004264646464a6660526058004264a66604ea66604e66e3c014dd71816001099b8800400114a0264a66605600229404c8cc004004008894ccc0b4004528899299981599b8848000c0b0dd518180010998018018008a50303000132330010013758603a60546ea8024894ccc0b000452f5c026464a666056604660586ea80044cc0100100084cc0bcccc0accdc42400060586ea8c0c0c0b4dd5000a6103d87a80004c0103d87980003300400400232533302e3301b37566040605a6ea80048894ccc0b94ccc0b8cdc7801816099b8f00248810843686573744b65790014a026036660646e9ccc0c8dd4801998191ba900233032375000297ae04bd700a60103d87a800014c103d87a8000130183302f33302b3322323300100100322533303200114a0264a66606066e3cdd7181a8010020a5113300300300130350013758606060626062606260626062606260626062605a6ea8030c94ccc0b0c094c0b4dd500089bae3031302e375400226eb8c0c4c0b8dd5000980f98169baa301f302d3754002980103d87a80004c0103d87980004bd70180f98161baa302f002302f00114a06eb4c0acc0b000458dd61815000998079bac30133026375400a46660220020460426eb4c0a0c0a4008dd718138008b1bac30260013300b3758602860446ea80048ccc034c058c08cdd500080f80e980998109baa01514a026464a666042603460446ea80044c94ccc088c040c08cdd5000899b88375a604e60486ea8004c8c94ccc090c070c094dd5000899299981299299981498140008a999813180f18138008a5115333026301f302700114a02c2c6ea8c068c09cdd50018800899b8000148008dd6981498131baa0011630173025375400264664464a66604c603e00220062a66604c603c00220062004604c6ea8c94ccc098c07c0045300103d879800015333026301400114c0103d87b800015333026533302632533302a302900115333027301f302800114a22a66604e6040605000229405858dd5180d98141baa003132533302a302900115333027301f302800114a22a66604e6040605000229405858dd5180d98141baa002133302632533302a302900115333027301f302800114a22a66604e6040605000229405858dd5180d98141baa0024a09445300103d87a80001533302632533302a302900115333027301f302800114a22a66604e6040605000229405858dd5180d98141baa00314c103d87b800014c103d879800030263754664464a66605060420022a666050604260526ea80085300103d87a800014c103d879800015333028301600115333028301630293754004298103d87a800014c103d87b800013232533302a302300114c0103d87b80001533302a301800114c0103d8798000132533302b337100060022980103d87980001533302b3370e0060022980103d87a800014c103d87b8000375a605e60586ea8010c0a8dd50019bad302d302a375400660506ea8008c064c09cdd5001180c98139baa0013017302537540026030604a6ea8004c09cc0a0c0a0c0a0c0a0c0a0c0a0c0a0c090dd50018b180818119baa301630233754604c60466ea800458c8cc004004dd6180a98119baa00222533302500114c0103d87a8000132325333024301c325333025301d302637540022900009bad302a3027375400264a66604a603a604c6ea80045300103d87a8000132330010013756605660506ea8008894ccc0a8004530103d87a8000132323232533302b3372204e0042a66605666e3c09c0084c060cc0bcdd4000a5eb80530103d87a8000133006006003375a60580066eb8c0a8008c0b8008c0b0004c8cc004004dd5980d18139baa301a3027375400644a666052002298103d87a8000132323232533302a337220500042a66605466e3c0a00084c05ccc0b8dd3000a5eb80530103d87a8000133006006003375660560066eb8c0a4008c0b4008c0ac0044c044cc0a00092f5c02660080080026052004604e002602660426ea80545888c8cc00400400c894ccc09400452f5c026604c6e9cc00cc09c004cc008008c0a0004c004004894ccc08400452f5c210180008101800009919198121ba73302430250023758604a002660486e9ccc090c094c098008dd618129813000a5eb80cc00c00cc094008dd61811800980080091299980f8008a5eb804c8cc084004cc00c00ccc038c08c0088ccc078cdd78008012504a2604200244464666002002008006444a66604400420022666006006604a004660080026eacc09000888c8ccc00400400c0088894ccc08000840044c8ccc010010c09000ccc88c8cc004004014894ccc0940044cc098cdd81ba9004374c00697adef6c6013232323253330263372001000426605466ec0dd48041ba6007005153330263371e010004264a66604e604060506ea80044cc0accdd81ba9009302c30293754002008200864a66604ea66605400229445280a6103d87a8000130143302b374c00297ae032333001001008002222533302c0021001132333004004303000333223233001001005225333031001133032337606ea4010dd4001a5eb7bdb1804c8c8c8c94ccc0c8cdc800400109981b19bb037520106ea001c01454ccc0c8cdc780400109929998199816181a1baa001133037337606ea4024c0e0c0d4dd5000802080219299981998160008a60103d87a80001302033037375000297ae03370000e00226606c66ec0dd48011ba800133006006003375a60660066eb8c0c4008c0d4008c0cc004dd718158009bad302c001302e00213302a337606ea4008dd3000998030030019bab3027003375c604a0046052004604e0026eb8c07c004dd5981000098110011199805000a5eb80888ccc038008004888cc088dd3998111ba9006330223752006660446ea00092f5c000244646600200200644a66603a00229444c94ccc06cc8c8cc004004018894ccc084004528099299980f99baf3024002374e00829444cc00c00c004c090004dd618100010998018018008a50302000122323300100100322533301c00114bd7009919299980d9809980e1baa00113300400400213301f3020301d3754002660080080046008603e004603e00244464a666030602260326ea8004530103d87a800013232533301d00114c0103d87a80001533301d3020001132533301b3009301c3754002260106603e6e9ccc07cdd48019980f9810180e9baa0014bd7025eb80530103d87a80003009301c375400c2980103d87a8000330093756601c60366ea80148894ccc0714ccc070cdc78018038a99980e19b8f0020061301400114a029404c024cc080dd3998101ba90033302037520046604098010101004bd7025eb805300103d87a8000375c603a60346ea8004c02cc064dd51805980c9baa003374a90001b87480108c05cc060c06000488ccc0100092f5c044466601000400244464a666032602260346ea800440084cc074c078c06cdd500080119980380300180111191980080080191299980b0008a5eb804c8c94ccc054c0140084cc064008cc0100100044cc010010004c068008c060004888c8cc004004010894ccc05800440104c8ccc010dd7180a8009bab301600133003003301a002301800122232330010010042253330150011004132333004375c60280026eb4c054004cc00c00cc064008c05c0048c0480048c044c0480045261365632533300a30030011533300d300c37540082930b0a99980518010008a99980698061baa00414985858c028dd50019b8748008dc3a40006eb8004dd7000ab9a5573aaae7955cfaba05742ae89",
  };

  const alwaysTrue: MintingPolicy = {
    type: "PlutusV2",
    script:
      "58635861010000323232323232322253330053370e900018031baa001153330054a22930b09912999803a51149858c020c01cdd500099800800a40004444666600a66e1c00400c0208cccc014014cdc0002240046014002004004ae6955ceaab9e5742ae89",
  };
  const alwaysTruePolicy = lucid.utils.validatorToScriptHash(alwaysTrue);
  const alwaysTrueToken1nameStr = "Gold Medallion";
  const alwaysTrueToken1nameHex = fromText(alwaysTrueToken1nameStr);
  const alwaysTrueToken1 = `${alwaysTruePolicy}${alwaysTrueToken1nameHex}`;
  const alwaysTrueToken1metadata = {
    name: alwaysTrueToken1nameStr,
    image: "ipfs://QmbkhhMrYKVUnEufyTGkUwcTWA61Zj9u7Mf2f9iPSw7QBT",
  };
  const alwaysTrueToken2nameStr = "Black Pearl";
  const alwaysTrueToken2nameHex = fromText(alwaysTrueToken2nameStr);
  const alwaysTrueToken2 = `${alwaysTruePolicy}${alwaysTrueToken2nameHex}`;
  const alwaysTrueToken2metadata = {
    name: alwaysTrueToken2nameStr,
    image: "ipfs://QmUZcPgZXPn1emtijgFJMwkFswXE7mwtEXR6mraCYnrASn",
  };

  const voidData = Data.to(new Constr(0, []));

  const action = {
    delayUnlock: Data.to(new Constr(0, [])),
    unlockChest: Data.to(new Constr(1, [])),
  };

  const chestKey = {
    name: "ChestKey",
    image: "ipfs://QmdvFVHauVFw4XTxQMHfsQy4g4zNsxHaNDv1mMw1Fbt3PY",
  };

  const chestLock = {
    name: "Dead Man's Chest",
    image: "ipfs://QmavSMgKNtRFu4wXCucUbC4dNDqwN4TDfBoW6sk7kxj9tE",
  };

  const mintTreasures = async () => {
    try {
      console.log("MintTreasures():");
      if (lucid) {
        const tx = await lucid
          .newTx()
          .attachMintingPolicy(alwaysTrue)
          .mintAssets(
            {
              [alwaysTrueToken1]: BigInt(5),
              [alwaysTrueToken2]: BigInt(5),
            },
            voidData
          )
          .attachMetadata(721, {
            [alwaysTruePolicy]: {
              [alwaysTrueToken1nameHex]: {
                name: alwaysTrueToken1metadata.name,
                image: alwaysTrueToken1metadata.image,
              },
              [alwaysTrueToken2nameHex]: {
                name: alwaysTrueToken2metadata.name,
                image: alwaysTrueToken2metadata.image,
              },
            },
          })
          .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log({ txHash: txHash });
        return txHash;
      }
      throw { error: "Invalid Lucid State!" };
    } catch (x) {
      console.log(x);
    }
  };

  const createChest = async () => {
    try {
      console.log("CreateChest():");
      if (lucid) {
        const userAddress = await lucid.wallet.address();
        console.log({ userAddress: userAddress });

        const utxos = await lucid.wallet.getUtxos();
        console.log({ utxos: utxos });
        if (!utxos.length) {
          throw { emptyWalletAddress: "No UTxO to consume." };
        }

        const chestHexName = fromText(chestLock.name);
        const chestKeyHexName = fromText(chestKey.name);
        console.log({ lock: chestHexName, key: chestKeyHexName });

        // validator params of `morbid.validator.mint_chest` script
        const mintChestParams = {
          utxoRef: new Constr(0, [
            new Constr(0, [String(utxos[0].txHash)]), // TxHash
            BigInt(utxos[0].outputIndex), // TxIndex
          ]), // OutputReference
          chestName: chestHexName, // AssetName
        };
        const mintChest = applyParamsToScript(compiledCode.mintChest, [mintChestParams.utxoRef, mintChestParams.chestName]);
        const mintChestScript: MintingPolicy = {
          type: "PlutusV2",
          script: applyDoubleCborEncoding(mintChest), // cborHex
        };
        const mintChestScriptHash = lucid.utils.validatorToScriptHash({
          type: "PlutusV2",
          script: mintChest,
        }); // used as `policy_id`

        const mintedLockName = `${mintChestScriptHash}${chestHexName}`;
        const mintedKeyName = `${mintChestScriptHash}${chestKeyHexName}`;

        // validator params of `morbid.validator.validate` script
        const chestParams = {
          policyId: mintChestScriptHash, // PolicyId
          chestName: chestHexName, // AssetName
        };
        const chest = applyParamsToScript(compiledCode.validate, [chestParams.policyId, chestParams.chestName]);
        const validateScript: SpendingValidator = {
          type: "PlutusV2",
          script: applyDoubleCborEncoding(chest), // cborHex
        };
        const chestAddress = lucid.utils.validatorToAddress({
          type: "PlutusV2",
          script: chest,
        });
        console.log({ chestAddress: chestAddress });

        const block = await getLatestBlockInfo();
        const time = block["time"] * 1_000; // ms
        console.log({ time: time });

        const deadline = BigInt(time + 60_000); // +1min
        const datum = Data.to(deadline);
        console.log({ datum: datum });

        const tx = await lucid
          .newTx()
          .collectFrom(utxos)
          .attachMintingPolicy(mintChestScript)
          .mintAssets({ [mintedLockName]: BigInt(1), [mintedKeyName]: BigInt(2) }, voidData)
          .payToContract(
            chestAddress,
            { inline: datum, scriptRef: validateScript },
            {
              lovelace: BigInt(42_000000),
              [mintedLockName]: BigInt(1),
              [alwaysTrueToken1]: BigInt(1),
              [alwaysTrueToken2]: BigInt(1),
            }
          )
          .attachMetadata(721, {
            [mintChestScriptHash]: {
              [chestHexName]: {
                name: chestLock.name,
                image: chestLock.image,
              },
              [chestKeyHexName]: {
                name: `${chestLock.name} - ${chestKey.name}`,
                image: chestKey.image,
                chest_address: chestAddress,
                // ref_hash: utxos[0].txHash, // not needed
                // ref_index: utxos[0].outputIndex,
              },
            },
          })
          .validFrom(time)
          .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log({ txHash: txHash });

        setActionResult(`TxHash: ${txHash}`);
        window.location.reload();
        return txHash;
      }
      throw { error: "Invalid Lucid State!" };
    } catch (x) {
      setActionResult(JSON.stringify(x));
    }
  };

  const addTreasure = async () => {
    try {
      console.log(`AddTreasure(${chestAddress}):`);
      if (lucid) {
        // await lookForChestAddress(); // setChestAddress already set

        if (!chestAddress?.length) {
          throw { noChestKey: "No ChestKey was found." };
        }

        const tx = await lucid
          .newTx()
          .payToContract(
            chestAddress,
            { inline: voidData }, // still need to put arbitrary INLINE Datum to be redeemable
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
      console.log(`DelayUnlock(${chestAddress}):`);
      if (lucid && keyTokenUTxO) {
        const userAddress = await lucid.wallet.address();
        console.log({ userAddress: userAddress });

        const lock = (await lucid.utxosAt(chestAddress)).find((utxo) => utxo.datum && utxo.scriptRef);
        console.log({ lock: lock });
        if (!lock) {
          throw { noChestLock: "No ChestLock was found." };
        }

        const block = await getLatestBlockInfo();
        const time = block["time"] * 1_000; // ms
        console.log({ time: time });

        const deadline = BigInt(time + 60_000); // +1min
        const datum = Data.to(deadline);
        console.log({ datum: datum });

        const lockName = `${keyTokenUTxO.policyID}${fromText(chestLock.name)}`;
        console.log({
          [`${chestLock.name} - ChestLock`]: {
            lovelace: lock.assets["lovelace"],
            [lockName]: lock.assets[lockName],
          },
        });

        const tx = await lucid
          .newTx()
          .readFrom([lock, keyTokenUTxO.utxo])
          .collectFrom([lock], action.delayUnlock)

          // normal case:
          .payToContract(chestAddress, { inline: datum, scriptRef: lock.scriptRef! }, lock.assets)

          // invalid datum:
          // .payToContract(
          //   chestAddress,
          //   { inline: voidData, scriptRef: lock.scriptRef! },
          //   lock.assets
          // )

          // drain by resend chest to another address:
          // .payToAddressWithData(
          //   userAddress, // drain to any address
          //   { inline: datum, scriptRef: lock.scriptRef! },
          //   lock.assets
          // )

          // drain `ChestLock` UTxO:
          // .payToContract(
          //   chestAddress, // just resend the lock only
          //   { inline: datum, scriptRef: lock.scriptRef! },
          //   { [lockName]: BigInt(lock.assets[lockName]) }
          // )
          // .payToAddress(
          //   userAddress, // drain ADA to any address
          //   { lovelace: BigInt(lock.assets["lovelace"]) }
          // )

          // take-out treasures:
          //   .payToContract(
          //     chestAddress, // just resend the lock and ADA
          //     { inline: datum, scriptRef: lock.scriptRef! },
          //     {
          //       lovelace: BigInt(lock.assets["lovelace"]),
          //       [lockName]: BigInt(lock.assets[lockName]),
          //     }
          //   )
          //   .payToAddress(
          //     userAddress, // steal treasures
          //     {
          //       [alwaysTrueToken1]: BigInt(lock.assets[alwaysTrueToken1]),
          //       [alwaysTrueToken2]: BigInt(lock.assets[alwaysTrueToken2]),
          //     }
          //   )

          // take-out chest lock:
          // .payToContract(
          //   chestAddress,
          //   { inline: datum, scriptRef: lock.scriptRef! },
          //   { lovelace: BigInt(lock.assets["lovelace"]) }
          // )
          // .payToAddress(
          //   userAddress, // take-out the lock to any address
          //   { [lockName]: BigInt(lock.assets[lockName]) }
          // )

          .addSigner(userAddress)
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

  const unlockChest = async () => {
    try {
      console.log(`UnlockChest(${chestAddress}):`);
      if (lucid) {
        const utxos = (await lucid.utxosAt(chestAddress)).filter(
          (utxo) => utxo.datum // UTxO must have datum to be redeemable
        );
        console.log({ utxos: utxos });
        if (!utxos?.length) {
          throw { emptyScriptAddress: "No UTxO to redeem." };
        }

        const scriptRef = utxos.find((utxo) => utxo.scriptRef);
        console.log({ scriptRef: scriptRef });
        if (!scriptRef) {
          throw { noScriptRef: "No ScriptRef was found." };
        }

        const block = await getLatestBlockInfo();
        const time = block["time"] * 1_000; // ms
        console.log({ time: time });

        const tx = await lucid.newTx().readFrom([scriptRef]).collectFrom(utxos, action.unlockChest).validFrom(time).complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log({ txHash: txHash });

        setActionResult(`TxHash: ${txHash}`);
        window.location.reload();
        return txHash;
      }
      throw { error: "Invalid Lucid State!" };
    } catch (x) {
      setActionResult(JSON.stringify(x));
    }
  };

  const resendChest = async () => {
    try {
      console.log(`ResendChest(${chestAddress}):`);
      if (resendableChest) {
        const initialMintTxUTxOs = await getTxUTxOs(resendableChest.initial_mint_tx_hash);
        console.log({ initialMintTxUTxOs: initialMintTxUTxOs });

        const oRefScriptHash = initialMintTxUTxOs["outputs"].find((x: any) => x.reference_script_hash);
        console.log({ oRefScriptHash: oRefScriptHash });

        const validateScript: SpendingValidator = {
          type: "PlutusV2",
          script: await getScriptCBOR(oRefScriptHash.reference_script_hash), // cborHex
        };
        console.log({ validateScript: validateScript });

        const block = await getLatestBlockInfo();
        const time = block["time"] * 1_000; // ms
        console.log({ time: time });

        const deadline = BigInt(time + 60_000); // +1min
        const datum = Data.to(deadline);
        console.log({ datum: datum });

        const tx = await lucid
          .newTx()
          .payToContract(
            chestAddress,
            { inline: datum, scriptRef: validateScript },
            {
              lovelace: BigInt(42_000000),
              [`${resendableChest.policyID}${resendableChest.assetName}`]: BigInt(1),
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

  return !loaded ? (
    <></>
  ) : (
    <div>
      <div>
        <br />
        <hr />
        <br />
        <div>Mint arbitrary tokens for testing:</div>
        {/* MintTreasures */}
        <button className="btn btn-primary m-5" onClick={mintTreasures}>
          Mint Treasures
        </button>
        <br />
        <br />
        <hr />
        <br />
      </div>

      <div>
        {/* CreateChest */}
        <button className="btn btn-primary m-5" onClick={createChest}>
          Create Chest
        </button>

        {chestAddress?.length ? (
          <>
            {/* AddTreasure */}
            <button className="btn btn-secondary m-5" onClick={addTreasure}>
              Add Treasure
            </button>

            {keyTokenUTxO ? (
              <>
                {/* DelayUnlock */}
                <button className="btn btn-secondary m-5" onClick={delayUnlock}>
                  Delay Unlock
                </button>
              </>
            ) : (
              <></>
            )}

            {/* UnlockChest */}
            <button className="btn btn-secondary m-5" onClick={unlockChest}>
              Unlock Chest
            </button>

            {resendableChest ? (
              <>
                {/* ResendChest */}
                <button className="btn btn-secondary m-5" onClick={resendChest}>
                  Resend Chest
                </button>
              </>
            ) : (
              <></>
            )}

            {/* ChestInfo */}
            <div>{`Current ChestAddress: ${chestAddress}`}</div>
          </>
        ) : (
          <div>I don't remember having any chest currently...</div>
        )}
      </div>
    </div>
  );
};

export default MorbidV2;
