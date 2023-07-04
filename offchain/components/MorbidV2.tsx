import {
  Constr,
  Data,
  Lucid,
  MintingPolicy,
  SpendingValidator,
  UTxO,
  applyDoubleCborEncoding,
  applyParamsToScript,
  fromText,
} from "lucid-cardano";
import { useEffect, useState } from "react";
import {
  getAssetMetadata,
  getLatestBlockInfo,
  getScriptCBOR,
  getTxMetadata,
  getTxUTxOs,
} from "../utils/blockfrost";

declare type TokenUTxO = {
  initial_mint_tx_hash: string;
  policyID: string;
  assetName: string;
  metadata: any;
};

const MorbidV2 = (props: {
  lucid: Lucid;
  setActionResult: (actionResult: string) => void;
}) => {
  const lucid = props.lucid;
  const setActionResult = props.setActionResult;

  const [loaded, setLoaded] = useState(false);
  const [chestAddress, setChestAddress] = useState("");
  const [resendableChest, setResendableChest] = useState<TokenUTxO>();

  useEffect(() => {
    lookForChestAddress();
    lookForResendableChest();
    setLoaded(true);
  }, []);

  const lookForChestAddress = async () => {
    const key = await lookForToken(chestKey.name);
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

      const forEachAsync = async (
        utxos: UTxO[],
        callback: { (utxo: UTxO): Promise<void>; (arg0: any): any }
      ) => {
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

            const onchainMetadata =
              assetMetadata["onchain_metadata"] ||
              (await getTxMetadata(initial_mint_tx_hash))["json_metadata"][
                policyID
              ][assetName];
            console.log({ onchainMetadata: onchainMetadata });
            result = {
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
      "5909e20100003232323232323232323232323222322232533300b3333323232323232323232323232323232323232323232322222323253302932332232373200266603c0046e600052201003302e37526e60c06d2210852656465656d6572003302e37526e61241023a20003302e37526e60c06d2210122003302e37526e60c8c8dcc80099980d00099b81371a002900124410037660026605c6ea4dcc180da450122004bd7024900330280044c0103d8798000132533302d001153330284a0294454cc0a92411d76616c69645f6e756d6265725f6f665f6173736574203f2046616c73650014a0264a66605c0022a6660529405288a99815a4811d76616c69645f6e756d6265725f6f665f6173736574203f2046616c73650014a0264a66605e0022646464646466036002464a666062002294454cc0cd24011773656e745f63686573745f6c6f636b203f2046616c73650014a0646464a666070607600426464a66606a66e20c8c8c8c94ccc0e4cdc3a400460786ea8cc0d4c0dc00d2002100113370000290011bad303f00130353253330383370e9001181b80088008a9981d2492a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e001633033303500148000cc88c94ccc0e4cdc3a400000220062a66607266e1d20020011003100230373253330393370e90000008a60103d8798000153330393370e90020008a6103d87b80001533303953330393370e9001181e1baa330353037003480084cdc3a400460786ea8cc0d4c0dc009200213330393370e9001181e1baa330353037002480092825114c0103d87a8000153330393370e9001181e1baa33035303700348008530103d87b800014c103d87980003037332232533303b3370e9000000899299981e19b8748000004530103d87a800014c103d8798000303a0021533303b3370e9002000899299981e19b8748010004530103d87a800014c103d87b8000303a0021323232533303e3370e90000008a60103d87b80001533303e3370e90020008a6103d8798000132325333040337100080022980103d8798000153330403370e0080022980103d87a800014c103d87b8000375a608c002607800a60780086eb4c10c004c0e400cc0e4008cc0d0c0d8009200033034303600148000cc0c8c0d0005200033032303400148008cc0c4c0cc015200e00114a22a6606e920144756e736166655f756e777261702e66696e6974655f73746172745f6f662874786e2e76616c69646974795f72616e676529203c20646561646c696e65203f2046616c73650014a06eb4c0e0008dd7181b0008a9981aa481334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016303900132330160012323253330353370e9000000899260103d87a800030330021323232533303d00114c103d87a8000132533303e00113232533303b3370e9002000899191919ba548000cc110c8dd3998229821000998229821800a5eb80cdd81ba9008375000297ae0375a00260860026072004298103d87a8000303900133035303700748010530103d87a800030400013233301a0014bd7011119980d8010009111919299982019b8748008004400c4c8c8cc120dd38008029bac3048001303e002303e001533303e533303e533303e3371e00c030266e3c00c074528099b8700248008528099ba548000cc110dd3998221ba9006330443752006660886ea120024bd7025eb80530103d87a8000375666066606a00a90011bae303d001303300230330013302f30313302f303100148001200037586605a605e002900219816181700624000660340024664464a666064002294454cc0d12401186d696e7465645f63686573745f6b6579203f2046616c73650014a06602e004466e3cdd7181a8008011981a8029981a80225eb7bdb18122010843686573744b657900330190012332232533303100114a22a660669201196d696e7465645f63686573745f6c6f636b203f2046616c73650014a06603000466ec0dd48009ba848008cc0d0010cc0d000d2f5bded8c001a660309448c94ccc0b80045288a998182481186f6e655f73686f745f6d696e74696e67203f2046616c73650014a06466030002466ebccc0acc0b4005200000d3758660526056660526056012900024000606400660620062a6660549405288a9981624811d76616c69645f6e756d6265725f6f665f6173736574203f2046616c73650014a06062002606000264646464a66605666e1d200200114bd6f7b6300991bab303300130290023029001330160010033301700148900375666046604a66046604a0069000240106eb8c0b4004c08cc94ccc098cdc3a4000604a00220022a660509212a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e001633021302300148008c00400488894ccc0a000c40084c8c8c8c8ccc014004008cccc02002000c018014dd718148019bad3029002302c004302a00330010012222533302600310021323232323330050010023333008008003006005375c604e0066eacc09c008c0a8010c0a000cc0040048894ccc08c00852f5bded8c0264646464a66604466e1d20020011333007007003005132323302a0013330090090050073232337606058004605800260580026eb0c0a8004c080008c080004c00c008c09c00cc094008c0040048894ccc08400852809919299980f18018010a511333005005001003302500330230023001001222533301f00214a026464a66603866ebcdd31981100125eb7bdb180dd319811001a5eb7bdb1805288999802802800801981180198108011800800911299980e8010a5013232533301a300300214a2266600a00a0020066042006603e00444a66602c00426002930a503001001222533301a00214c0103d87a800013232323253330193371e00a002266e9520003301f374c00497ae01333007007003005375c60360066eacc06c008c07800cc070008c0040048894ccc06000852f5bded8c0264646464a66602e66e3c014004400c4cc074cdd81ba9001374c00466600e00e00600a6eb8c06400cdd5980c801180e001980d00118008009111299980919b880024800040044c8c8c8cccc01c01c018cdc0802a400466e2ccdc0001299980a99b8800248050520601482b804cdc599b80001533301533710002900a0a40c0290570080219b8600248080cdc1800a404066e3800c0088dcc80098008009111299980a0018800899191999803003000802299980899b87371a006900009b9800213371400666e28010dcc001180c0021b99375c602c00600800c0020042930b299980519b8748000c034dd50010a4c2c6eb8004cc0040052000222233330073370e00200601c4666600a00a66e00011200230100010020022300737540024600a6ea80055cd2b9b5738aae7555cf2ab9f5740ae855d101",
    validate:
      "590fed01000032323232323232323232323223223222232533300d333333232323232323232323232323232323232323232323232323232323232323232323232323232222223253303a332232373200266605a0046e60005221003303e37526e60c0a92210d4d61796265446561646c696e65003303e37526e61241023a20003303e37526e60c0a92210122003303e37526e60c8c8dcc80099981480099b81371a002900124410037660086607c6ea4dcc18152450122004bd7024900132533303a3370e900000089919814800919299981e8008a51153303e49119726573656e745f63686573745f6c6f636b203f2046616c73650014a0646464a666086608c004264646464a66608e609400426464a66608aa66608aa66608a66e3c0180085288a99823248127696e7075745f61646472657373203d3d206f75747075745f61646472657373203f2046616c73650014a02a66608a66e200140045288a99823249236f6c645f646561646c696e65203c206e65775f646561646c696e65203f2046616c73650014a029404c94ccc12800454ccc119280a511533047490125666f756e645f63686573745f6b65795f696e5f7265665f696e70757473203f2046616c73650014a02a66608c602c002294454cc11d2401306c6973742e616e64287369676e65645f62795f616c6c5f63686573745f6b65795f686f6c64657229203f2046616c73650014a0646603000246464a66609a0022980103d87a800013374a900019827199824991980f0009919299982619b87480000044c8dd7182980098250010991bae3053001304a002304a00133046304833046304800348001200037586608a608e01a900826103d87a80004c0103d87980004bd7019198100009112999826299982619b8f00301813371e00491010843686573744b65790014a0266e95200033051374e660a26ea400ccc144dd4801198289ba80014bd7025eb805300103d87a8000375666088608c0029001198219822800a40046eb0cc104c10c025200214a06eb4c11c008dd718228008a99822249334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163048001323301e001233301900100f01037586607a607e00a90021bad3043002375c60820022a660809201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163044001323301a00123233301600100c00d3303b303d00148008dd61981c981d800a400066070607400a900019814251232533303c00114a22a6607a9201136e6f745f647261696e6564203f2046616c73650014a06464646464646464a66608e60940042646464646464a66609a60a0004264a666094a66609466e1c01c0045288a99825a4929696e7075745f6c6f76656c616365203d3d206f75747075745f6c6f76656c616365203f2046616c73650014a02a66609466ebc0200085288a99825a4935696e7075745f7363726970745f61646472657373203d3d206f75747075745f7363726970745f61646472657373203f2046616c73650014a02940c09c01054cc1292401334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016304e0013230290013302a00100b375860920046eb0c11c004c8c8c0ac004cc0b0004020dd6198201821004a400860420082a66088921334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016304800132302300133024001005375860860046eb0c104004c8c8c094004cc0980048c8c010004cc0f0c0f8005200237586607460780069000119bb03750a66607c6460060026607460780029000099199814000a44100488100375666074607800290010a4000660746078002900011919299981f99b87480080044c928981e801099250303d002303d00133039303b00148000cc0dcc0e40112000132533303b00114a22a6607892011d756e6c6f636b5f61667465725f646561646c696e65203f2046616c73650014a0646464646464a66608066e20004c8c8c8c94ccc110cdc3a4004608c6ea8cc100c10800d2002100113370000290011bad304900130403253330433370e9001182100088008a998222492a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e00163303e304000148000cc88c94ccc110cdc3a400000220062a66608866e1d20020011003100230423253330443370e90000008a60103d8798000153330443370e90020008a6103d87b80001533304453330443370e900118231baa330403042003480084cdc3a4004608c6ea8cc100c108009200213330443370e900118231baa330403042002480092825114c0103d87a8000153330443370e900118231baa33040304200348008530103d87b800014c103d8798000304233223253330463370e9000000899299982399b8748000004530103d87a800014c103d87980003045002153330463370e9002000899299982399b8748010004530103d87a800014c103d87b80003045002132323253330493370e90000008a60103d87b8000153330493370e90020008a6103d879800013232533304b337100080022980103d87980001533304b3370e0080022980103d87a800014c103d87b8000375a60a0002608e00a608e0086eb4c134004c11000cc110008cc0fcc10400920003303f304100148000cc0f4c0fc00520003303d303f00148008cc0f0c0f8019200e14a22a6608292144756e736166655f756e777261702e66696e6974655f73746172745f6f662874786e2e76616c69646974795f72616e676529203e20646561646c696e65203f2046616c73650014a06eb4004c110004c0ecc94ccc0f8cdc3a4008607a00220022a6607e92012a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e001633039303b33039303b0014800920043042001303932533303c3370e9000181d80088008a9981ea492a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e0016323300a00123370e66604c6eaccc0e4c0eccc0e4c0ec005200248008024029200237586606e607200290001981b181c001a40006070006606c64a66607266e1d200230380011001153303a49012a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e001633034303600148008c0040048894ccc0e00085300103d87a8000132325333036300300213374a90001981d80125eb804ccc01401400400cc0f000cc0e8008c004004894ccc0d40045288991929998198010998020020008a5030390023370e900118199baa30370013001001222533303400214bd70099191919299981a19b87480080044ccc01c01c00c0144c8c8cc0ecccc0d800530103d87a80004c0103d87980003330090090050073370e9001181b9baa303b001303200230320013003002303800330360023001001222533303200214a026464a66606066e3c00800c5288999802802800801981b0019bae30340022223232533302f3370e900000089926103d87a8000302d0021323232533303600114c103d87a800013253330370011323253330353370e9002000899191919ba548000cc0f4c8dd39981f181d8009981f181e000a5eb80cdd81ba9008375000297ae0375a00260780026066004298103d87a800030330013302f303100948010530103d87a8000303900132330090012225333035533303553330353371e006014266e3c008024528099b8700148008528099ba548000cc0e8dd39981d1ba90033303a3752004660746ea120024bd7025eb805300103d87a800037566605a605e00e90011bae3036001302d002302d00133029302b33029302b003480012000223330050024bd701111998030010009111919299981999b8748008004400c4c8c8cc0e8dd38008029bac303a0013031002303100133300700600300230010012222533302f00310021323232323330050010023333008008003006005375c60600066eb4c0c0008c0cc010c0c400cc00400488894ccc0b400c40084c8c8c8c8ccc014004008cccc02002000c018014dd718170019bab302e0023031004302f0033001001222533302a00214bd6f7b630099191919299981519b87480080044ccc01c01c00c0144c8c8cc0c4004ccc02402401401cc8c8cdd81819801181980098198009bac3031001302800230280013003002302e003302c00223330020014800088cdc000080118008009111299981400188010991919998030030009980180100200198160021bad302a003300100122533302400114bd7009919198138011980200219802800919981199baf0010034a0944c0a0008c098004c0040048894ccc08c00852f5c026464a666042600600426604c00466600a00a002006266600a00a002006604e006604a004600200244a666040002297bdb180101800001018000132323232323232337606e9ccc0a0dd40020011ba7330280050013758604a0046eb0c08c004cc01801800cdd698108019810801181200118110009800800911299980f8010a5eb7bdb1804c8c8cc088c00c008ccc01401400400cc08c00cc084008888c8c8c94ccc074cdc3a4004002290000991bad3024001301b002301b00132533301c3370e90010008a60103d87a8000132323300800100537566046002603400460340026600c0060046002002444a666038004298103d87a8000132323232533301c3371e00a002266e95200033021375000497ae01333007007003005375c603a0066eb4c074008c08000cc078008c0040048894ccc068008530103d87a8000132323232533301a3371e00a002266e9520003301f374c00497ae01333007007003005375c60360066eacc06c008c07800cc070008894ccc0500084c0052614a060020024444a66602866e2000920001001132323233330070070063370200a900119b8b33700004a66602e66e200092014148180520ae013371666e000054ccc05ccdc4000a4028290300a415c0200866e18009202033706002901019b8e0030022373200260020024444a66602a006200226464666600c00c002008a66602666e1cdc6801a400026e600084cdc500199b8a004373000460320086e64dd7180b8018028038020008010a4c2c6400664a66601a66e1d200000115333011300b003149854cc0392411d4578706563746564206e6f206669656c647320666f7220436f6e73747200161533300d3370e90010008a99980898058018a4c2a6601c92011d4578706563746564206e6f206669656c647320666f7220436f6e7374720016153300e4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016300b002375c0026eb8004cc0040052000222233330073370e00200601a4666600a00a66e000112002300f001002002230063754002460086ea80055cd2b9c5573aaae7955cfaba05742ae881",
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
    name: "MyChest",
    image: "ipfs://QmavSMgKNtRFu4wXCucUbC4dNDqwN4TDfBoW6sk7kxj9tE",
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
        const mintChest = applyParamsToScript(compiledCode.mintChest, [
          mintChestParams.utxoRef,
          mintChestParams.chestName,
        ]);
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
        const chest = applyParamsToScript(compiledCode.validate, [
          chestParams.policyId,
          chestParams.chestName,
        ]);
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
          .mintAssets(
            { [mintedLockName]: BigInt(1), [mintedKeyName]: BigInt(1) },
            voidData
          )
          .payToContract(
            chestAddress,
            { inline: datum, scriptRef: validateScript },
            { lovelace: BigInt(42_000000), [mintedLockName]: BigInt(1) }
          )
          .attachMetadata(721, {
            [mintChestScriptHash]: {
              [chestHexName]: {
                name: chestLock.name,
                image: chestLock.image,
              },
              [chestKeyHexName]: {
                name: `${chestKey.name}1`,
                image: chestKey.image,
                chest_address: chestAddress,
                ref_hash: utxos[0].txHash,
                ref_index: utxos[0].outputIndex,
              },
            },
          })
          .validFrom(time)
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
      console.log(`DelayUnlock(${chestAddress}):`);
      if (lucid) {
        // const block = await getLatestBlockInfo();
        // const time = block["time"] * 1_000; // ms
        // console.log({ time: time });
        // const userAddress = await lucid.wallet.address();
        // console.log({ userAddress: userAddress });
        // const morbidAddress = lucid.utils.validatorToAddress(morbidScript);
        // console.log({ scriptAddress: morbidAddress });
        // const utxos = await lucid.utxosAt(morbidAddress);
        // console.log({ utxos: utxos });
        // if (!utxos?.length) {
        //   throw { emptyScriptAddress: "No UTxO to redeem." };
        // }
        // let treasure = 0;
        // const chestUTxOs: UTxO[] = [];
        // const refScript: UTxO[] = [];
        // const forEachAsync = async (
        //   utxos: UTxO[],
        //   callback: { (utxo: UTxO): Promise<void>; (arg0: any): any }
        // ) => {
        //   for (const utxo of utxos) {
        //     await callback(utxo);
        //   }
        // };
        // const myAsyncCallback = async (utxo: UTxO) => {
        //   if (!utxo.datum) return;
        //   const datumFields = await getDatumFields(hashDatum(utxo.datum));
        //   switch (datumFields.length) {
        //     case 2: // CreateChest
        //       if (utxo.txHash === chest.txHash) {
        //         const deadline = datumFields[0];
        //         const deadlineValue = deadline["int"];
        //         if (deadlineValue < time) {
        //           refScript.push(utxo);
        //           chestUTxOs.push(utxo);
        //           treasure += Number.parseInt(
        //             assetsToValue(utxo.assets).to_js_value()["coin"]
        //           );
        //         }
        //       }
        //       return;
        //     case 1: // AddTreasure
        //       const refTxn = datumFields[0];
        //       const refTxnValue = refTxn["bytes"];
        //       if (refTxnValue === chest.txHash) {
        //         chestUTxOs.push(utxo);
        //         treasure += Number.parseInt(
        //           assetsToValue(utxo.assets).to_js_value()["coin"]
        //         );
        //       }
        //       return;
        //     default:
        //       return;
        //   }
        // };
        // await forEachAsync(utxos, myAsyncCallback);
        // console.log({ chestUTxOs: chestUTxOs });
        // if (!chestUTxOs.length) {
        //   throw { nothingToUnlock: "No valid UTxO to redeem." };
        // }
        // console.log({ refScript: refScript });
        // if (!refScript.length) {
        //   throw { refScriptNotFound: "No reference script found." };
        // }
        // const delayUnlock = Data.to(new Constr(0, [])); // redeemer
        // console.log({ redeemer: delayUnlock }); // DelayUnlock
        // const deadline = time + 1_000; // +1sec
        // const creator =
        //   lucid.utils.getAddressDetails(userAddress).paymentCredential?.hash;
        // const createChest = Data.to(
        //   new Constr(0, [BigInt(deadline), String(creator)])
        // );
        // console.log({ datum: createChest });
        // console.log({ treasure: treasure });
        // const tx = await lucid
        //   .newTx()
        //   .collectFrom(chestUTxOs, delayUnlock)
        //   .readFrom(refScript) // reference script
        //   .addSigner(userAddress)
        //   // normal case:
        //   .payToContract(
        //     morbidAddress, // re-create chest
        //     { inline: createChest, scriptRef: morbidScript },
        //     { lovelace: BigInt(treasure) }
        //   )
        //   // drain by recreate chest to another address:
        //   //   .payToAddressWithData(
        //   //     userAddress, // drain to any address
        //   //     { inline: createChest },
        //   //     { lovelace: BigInt(treasure) }
        //   //   )
        //   // drain by recreate chest to another address with multiple txOuts:
        //   //   .payToContract(
        //   //     morbidAddress, // re-create chest
        //   //     { inline: createChest, scriptRef: morbidScript },
        //   //     { lovelace: BigInt(0) }
        //   //   )
        //   //   .payToAddress(
        //   //     userAddress, // drain to any address
        //   //     { lovelace: BigInt(treasure) }
        //   //   )
        //   .validFrom(time)
        //   .complete();
        // const signedTx = await tx.sign().complete();
        // const txHash = await signedTx.submit();
        // console.log({ txHash: txHash });
        // setActionResult(`TxHash: ${txHash}`);
        // setChest({ txHash: txHash });
        // return txHash;
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

        const tx = await lucid
          .newTx()
          .readFrom([scriptRef])
          .collectFrom(utxos, action.unlockChest)
          .validFrom(time)
          .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log({ txHash: txHash });

        setActionResult(`TxHash: ${txHash}`);
        // setChestAddress("");
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
        const initialMintTxUTxOs = await getTxUTxOs(
          resendableChest.initial_mint_tx_hash
        );
        console.log({ initialMintTxUTxOs: initialMintTxUTxOs });

        const oRefScriptHash = initialMintTxUTxOs["outputs"].find(
          (x: any) => x.reference_script_hash
        );
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
              [`${resendableChest.policyID}${resendableChest.assetName}`]:
                BigInt(1),
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

          {/* DelayUnlock */}
          <button className="btn btn-secondary m-5" onClick={delayUnlock}>
            Delay Unlock
          </button>

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
  );
};

export default MorbidV2;
