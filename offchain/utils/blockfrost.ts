export async function getLatestBlockInfo() {
  const block = await fetch(
    "https://cardano-preview.blockfrost.io/api/v0/blocks/latest",
    {
      headers: {
        project_id: process.env.NEXT_PUBLIC_BLOCKFROST as string,
      },
    }
  );
  const blockJson = await block.json();
  // console.log({ blockJson: blockJson });

  return blockJson;
}

export async function getDatumFields(datumHash: string) {
  const datum = await fetch(
    `https://cardano-preview.blockfrost.io/api/v0/scripts/datum/${datumHash}`,
    {
      headers: {
        project_id: process.env.NEXT_PUBLIC_BLOCKFROST as string,
      },
    }
  );
  const datumJson = await datum.json();
  // console.log({ datumJson: datumJson });
  const datumJsonValue = datumJson["json_value"];
  // console.log({ datumJsonValue: datumJsonValue });
  const datumFields = datumJsonValue["fields"];
  // console.log({ datumFields: datumFields });

  return datumFields;
}

export async function getScriptCBOR(scriptHash: string) {
  return (
    await (
      await fetch(
        `https://cardano-preview.blockfrost.io/api/v0/scripts/${scriptHash}/cbor`,
        {
          headers: {
            project_id: process.env.NEXT_PUBLIC_BLOCKFROST as string,
          },
        }
      )
    ).json()
  )["cbor"];
}

export async function getTxUTxOs(txHash: string) {
  return (
    await fetch(
      `https://cardano-preview.blockfrost.io/api/v0/txs/${txHash}/utxos`,
      {
        headers: {
          project_id: process.env.NEXT_PUBLIC_BLOCKFROST as string,
        },
      }
    )
  ).json();
}

export async function getTxMetadata(txHash: string) {
  const metadata = await fetch(
    `https://cardano-preview.blockfrost.io/api/v0/txs/${txHash}/metadata`,
    {
      headers: {
        project_id: process.env.NEXT_PUBLIC_BLOCKFROST as string,
      },
    }
  );
  const metadataJson = await metadata.json();
  // console.log({ metadataJson: metadataJson[0] });
  return metadataJson[0];
}

export async function getAssetMetadata(assetName: string) {
  if (assetName === "lovelace") return;

  const metadata = await fetch(
    `https://cardano-preview.blockfrost.io/api/v0/assets/${assetName}`,
    {
      headers: {
        project_id: process.env.NEXT_PUBLIC_BLOCKFROST as string,
      },
    }
  );
  const metadataJson = await metadata.json();
  // console.log({ metadataJson: metadataJson });
  return metadataJson;
}
