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
