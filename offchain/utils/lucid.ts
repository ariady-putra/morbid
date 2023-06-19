import { Blockfrost, Lucid } from "lucid-cardano";

const initLucid = async (wallet: string) => {
  const api = await window.cardano[wallet.toLowerCase()].enable();

  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preview.blockfrost.io/api/v0",
      process.env.NEXT_PUBLIC_BLOCKFROST as string
    ),
    "Preview"
  );

  lucid.selectWallet(api);

  return lucid;
};

export default initLucid;
