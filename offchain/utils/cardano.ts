import { C, fromHex } from "lucid-cardano";

export function hashDatum(inline: string): string {
  const datumBytes = fromHex(inline);
  const plutusData = C.PlutusData.from_bytes(datumBytes);
  const plutusHash = C.hash_plutus_data(plutusData);

  return plutusHash.to_hex();
}
