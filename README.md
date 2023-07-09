# Davy Jones' Locker

<br/><div align="center">
  >_Fifteen men on the dead man's chest—_<br/>
  >_...Yo-ho-ho, and a bottle of rum!_<br/>
  >_Drink and the devil had done for the rest—_<br/>
  >_...Yo-ho-ho, and a bottle of rum!_<br/>
</div><br/>

This is a dead-man's switch contract where you can:
- Create Chest
- Add Treasure
- Delay Unlock
- Unlock Chest
- Resend Chest

## Create Chest
Mints `ChestLock` and `ChestKey`(s), a new address will be generated. Send `ChestLock`
while depositing initial assets. Keep `ChestKey` token(s) in your wallet, you will need
to show it when `DelayUnlock`.

Give access for delaying the chest unlocking deadline to anyone by sending them the
`ChestKey` token.

## Add Treasure
Anyone can deposit more assets to the chest. Don't forget to include an arbitrary inline
datum to be redeemable.

## Delay Unlock
Show the matching `ChestKey` token to postpone the chest unlocking deadline.

## Unlock Chest
Redeem all assets from the chest when the deadline has passed. You will also receive the
`ChestLock` NFT.

## Resend Chest
Resend `ChestLock` to the chest address. Similar to `CreateChest` but it is done by
the chest unlocker. There is no real benefit to do this, but it's possible.

---

### Chest Address Sample
```
addr_test1wrwc5pcsuhn759uqc5s2k8ffhfnddfdqsun8xy2da454zng8nltwz
```
View it in action on
[CExplorer](https://preview.cexplorer.io/address/addr_test1wrwc5pcsuhn759uqc5s2k8ffhfnddfdqsun8xy2da454zng8nltwz/tx#data).

#### Timeline
- **Create Chest**<br/>
  `https://preview.cexplorer.io/tx/e4a4626022f83ca1db7bf745fc91f91cf1094465de4609bcb5abed7943d6d045`
<br/>Mints **2 ChestKey** tokens. Sends some arbitrary tokens as well.

- **Add Treasure**<br/>
  `https://preview.cexplorer.io/tx/b10bd3b4abd1e8791bd1709d89581e8352b02789112bcbd9ea62c75bfe71d91b`
<br/>Put an arbitrary **inline datum** and send some arbitrary tokens.

- **Delay Unlock**<br/>
  `https://preview.cexplorer.io/tx/d4ea27ec7a64ab21c5a2c4e8d2b2b837003c36426d514575afb6570257594b6a`
<br/>Postpone chest unlocking deadline.

- **Unlock Chest**<br/>
  `https://preview.cexplorer.io/tx/35cc3aed97346411a35092b5cc1559501d911b7e732faf1fb194587b65abfb6f`
<br/>Redeem all assets including **ChestLock** NFT (named as **Dead Man's Chest**) from the chest.

- **Resend Chest**<br/>
  `https://preview.cexplorer.io/tx/eb71011020e13853e23a1e24f2e467c8f3841912b300b1ba0bbc1102df395573`
<br/>Resend **ChestLock** to the chest address and some arbitrary tokens.

- **Share Access**<br/>
  `https://preview.cexplorer.io/tx/864e6891df746aae55a97b73bf12058ef905123f6b83ebe073cfba449f53ad54`
<br/>Share **1 ChestKey** token to another address.

- **Delay Unlock** (by another address)<br/>
  `https://preview.cexplorer.io/tx/5b1cb9457236a341ac68c6f4a80f3e7c7f0f932a7f58db8025e8b4aba80dd731`
<br/>Using the shared **ChestKey** token.

---

<br/><div align="center">
  <img src="https://images5.alphacoders.com/641/641119.jpg"/>
  >_But one man of her crew alive,_<br/>
  >_What put to sea with seventy-five._<br/>
</div><br/>
