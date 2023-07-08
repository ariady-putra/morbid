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
addr_test1wr702hq6tnejc2gw35tl7q58u20klq5390fzyqzt7ns5hhgq2lcex
```
View it in action on
[CExplorer](https://preview.cexplorer.io/address/addr_test1wr702hq6tnejc2gw35tl7q58u20klq5390fzyqzt7ns5hhgq2lcex/tx#data).

#### Timeline
- **Create Chest**<br/>
  `https://preview.cexplorer.io/tx/306eeab001b20cd84fe0069197816672972b07696e1c46c6901e94e39f900c75`
<br/>Mints **2 ChestKey** tokens.

- **Add Treasure**<br/>
  `https://preview.cexplorer.io/tx/8bbf6796b0191370c1fff44c1e2f7b4176ef4b0baccc8de654c3696fcbe21605`
<br/>Put an arbitrary **inline datum**.

- **Delay Unlock**<br/>
  `https://preview.cexplorer.io/tx/910d0c912413ede206548f22dc0e606893263e8f9ab45164004289c75985b570`
<br/>Postpone chest unlocking deadline.

- **Unlock Chest**<br/>
  `https://preview.cexplorer.io/tx/91de67cfc1077c56c22696c5efbf41bf19744ef9ea2495969ca36a78f13d6c29`
<br/>Redeem all assets including **ChestLock** NFT (named as **Dead Man's Chest**) from the chest.

- **Resend Chest**<br/>
  `https://preview.cexplorer.io/tx/6f13814c90d127a8140be2ab40242ef51d216f9af5a4f47e55341a7e296ae46b`
<br/>Resend **ChestLock** to the chest address.

- **Share Access**<br/>
  `https://preview.cexplorer.io/tx/24a05669a6048ffa278fcea3d73bcf04a82ac20736609063d13d3c4425ef6e46`
<br/>Share **1 ChestKey** token to another address.

- **Delay Unlock** (by another address)<br/>
  `https://preview.cexplorer.io/tx/975238029fec63bd43ce206ecf6cb13fa4ad86d87423e56ffb76633aa984838c`
<br/>Using the shared **ChestKey** token.

---

<br/><div align="center">
  <img src="https://images5.alphacoders.com/641/641119.jpg"/>
  >_But one man of her crew alive,_<br/>
  >_What put to sea with seventy-five._<br/>
</div><br/>
