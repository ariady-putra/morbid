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
Mints `ChestLock` and `ChestKey`(s). Send `ChestLock` while depositing initial ADA.
Keep `ChestKey` token(s) in your wallet, you will need to show it when `DelayUnlock`.

Give access for delaying the chest unlocking deadline to anyone by sending them the
`ChestKey` token.

## Add Treasure
Anyone can deposit more ADA to the chest. Don't forget to include an arbitrary inline
datum to be redeemable.

## Delay Unlock
Show the matching `ChestKey` token to postpone the chest unlocking deadline.

## Unlock Chest
Redeem all ADA from the chest when the deadline has passed. You will also receive the
`ChestLock` NFT.

## Resend Chest
Resend `ChestLock` to the chest address. Similar to `CreateChest` but it's done by the
chest unlocker. There is no real benefit to do this, but it's possible.

---

### Chest Address Sample
```
addr_test1wr702hq6tnejc2gw35tl7q58u20klq5390fzyqzt7ns5hhgq2lcex
```
View it in action on
[CExplorer](https://preview.cexplorer.io/address/addr_test1wr702hq6tnejc2gw35tl7q58u20klq5390fzyqzt7ns5hhgq2lcex/tx#data).

---

<br/><div align="center">
  <img src="https://images5.alphacoders.com/641/641119.jpg"/>
  >_But one man of her crew alive,_<br/>
  >_What put to sea with seventy-five._<br/>
</div><br/>
