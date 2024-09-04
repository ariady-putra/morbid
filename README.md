# Davy Jones' Locker

> [!IMPORTANT]
> The offchain is likely to fail after Conway era (Chang hardfork).
> A fix will be delivered some time later after the storm subsides.
> Or probably a new and better front-end will be developed instead.

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

Updated for [aiken v1.0.14-alpha](https://github.com/aiken-lang/aiken/releases/tag/v1.0.14-alpha):
```
addr_test1wze6hmn8spj3lcqrckjmytms0uwm4830zjvdwzqsyrzlusqfhsyck
```

View it in action on
[CExplorer](https://preview.cexplorer.io/address/addr_test1wze6hmn8spj3lcqrckjmytms0uwm4830zjvdwzqsyrzlusqfhsyck/tx#data).

#### Timeline

- **Create Chest**<br/>
  `https://preview.cexplorer.io/tx/218c1cb8564e1223202b7116d9897d955e493f44776d93b826e698c768a8b1c6`
<br/>Mints **2 ChestKey** tokens. Sends some arbitrary tokens as well.

- **Add Treasure**<br/>
  `https://preview.cexplorer.io/tx/9db99e7b49963bb5f63fbac80030c916b3a8a14e2b829e50cf87555d4f565aa2`
<br/>Put an arbitrary **inline datum** and send some arbitrary tokens.

- **Delay Unlock**<br/>
  `https://preview.cexplorer.io/tx/0970f113f9004b184b72ac25533a5900360252b58d6a9c9b8af5a6245fb470f5`
<br/>Postpone chest unlocking deadline.

- **Unlock Chest**<br/>
  `https://preview.cexplorer.io/tx/ca73a798869583209fe6330519710138bd05b9bdbdca2511442e22f1e18d2709`
<br/>Redeem all assets including **ChestLock** NFT (named as **Dead Man's Chest**) from the chest.

- **Resend Chest**<br/>
  `https://preview.cexplorer.io/tx/10eead16236bb0c0d7fa1dae9d4c03dffdd8d927f89201449c8dd4f286035da3`
<br/>Resend **ChestLock** to the chest address and some arbitrary tokens.

- **Share Access**<br/>
  `https://preview.cexplorer.io/tx/b3b9c8d961120f9f7cf173884938877146b641612c5d2bd9c0c14a5f7e21b19b`
<br/>Share **1 ChestKey** token to another address.

- **Delay Unlock** (by another address)<br/>
  `https://preview.cexplorer.io/tx/f0082d9e75fae24ec0c7ff8668ce4ed7206385afe01d13e215ef80c832dac2d5`
<br/>Using the shared **ChestKey** token.

---

<br/><div align="center">
  <img src="https://images5.alphacoders.com/641/641119.jpg"/>
  >_But one man of her crew alive,_<br/>
  >_What put to sea with seventy-five._<br/>
</div><br/>
