import { useState, useEffect } from "react";
import { useStoreActions, useStoreState } from "../utils/store";
import initLucid from "../utils/lucid";

const WalletConnect = () => {
  const walletStore = useStoreState((state) => state.wallet);
  const setWallet = useStoreActions((actions) => actions.setWallet);
  const availableWallets = useStoreState((state) => state.availableWallets);
  const setAvailableWallets = useStoreActions(
    (actions) => actions.setAvailableWallets
  );

  const [connectedAddress, setConnectedAddress] = useState("");

  const [loaded, setLoaded] = useState(false);

  const loadWalletSession = async () => {
    if (
      walletStore.connected &&
      walletStore.name &&
      window.cardano /*&&
      (await window.cardano[walletStore.name.toLowerCase()].enable())*/
    ) {
      walletConnected(walletStore.name);
    }
  };

  const walletConnected = async (wallet: string, connect: boolean = true) => {
    const addr = connect
      ? await (await initLucid(wallet)).wallet.address()
      : "";
    const walletStoreObj = connect
      ? { connected: true, name: wallet, address: addr }
      : { connected: false, name: "", address: "" };
    setConnectedAddress(addr);
    setWallet(walletStoreObj);
  };

  const selectWallet = async (wallet: string) => {
    if (
      window.cardano /*&&
      (await window.cardano[wallet.toLocaleLowerCase()].enable())*/
    ) {
      walletConnected(wallet);
    }
  };

  useEffect(() => {
    let wallets = [];
    if (window.cardano) {
      for (const key in window.cardano) {
        if (window.cardano[key].apiVersion) {
          wallets.push(key);
        }
      }
      wallets.sort();
      loadWalletSession();
    }
    setAvailableWallets(wallets);
    setLoaded(true);
  }, []);

  return !loaded ? (
    <></>
  ) : (
    <div className="dropdown dropdown-end">
      <label tabIndex={0} className="btn m-1">
        {connectedAddress != "" ? (
          <>
            <img
              src={window.cardano[walletStore.name].icon}
              className="w-8 h-8"
            />
            {walletStore.address.slice(0, 16)}....
            {walletStore.address.slice(walletStore.address.length - 8)}
          </>
        ) : (
          <>Connect</>
        )}
      </label>
      <ul
        tabIndex={0}
        className="dropdown-content menu p-2 shadow bg-base-300 rounded-box w-52"
      >
        {availableWallets.map(
          (wallet) =>
            wallet != walletStore.name && (
              <li
                key={wallet}
                onClick={() => {
                  selectWallet(wallet);
                }}
              >
                <a>
                  <img
                    src={window.cardano[wallet].icon}
                    className="w-10 h-10"
                  />
                  {window.cardano[wallet].name} <br />
                  (API v{window.cardano[wallet].apiVersion})
                </a>
              </li>
            )
        )}
      </ul>
    </div>
  );
};

export default WalletConnect;
