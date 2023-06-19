import { createTypedHooks, persist } from "easy-peasy";
import { Action, action } from "easy-peasy";
import { createStore } from "easy-peasy";

interface WalletStore {
  connected: boolean;
  name: string;
  address: string;
}

interface StoreModel {
  wallet: WalletStore;
  setWallet: Action<StoreModel, WalletStore>;
  availableWallets: string[];
  setAvailableWallets: Action<StoreModel, string[]>;
}

const model: StoreModel = {
  wallet: { connected: false, name: "", address: "" },
  setWallet: action((state, newWallet) => {
    state.wallet = newWallet;
  }),
  availableWallets: [],
  setAvailableWallets: action((state, newAvailableWallets) => {
    state.availableWallets = newAvailableWallets;
  }),
};

// TODO: Fix numerous connect-wallet request pop-ups due to calling `window.cardano[key].enable()` in various places
// const store = createStore(persist(model));
const store = createStore(model);
export default store;

const { useStoreActions, useStoreState, useStoreDispatch, useStore } =
  createTypedHooks<StoreModel>();

export { useStoreActions, useStoreState, useStoreDispatch, useStore };
