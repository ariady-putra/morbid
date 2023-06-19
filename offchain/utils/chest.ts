import { createTypedHooks, persist } from "easy-peasy";
import { Action, action } from "easy-peasy";
import { createStore } from "easy-peasy";

interface ChestStore {
  txHash: string;
}

interface StoreModel {
  chest: ChestStore;
  setChest: Action<StoreModel, ChestStore>;
}

const chestModel: StoreModel = {
  chest: { txHash: "" },
  setChest: action((state, txHash) => {
    state.chest = txHash;
  }),
};

const chestStore = createStore(persist(chestModel));
export default chestStore;

const { useStoreActions, useStoreState, useStoreDispatch, useStore } =
  createTypedHooks<StoreModel>();

export { useStoreActions, useStoreState, useStoreDispatch, useStore };
