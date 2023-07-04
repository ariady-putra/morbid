import type { NextPage } from "next";
import Router from "next/router";

import { useState, useEffect } from "react";
import { Lucid } from "lucid-cardano";

import { useStoreState } from "../utils/store";
import initLucid from "../utils/lucid";

import MorbidV2 from "../components/MorbidV2";
import NavBar from "../components/NavBar";
import Title from "../components/Title";

const Helios: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet);
  const [lucid, setLucid] = useState<Lucid>();

  const [actionResult, setActionResult] = useState("");

  const [loaded, setLoaded] = useState(false);

  useEffect(() => {
    setLoaded(true);
  }, []);

  useEffect(() => {
    if (walletStore.name == "") {
      Router.push("/");
      return;
    }

    if (lucid) {
    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => {
        setLucid(Lucid);
      });
    }
  }, [lucid]);

  return !loaded || !lucid || walletStore.name == "" ? (
    <></>
  ) : (
    <div className="px-10">
      <Title />

      <div className="navbar bg-base-100">
        <NavBar />
      </div>

      <MorbidV2 lucid={lucid} setActionResult={setActionResult} />

      <div className="px-10 text-xl">
        <pre>{actionResult}</pre>
      </div>
    </div>
  );
};

export default Helios;
