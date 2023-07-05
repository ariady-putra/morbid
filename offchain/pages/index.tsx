import { useEffect, useState } from "react";

import type { NextPage } from "next";
import Link from "next/link";

import NavBar from "../components/NavBar";
import Title from "../components/Title";

const Home: NextPage = () => {
  const [loaded, setLoaded] = useState(false);

  useEffect(() => {
    setLoaded(true);
  }, []);

  return !loaded ? (
    <></>
  ) : (
    <div className="px-10">
      <Title />

      <div className="navbar bg-base-100">
        <NavBar />
      </div>

      <div className="mx-40 my-10">
        <Link href="/helios">
          <button className="btn btn-primary m-5">
            Enter Davy Jone's Locker
          </button>
        </Link>
        <Link href="/morbidV2">
          <button className="btn btn-primary m-5">
            v0.2
          </button>
        </Link>
        <Link href="/morbidV1">
          <button className="btn btn-primary m-5">
            v0.1
          </button>
        </Link>
      </div>
    </div>
  );
};

export default Home;
