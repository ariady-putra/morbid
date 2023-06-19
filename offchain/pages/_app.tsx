import "../styles/globals.css";
import type { AppProps } from "next/app";
import { StoreProvider } from "easy-peasy";
import store from "../utils/store";
import { useEffect, useState } from "react";

function MyApp({ Component, pageProps }: AppProps) {
  const StoreProviderOverride = StoreProvider as any;

  const [loaded, setLoaded] = useState(false);

  useEffect(() => {
    setLoaded(true);
  }, []);

  return !loaded ? (
    <></>
  ) : (
    <StoreProviderOverride store={store}>
      <Component {...pageProps} />
    </StoreProviderOverride>
  );
}

export default MyApp;
