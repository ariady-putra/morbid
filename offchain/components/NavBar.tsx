import Home from "./Home";
import WalletConnect from "./WalletConnect";

const NavBar = () => {
  return (
    <>
      <div className="flex-1">
        <Home />
      </div>

      <div className="flex-none">
        <WalletConnect />
      </div>
    </>
  );
};

export default NavBar;
