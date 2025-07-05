import React, { useState } from "react";

const walletKeys = ["lace", "nami", "eternl"] as const;
type WalletKey = typeof walletKeys[number];

interface WalletApi {
  enable: () => Promise<EnabledApi>;
  name?: string;
  icon?: string;
}
interface EnabledApi {
  getUsedAddresses: () => Promise<string[]>;
  getBalance: () => Promise<string>;
  getNetworkId: () => Promise<number>;
}

const getAvailableWallets = (): { key: WalletKey; name: string; icon?: string }[] => {
  const anyWindow = window as any;
  return walletKeys
    .filter((key) => anyWindow.cardano?.[key])
    .map((key) => ({
      key,
      name: anyWindow.cardano[key].name || key.charAt(0).toUpperCase() + key.slice(1),
      icon: anyWindow.cardano[key].icon,
    }));
};

const getWalletApi = (walletKey: WalletKey): WalletApi | undefined => {
  const anyWindow = window as any;
  return anyWindow.cardano?.[walletKey];
};

const MultiWalletConnector: React.FC = () => {
  const [selectedWallet, setSelectedWallet] = useState<WalletKey | null>(null);
  const [connected, setConnected] = useState(false);
  const [addresses, setAddresses] = useState<string[]>([]);
  const [balance, setBalance] = useState<string>("");
  const [networkId, setNetworkId] = useState<number | null>(null);
  const [error, setError] = useState<string | null>(null);

  const availableWallets = getAvailableWallets();

  const connect = async (walletKey: WalletKey) => {
    setError(null);
    setSelectedWallet(walletKey);
    const wallet = getWalletApi(walletKey);
    if (!wallet) {
      setError("Wallet not found. Please install the extension.");
      return;
    }
    try {
      const api = await wallet.enable();
      setConnected(true);
      setAddresses(await api.getUsedAddresses());
      setBalance(await api.getBalance());
      setNetworkId(await api.getNetworkId());
    } catch (e) {
      setError("User denied access or error occurred.");
    }
  };

  return (
    <div>
      <h2>Connect to Cardano Wallet</h2>
      {!connected ? (
        <div>
          {availableWallets.length === 0 && <div>No supported wallets found.</div>}
          {availableWallets.map((wallet) => (
            <button key={wallet.key} onClick={() => connect(wallet.key)}>
              {wallet.icon && <img src={wallet.icon} alt={wallet.name} style={{ width: 24, marginRight: 8 }} />}
              Connect {wallet.name}
            </button>
          ))}
        </div>
      ) : (
        <div>
          <div>
            <strong>Addresses:</strong>
            <ul>
              {addresses.map((addr) => (
                <li key={addr}>{addr}</li>
              ))}
            </ul>
          </div>
          <div>
            <strong>Balance:</strong> {balance}
          </div>
          <div>
            <strong>Network ID:</strong> {networkId}
          </div>
        </div>
      )}
      {error && <div style={{ color: "red" }}>{error}</div>}
    </div>
  );
};

export default MultiWalletConnector;
