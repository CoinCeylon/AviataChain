import React from 'react';
import { CardanoWallet, useWallet } from '@meshsdk/react';

const WalletConnector: React.FC = () => {
  const { connected, wallet, connecting, name , disconnect} = useWallet();

  return (
    <div className="wallet-connector">
      <h2>Wallet Connection</h2>
      {!connected ? (
        <div>
          <p>Connect your Cardano wallet to get started</p>
          <CardanoWallet />
          {connecting && <p>Connecting...</p>}
        </div>
      ) : (
        <div>
          <p> Connected to {name}</p>
          <button 
            onClick={() => disconnect()}
            className="disconnect-btn"
          >
            Disconnect
          </button>
        </div>
      )}
    </div>
  );
};

export default WalletConnector;
