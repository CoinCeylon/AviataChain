// WalletView.tsx
import React from 'react';
import { Shield, CreditCard, Gift, Wallet } from "lucide-react";
import { BlockfrostData } from "../data/types"; // Adjust path if needed

import LaceWalletConnector from "../components/LaceWalletConnector";
import {
  ConnectWalletList,
  ConnectWalletButton,
} from "@cardano-foundation/cardano-connect-with-wallet";

type WalletViewProps = {
  walletConnected: boolean;
  walletAddress: string;
  blockfrostData?: BlockfrostData;
  connectWallet: () => void;
  disconnectWallet: () => void;
  // Optionally, add a callback if you want to sync connection state from LaceWalletConnector
  onWalletConnectedChange?: (connected: boolean) => void;
};

const WalletView: React.FC<WalletViewProps> = ({
  walletConnected,
  walletAddress,
  blockfrostData,
  connectWallet,
  disconnectWallet,
  onWalletConnectedChange,
}) => {
  // This handler is passed to LaceWalletConnector if you want to sync state
  const handleWalletConnectedChange = (connected: boolean) => {
    if (onWalletConnectedChange) {
      onWalletConnectedChange(connected);
	  walletConnected = connected;
	 
    }
  };

  return (
    <div className="wallet-view">
      <h3>Default Blockchain Wallet (Lace)</h3>      
      <div style={{ maxWidth: 800, margin: "40px auto" }}>       
        <LaceWalletConnector
          walletConnectedChange={handleWalletConnectedChange}
        />
      </div>	  
    </div>
  );
};

export default WalletView;
