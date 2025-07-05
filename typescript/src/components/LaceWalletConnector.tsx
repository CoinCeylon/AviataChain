// LaceWalletConnector.tsx
import React, { useEffect, useState } from "react";
import { Shield, CreditCard, Gift, Wallet } from "lucide-react"; 
import * as CSL from '@emurgo/cardano-serialization-lib-browser';
import axios from 'axios';

const BLOCKFROST_API_KEY = 'preview9zBw0s97SbFzj154sE8RydQ5M50J9Zkp'; // Replace with your actual API key
const BASE_URL = 'https://cardano-preview.blockfrost.io/api/v0';

// Local interface for just what you need
interface LaceApi {
  enable: () => Promise<LaceEnabledApi>;
}

type LaceWalletConnectorProps = {
  walletConnectedChange: (onConnect: boolean) => void;
};

interface LaceEnabledApi {
  getUsedAddresses: () => Promise<string[]>;
  getBalance: () => Promise<string>;
  getNetworkId: () => Promise<number>;
}

const getLaceApi = (): LaceApi | undefined => {
  // Only access window.cardano locally and type-guard for Lace
  const anyWindow = window as unknown as { cardano?: any };
  return anyWindow.cardano?.lace;
};

const LaceWalletConnector: React.FC<LaceWalletConnectorProps> = ({ walletConnectedChange }) => {
  const [addresses, setAddresses] = useState<string[]>([]);
  const [defaultAddress, setDefaultAddress] = useState<string>("")
  const [networkName, setNetworkName] = useState<string>("Testnet")
  const [balance, setBalance] = useState<string>("");
  const [networkId, setNetworkId] = useState<number | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [connected, setConnected] = useState(false);
  const [txCount, setTxCount] = useState<number | null>(null);
  
  const connect = async () => {
    setError(null);
    const lace = getLaceApi();
    if (!lace) {
      setError("Lace wallet not found. Please install the Lace extension.");
      return;
    }
    try {
      const api = await lace.enable();
      setConnected(true);
      
      walletConnectedChange(true);
	  // Get used addresses (returns an array of hex-encoded addresses)
      const usedAddresses = await api.getUsedAddresses();

      setDefaultAddress(usedAddresses[0]); // This is your main address

      setAddresses(await api.getUsedAddresses());

	  // Get the balance in lovelace (as a hex string)
      const balanceHex: string = await api.getBalance();
	    // Convert hex to decimal
 //const balanceLovelace = BigInt(balanceHex).toString();
	// alert( balanceLovelace)
	 // const balanceAda = Number(balanceLovelace) / 1_000_000;
//setBalance(await );
      setBalance(await api.getBalance());
	 // setBalance(balanceAda.toString());
      setNetworkId(await api.getNetworkId());
	  const nw = await api.getNetworkId();
	  if (networkId === 1) {
          setNetworkName("Mainnet");
      } else {
          setNetworkName("Testnet");
      }
	  setTxCount(await getTransactionCount(defaultAddress));
	  
    } catch (e) {
      setError("User denied access or error occurred.");
    }
  };
  
  const handleDisconnect = () => {
     setConnected(false);
	 walletConnectedChange(false);
     setAddresses([]);
     setBalance("");
     // setRewards("");
     //setTransactions([]);
     setNetworkId(null);
     // setSelectedWallet(null);
};

async function getTransactionCount(address: string): Promise<number> {
  let page = 1;
  let count = 0;
  const pageSize = 100;

  while (true) {
    const response = await axios.get(
      `${BASE_URL}/addresses/${address}/transactions`,
      {
        headers: { project_id: BLOCKFROST_API_KEY },
        params: { count: pageSize, page }
      }
    );
    const txs = response.data;
    count += txs.length;
    if (txs.length < pageSize) break;
    page++;
  }
  return count;
}

async function getStakingRewards(stakeAddress: string): Promise<{
  rewards_sum: string;
  withdrawals_sum: string;
  withdrawable_amount: string;
}> {
  const response = await axios.get(
    `${BASE_URL}/accounts/${stakeAddress}`,
    {
      headers: { project_id: BLOCKFROST_API_KEY }
    }
  );
  // The response includes rewards_sum, withdrawals_sum, withdrawable_amount (all in Lovelace)
  return {
    rewards_sum: response.data.rewards_sum,
    withdrawals_sum: response.data.withdrawals_sum,
    withdrawable_amount: response.data.withdrawable_amount
  };
}

  return (
   <div className="wallet-view">     
	  <div>
        {connected ? (
		 <div className="wallet-connected">
	       <div className="wallet-info">
            <div className="wallet-address">
            <Shield size={16} />
            <span>
               <strong>Addresses: {defaultAddress.slice(0, 12)}...{defaultAddress.slice(-8)}</strong>           
            </span>
          </div>
          <button onClick={handleDisconnect} className="disconnect-button">
            Disconnect
          </button>
        </div>   
     
        <div className="blockchain-stats">
            <div className="blockchain-stat">
              <h4>ADA Balance</h4>
              <p>{balance}</p>
            </div>
            <div className="blockchain-stat">
              <h4>Transactions</h4>
              <p>{txCount}</p>
            </div>
            <div className="blockchain-stat">
              <h4>Staking Rewards</h4>
              <p>0</p>
            </div>
			<div className="blockchain-stat">
              <h4>Cardano Network</h4>
              <p>{networkName}</p>
            </div>
          </div>
      
	    <div className="wallet-actions">
          <button className="wallet-action-button primary">
            <CreditCard size={16} />
            Transfer Miles to ADA
          </button>
          <button className="wallet-action-button secondary">
            <Gift size={16} />
            Stake for Rewards
          </button>
        </div>
      </div>
	   ) : (
      <div className="wallet-disconnected">
        <div className="wallet-prompt">
          <Wallet size={48} />
          <h4>Connect Your Cardano Wallet</h4>
          <p>
            Connect your wallet to access blockchain features and earn additional rewards
          </p>
          <button onClick={connect} disabled={connected} className="connect-button">
            Connect Wallet
          </button>
        </div>
      </div>
    )}
    </div>
 </div>
  );
};

export default LaceWalletConnector;