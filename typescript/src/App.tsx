import React, { useState, useEffect } from 'react';

import ProfileView from "./views/ProfileView";
import FlightsView from "./views/FlightsView";
import WalletView from "./views/WalletView";
import RewardsView from "./views/RewardsView";
import PartnersView from "./views/PartnersView"; 
import TransferView from "./views/TransferView"; 

import { Profile, Flight, BlockfrostData } from "./data/types";

import { NFT, UserNFT, Airline, Transaction, Reward } from "./data/interfaces";

import NFTMintView from './views/NFTMintView';

import { Plane, Wallet, Gift, Users, TrendingUp, Star, MapPin, Calendar, CreditCard, Shield, User, ArrowRightLeft, Globe, AlertCircle, CheckCircle, Award, Trophy } from 'lucide-react';
// CSS styles will be embedded below
import './App.css';

type TabButtonProps = {
  id: string;
  label: string;
  icon: React.ElementType;
  active: boolean;
};

type AirlineId = keyof typeof AIRLINE_POLICIES;

// Mock data for demonstration
const mockFlights : Flight[] = [
  { id: 1, from: 'NYC', to: 'LAX', date: '2025-07-15', miles: 2500, status: 'completed' },
  { id: 2, from: 'LAX', to: 'MIA', date: '2025-07-20', miles: 2200, status: 'upcoming' },
  { id: 3, from: 'MIA', to: 'NYC', date: '2025-08-01', miles: 1100, status: 'upcoming' }
];

const mockRewards = [
  { id: 1, title: 'Free Domestic Flight', cost: 25000, type: 'flight' },
  { id: 2, title: 'Airport Lounge Access', cost: 15000, type: 'service' },
  { id: 3, title: 'Seat Upgrade', cost: 8000, type: 'upgrade' },
  { id: 4, title: 'Hotel Discount 30%', cost: 12000, type: 'hotel' }
];

const mockAirlines: Airline[] = [
  { id: 'AA', name: 'American Airlines', points: 2500, logo: 'US' },
  { id: 'BA', name: 'British Airways', points: 1800, logo: 'GB' },
  { id: 'LH', name: 'Lufthansa', points: 3200, logo: 'DE' },
  { id: 'SQ', name: 'Singapore Airlines', points: 1500, logo: 'SG' },
  { id: 'QR', name: 'Qatar Airways', points: 2100, logo: 'QA' }
];

const partnerAirlines: Airline[] = [
  { id: 'AA', name: 'American Airlines', points: 2500, logo: 'US' },
  { id: 'BA', name: 'British Airways', points: 1800, logo: 'GB' },
  { id: 'LH', name: 'Lufthansa', points: 3200, logo: 'DE' },
  { id: 'SQ', name: 'Singapore Airlines', points: 1500, logo: 'SG' },
  { id: 'QR', name: 'Qatar Airways', points: 2100, logo: 'QA' }
];

 // Token policies for different airlines (in real implementation, these would be actual policy IDs)
  const AIRLINE_POLICIES = {
    'AA' :{ 
      policyId: 'a1b2c3d4e5f6789012345678901234567890123456789012345678901234',
      assetName: '416d65726963616e416972', // "AmericanAir" in hex
      ticker: 'AAPOINTS'
    },
     'BA' :{
      policyId: 'b2c3d4e5f6789012345678901234567890123456789012345678901234aa',
      assetName: '4272697469736841697277617973', // "BritishAirways" in hex
      ticker: 'BAPOINTS'
    },
     'LH' :{ 
      policyId: 'c3d4e5f6789012345678901234567890123456789012345678901234aabb',
      assetName: '4c7566746861736e61', // "Lufthansa" in hex
      ticker: 'LHPOINTS'
    },
     'SQ' :{ 
      policyId: 'd4e5f6789012345678901234567890123456789012345678901234aabbcc',
      assetName: '53696e6761706f7265416972', // "SingaporeAir" in hex
      ticker: 'SQPOINTS'
    },
     'QR' :{ 
      policyId: 'e5f6789012345678901234567890123456789012345678901234aabbccdd',
      assetName: '516174617241697277617973', // "QatarAirways" in hex
      ticker: 'QRPOINTS'
    }
  };

// Mock NFT rewards data
const mockNFTRewards = [
  { id: 1, name: 'Gold Status Badge', cost: 5000, rarity: 'Legendary', image: '🏆' },
  { id: 2, name: 'Lounge Access Pass', cost: 3000, rarity: 'Epic', image: '🏨' },
  { id: 3, name: 'Priority Boarding', cost: 1500, rarity: 'Rare', image: '⚡' },
  { id: 4, name: 'Extra Baggage', cost: 1000, rarity: 'Common', image: '🧳' }
];

function App() {
  const [walletConnected, setWalletConnected] = useState(false);
  const [walletAddress, setWalletAddress] = useState('');
  const [userMiles, setUserMiles] = useState(42350);
  const [userTier, setUserTier] = useState('Gold');
  const [activeTab, setActiveTab] = useState('dashboard');
  const [selectedMintAirline, setSelectedMintAirline] = useState('');
  const [selectedFlight, setSelectedFlight] = useState(null);
  const [blockfrostData, setBlockfrostData] = useState<BlockfrostData | undefined>(undefined);
 // Blockfrost API configuration
  const BLOCKFROST_API_KEY = 'your-blockfrost-api-key';
  const BLOCKFROST_BASE_URL = 'https://cardano-preview.blockfrost.io/api/v0';
  const [selectedAirline, setSelectedAirline] = useState<string>('');
  const [selectedFromAirline, setSelectedFromAirline] = useState('');
  const [selectedToAirline, setSelectedToAirline] = useState('');
  const [transferAmount, setTransferAmount] = useState('');
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [userNFTs, setUserNFTs] = useState<UserNFT[]>([]);
  const [isTransferring, setIsTransferring] = useState(false);

// This function will receive data from the child
  const walletConnectedChange = (connected: boolean) => {
    setWalletConnected(connected);
  };
  
  /*
    const transferMiles = async () => {
    if (!transferAmount || transferAmount <= 0) return;
    
    const amount = parseInt(transferAmount);
    if (userMiles[selectedFromAirline] < amount) {
      alert('Insufficient miles in source airline');
      return;
    }

    // Simulate blockchain transaction
    setUserMiles(prev => ({
      ...prev,
      [selectedFromAirline]: prev[selectedFromAirline] - amount,
      [selectedToAirline]: prev[selectedToAirline] + Math.floor(amount * 0.8) // 20% conversion fee
    }));
    
    setTransferAmount('');
    alert('Miles transferred successfully! (20% conversion fee applied)');
  };
  
  */
  

  // Simulate wallet connection
  const connectWallet = async () => {
    try {
      // In a real implementation, this would use MeshJS wallet connection
      setWalletConnected(true);
      setWalletAddress('addr1qxy2lpan99fcnyrz4uszcj0fzf8kjf4rnkfxz5dplqzxj');
      
      // Simulate fetching blockchain data via Blockfrost
      setTimeout(() => {
        setBlockfrostData({
          balance: '1,250 ADA',
          transactions: 23,
          stakingRewards: '45.8 ADA'
        });
      }, 1000);
    } catch (error) {
      console.error('Wallet connection failed:', error);
    }
  };

  const disconnectWallet = () => {
    setWalletConnected(false);
    setWalletAddress('');
    setBlockfrostData(undefined);
  };

 // Simulate mile transfer using Cardano blockchain
  const handleTransfer = async () => {
    if (!selectedFromAirline || !selectedToAirline || !transferAmount) {
      alert('Please fill all fields');
      return;
    }

    setIsTransferring(true);
    
    // Simulate blockchain transaction
    setTimeout(() => {
      const newTransaction = {
        id: Date.now(),
        from: mockAirlines.find(a => a.id === selectedFromAirline)?.name,
        to: mockAirlines.find(a => a.id === selectedToAirline)?.name,
        amount: parseInt(transferAmount),
        timestamp: new Date().toLocaleString(),
        txHash: `tx_${Math.random().toString(36).substr(2, 9)}`,
        status: 'Confirmed'
      };
      
      setTransactions(prev => [newTransaction, ...prev]);
      setTransferAmount('');
      setIsTransferring(false);
      
      // In real implementation: Use Blockfrost API and Mesh SDK
      console.log('Transaction submitted to Cardano blockchain');
    }, 2000);
  };

function showModal(content: string): void {
  // Remove any existing modal
  const existingModal = document.getElementById('custom-modal');
  if (existingModal) {
    existingModal.remove();
  }

  // Create modal container
  const modal = document.createElement('div');
  modal.id = 'custom-modal';
  modal.className = "nft-card";
  modal.style.position = 'fixed';
  modal.style.top = '0';
  modal.style.left = '0';
  modal.style.width = '100vw';
  modal.style.height = '100vh';
  modal.style.background = 'rgba(0,0,0,0.5)';
  modal.style.display = 'flex';
  modal.style.alignItems = 'center';
  modal.style.justifyContent = 'center';
  modal.style.zIndex = '1000';

  // Create modal content box
  const modalContent = document.createElement('div');
  modalContent.className = 'stat-card primary';
  modalContent.style.background = '#fff';
  modalContent.style.padding = '24px';
  modalContent.style.color = '#000';
  modalContent.style.borderRadius = '8px';
  modalContent.style.minWidth = '320px';
  modalContent.innerHTML = content;

  // Optional: Add a close button
  const closeButton = document.createElement('button');
  closeButton.className = 'nft-button';
  closeButton.textContent = 'Close';
  closeButton.style.marginTop = '16px';
  closeButton.onclick = () => modal.remove();

  modalContent.appendChild(closeButton);
  modal.appendChild(modalContent);
  document.body.appendChild(modal);
}

  // Simulate NFT minting
  const mintNFT = async (nft: NFT) => {
    if (!walletConnected) {
      alert('Please connect your wallet first');
      return;
    }

    const newNFT = {
      ...nft,
      mintedAt: new Date().toLocaleString(),
      tokenId: `token_${Math.random().toString(36).substr(2, 9)}`
    };
    
    setUserNFTs(prev => [...prev, newNFT]);
	showModal(`
	<div className="stat-card primary">
  <h3 >Minting NFT</h3>
  <p>Creating your NTF Token</p>
  <div class="loading" style="margin: 20px auto;"></div>
  <p>Processing on Cardano blockchain...</p>
  </div>
`);
    console.log('NFT minted on Cardano blockchain via Mesh SDK');
  };

  const redeemReward = (reward: Reward) => {
    if (userMiles >= reward.cost) {
      setUserMiles(prev => prev - reward.cost);
	  showModal(`
	<div className="stat-card primary">
  <h3>Redeem Airline Rewards</h3>
  <p>Successfully redeemed: ${reward.title}</p>
  <div class="loading" style="margin: 20px auto;"></div>  
  </div>
`);
     // alert(`Successfully redeemed: ${reward.title}`);
    } else {
		 showModal(`
	<div className="stat-card primary">
  <h3>Redeem Airline Rewards</h3>
  <p>Insufficient miles for this reward</p>
  <div class="loading" style="margin: 20px auto;"></div>  
  </div>
`);
     // alert('Insufficient miles for this reward');
    }
  };

  const TabButton = ({ id, label, icon: Icon, active }: TabButtonProps) => (
    <button
      onClick={() => setActiveTab(id)}
      className={`tab-button ${active ? 'active' : ''}`}
    >
      <Icon size={18} />
      <span>{label}</span>
    </button>
  );

  
  const mockProfile: Profile = {
     name: "Wasana Delpage",
     email: "wasana.delpage@email.com",
     memberSince: "2020-03-15",
     tier: "Gold",
     avatarUrl: "https://i.pravatar.cc/200?img=18",
};



  const DashboardView = () => (
    <div className="dashboard">
      <div className="stats-grid">
        <div className="stat-card primary">
          <div className="stat-icon">
            <Star />
          </div>
          <div className="stat-content">
            <h3>{userMiles.toLocaleString()}</h3>
            <p>Total Miles</p>
          </div>
        </div>
        
        <div className="stat-card secondary">
          <div className="stat-icon">
            <TrendingUp />
          </div>
          <div className="stat-content">
            <h3>{userTier}</h3>
            <p>Tier Status</p>
          </div>
        </div>
        
        <div className="stat-card tertiary">
          <div className="stat-icon">
            <Plane />
          </div>
          <div className="stat-content">
            <h3>3</h3>
            <p>Flights This Year</p>
          </div>
        </div>
        
        <div className="stat-card quaternary">
          <div className="stat-icon">
            <Gift />
          </div>
          <div className="stat-content">
            <h3>8</h3>
            <p>Available Rewards</p>
          </div>
        </div>
      </div>

      <div className="recent-activity">
        <h3>Recent Flight Activity</h3>
        <div className="activity-list">
          {mockFlights.slice(0, 3).map(flight => (
            <div key={flight.id} className="activity-item">
              <div className="activity-icon">
                <Plane size={16} />
              </div>
              <div className="activity-details">
                <p className="activity-title">{flight.from} → {flight.to}</p>
                <p className="activity-meta">{flight.date} • {flight.miles} miles</p>
              </div>
              <div className={`activity-status ${flight.status}`}>
                {flight.status}
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );

 

 

  return (
    <div className="App">
      <header className="app-header">
        <div className="header-content">
          <div className="logo">
            <Plane size={32} />
            <h1>AviataChain - Unified Airline Loyalty Platform</h1>
          </div>
          <div className="header-actions">
            <div className="user-info">
              <span className="user-miles">{userMiles.toLocaleString()} miles</span>
              <span className="user-tier">{userTier}</span>
            </div>
            <div className={`wallet-indicator ${walletConnected ? 'connected' : 'disconnected'}`}>
              <Wallet size={16} />
              <span>{walletConnected ? 'Connected' : 'Not Connected'}</span>
            </div>
          </div>
        </div>
      </header>

      <nav className="app-nav">
        <TabButton id="dashboard" label="Dashboard" icon={TrendingUp} active={activeTab === 'dashboard'} />
		<TabButton id="profile" label="Profile" icon={User} active={activeTab === 'profile'} />
        <TabButton id="flights" label="Flights" icon={Plane} active={activeTab === 'flights'} />
        <TabButton id="rewards" label="Rewards" icon={Gift} active={activeTab === 'rewards'} />
        <TabButton id="wallet" label="Wallet" icon={Wallet} active={activeTab === 'wallet'} />
		<TabButton id="partners" label="Partners" icon={Plane} active={activeTab === 'partners'} />
		<TabButton id="transfer" label="Transfer" icon={Plane} active={activeTab === 'transfer'} />
		<TabButton id="NFTMint" label="Travel NFT" icon={Plane} active={activeTab === 'NFTMint'} />
      </nav>

      <main className="app-main">
        {activeTab === 'dashboard' && <DashboardView />}
		{activeTab === 'profile' && <ProfileView profile={mockProfile} />}
        {activeTab === 'flights' && <FlightsView flights={mockFlights} />}
        {activeTab === 'rewards' && <RewardsView mockRewards={mockRewards} userMiles={userMiles} redeemReward={redeemReward} />}
        {activeTab === 'wallet' && <WalletView walletConnected={walletConnected} walletAddress={walletAddress} blockfrostData={blockfrostData} connectWallet={connectWallet} disconnectWallet={disconnectWallet} onWalletConnectedChange={walletConnectedChange} />}
		{activeTab === 'partners' && <PartnersView mockAirlines={mockAirlines} />}
		{activeTab === 'transfer' && <TransferView mockAirlines={mockAirlines} />}
		{activeTab === 'NFTMint' &&  <NFTMintView mockNFTRewards={mockNFTRewards} userNFTs={userNFTs} mintNFT={mintNFT} walletConnected={walletConnected} />}
      </main>

      <footer className="app-footer">
        <p>Powered by Cardano Blockchain • Blockfrost API • MeshJS - #Heroes Team</p>
      </footer>
    </div>
  );
}

export default App;