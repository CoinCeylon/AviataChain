export interface NFT {
  id: number;
  name: string;
  cost: number;
  rarity : string;
  image: string;
  mintedAt?: string;
  tokenId?: string;
  // Add other relevant fields
}


export interface UserNFT {
  name: string;
  image: React.ReactNode;
  mintedAt: string;
  tokenId: string;
}

export interface Reward {
  id: number; 
  title: string;
  cost: number;
  type: string;
 }
 
export interface Transaction {
  id: number;
  from?: string;
  to?: string;
  amount: number;
  timestamp: string;
  txHash: string;
  status: string;
}

export interface Airline {
  id: string;
  name: string;
  points: number;
  logo: string;
}