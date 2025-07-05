import React from "react";
import NFTCard from "./NFTCard";
import { NFT } from "../../data/interfaces";

interface NFTRewardsGridProps {
  mockNFTRewards: NFT[];
  mintNFT: (nft: NFT) => void;
  walletConnected: boolean;
}

const NFTRewardsGrid: React.FC<NFTRewardsGridProps> = ({
  mockNFTRewards,
  mintNFT,
  walletConnected,
}) => (
  <div className="nfts-grid">
    {mockNFTRewards.map((nft) => (
      <NFTCard
        key={nft.id}
        nft={nft}
        mintNFT={mintNFT}
        walletConnected={walletConnected}
      />
    ))}
  </div>
);

export default NFTRewardsGrid;
