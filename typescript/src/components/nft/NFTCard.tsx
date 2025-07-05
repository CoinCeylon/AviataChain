import React from "react";
import { NFT } from "../../data/interfaces";

interface NFTCardProps {
  nft: NFT;
  mintNFT: (nft: NFT) => void;
  walletConnected: boolean;
}

const NFTCard: React.FC<NFTCardProps> = ({ nft, mintNFT, walletConnected }) => (
  <div className="nft-card">
    <div className="p-4 text-center bg-gradient-to-br from-blue-50 to-purple-50">
      <div className="nft-icon">{nft.image}</div>
      <h3 className="font-semibold text-gray-900 mb-1">{nft.name}</h3>
      <span
        className={`px-2 py-1 rounded-full text-xs font-medium ${
          nft.rarity === "Legendary"
            ? "bg-yellow-100 text-yellow-800"
            : nft.rarity === "Epic"
            ? "bg-purple-100 text-purple-800"
            : nft.rarity === "Rare"
            ? "bg-blue-100 text-blue-800"
            : "bg-gray-100 text-gray-800"
        }`}
      >
        {nft.rarity}
      </span>
    </div>
    <div className="nft-cost">
      <div className="flex items-center justify-between mb-3">
        <span className="text-lg font-bold text-green-600">
          {nft.cost.toLocaleString()}
        </span>
        <span className="text-sm text-gray-500">Miles</span>
      </div>
      <button
        onClick={() => mintNFT(nft)}
        disabled={!walletConnected}
        className="nft-button"
      >
        Mint NFT
      </button>
    </div>
  </div>
);

export default NFTCard;
