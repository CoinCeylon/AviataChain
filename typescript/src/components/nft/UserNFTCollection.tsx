import React from "react";
import { Gift } from "lucide-react"; // Or your icon library
import { UserNFT } from "../../data/interfaces";

interface UserNFTCollectionProps {
  userNFTs: UserNFT[];
}

const UserNFTCollection: React.FC<UserNFTCollectionProps> = ({ userNFTs }) => (
  <div className="bg-white rounded-lg shadow-sm border p-6">
    <h3 className="text-lg font-semibold mb-4 flex items-center">
      <Gift className="w-5 h-5 mr-2 text-purple-600" />
      Your NFT Collection
    </h3>
    <div className="stats-grid">
      {userNFTs.map((nft, index) => (
        <div key={index} className="stat-card primary">
          <div className="text-center mb-3">
            <div className="stat-icon">{nft.image}</div>
            <h3>{nft.name}</h3>
            <p>Minted: {nft.mintedAt}</p>
          </div>
          <div className="text-xs text-gray-500 bg-white rounded p-2 font-mono">
            <p>Token ID: {nft.tokenId}</p>
          </div>
        </div>
      ))}
    </div>
  </div>
);

export default UserNFTCollection;
