import React from "react";
import NFTRewardsHeader from "../components/nft/NFTRewardsHeader";
import NFTRewardsGrid from "../components/nft/NFTRewardsGrid";
import UserNFTCollection from "../components/nft/UserNFTCollection";

import { NFT, UserNFT } from "../data/interfaces";

interface NFTMintViewProps {
  mockNFTRewards: NFT[];
  userNFTs: UserNFT[]; // Replace 'any' with a proper type if available
  mintNFT: (nft: NFT) => void;
  walletConnected: boolean;
}

const NFTMintView: React.FC<NFTMintViewProps> = ({
  mockNFTRewards,
  userNFTs,
  mintNFT,
  walletConnected,
}) => (
  <div className="nft-view">
    <NFTRewardsHeader />
    <NFTRewardsGrid
      mockNFTRewards={mockNFTRewards}
      mintNFT={mintNFT}
      walletConnected={walletConnected}
    />
    {userNFTs.length > 0 && <UserNFTCollection userNFTs={userNFTs} />}
  </div>
);

export default NFTMintView;
