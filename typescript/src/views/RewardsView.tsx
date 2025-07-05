import React from "react";
import { Gift } from 'lucide-react';
import { Reward } from "../data/interfaces";

interface RewardsViewProps {
  mockRewards: Reward[];
  userMiles: number;
  redeemReward: (reward: Reward) => void;
}

const RewardsView: React.FC<RewardsViewProps> = ({ mockRewards, userMiles, redeemReward }) => (
  <div className="rewards-view">
    <h3>Available Rewards</h3>
    <div className="rewards-grid">
      {mockRewards.map(reward => (
        <div key={reward.id} className="reward-card">
          <div className="reward-icon">
            <Gift />
          </div>
          <div className="reward-content">
            <h4>{reward.title}</h4>
            <p className="reward-cost">{reward.cost.toLocaleString()} miles</p>
            <button
              className={`reward-button ${userMiles >= reward.cost ? 'available' : 'unavailable'}`}
              onClick={() => redeemReward(reward)}
              disabled={userMiles < reward.cost}
            >
              {userMiles >= reward.cost ? 'Redeem' : 'Insufficient Miles'}
            </button>
          </div>
        </div>
      ))}
    </div>
  </div>
);

export default RewardsView;
