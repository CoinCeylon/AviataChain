// types.ts (optional)
export type Flight = {
  id: number;
  from: string;
  to: string;
  date: string;
  miles: number;
  status: "upcoming" | "completed" | "cancelled" | "pending"; // Extend as needed
};

export type Profile = {
  avatarUrl: string;
  name: string;
  email: string;
  memberSince: string;
  tier: string;
};

export type BlockfrostData = {
  balance: string;
  transactions: number;
  stakingRewards: string;
};