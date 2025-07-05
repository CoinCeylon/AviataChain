// ProfileView.tsx
import React from "react";
import { Profile } from "../data/types"; 

type ProfileViewProps = {
  profile: Profile;
};

const ProfileView: React.FC<ProfileViewProps> = ({ profile }) => (
  <div className="stat-card primary">
    <div className="profile-view">
      <h1>Your Profile</h1>
      <div className="profile-card">
        <img
          src={profile.avatarUrl}
          alt="Profile avatar"
          className="profile-avatar"
          width={180}
          height={180}
          style={{ borderRadius: "50%", marginBottom: 16 }}
        />
        <div className="profile-details">
          <h2>{profile.name}</h2>
          <p>{profile.email}</p>
          <p>
            <strong>Member Since:</strong> {profile.memberSince}
          </p>
          <h3>
            <strong>Tier:</strong> {profile.tier}
          </h3>
        </div>
      </div>
    </div>
  </div>
);

export default ProfileView;
