import React from "react";
import { Plane } from 'lucide-react';
import { Airline } from "../data/interfaces";

// Define the props expected by the PartnersView component
interface PartnersViewProps {
  mockAirlines: Airline[];
}

const PartnersView: React.FC<PartnersViewProps> = ({ mockAirlines }) => (
  <div className="rewards-view">
    <h3>Available Partners</h3>
    <div className="rewards-grid">
      {mockAirlines.map(airline => (
        <div key={airline.id} className="reward-card">
          <div className="reward-icon">
            <Plane />
          </div>
          <div className="reward-content">
            <h4>{airline.name}</h4>
            <p className="reward-cost">{airline.points.toLocaleString()} miles</p>
          </div>
        </div>
      ))}
    </div>
  </div>
);

export default PartnersView;
