// FlightsView.tsx
import React from "react";
import { Plane, Calendar, Star } from 'lucide-react'; 
import { Flight } from "../data/types"; 

type FlightsViewProps = {
  flights: Flight[];
};

const FlightsView: React.FC<FlightsViewProps> = ({ flights }) => (
  <div className="flights-view">
    <h3>Flight History & Upcoming</h3>
    <div className="flights-grid">
      {flights.map(flight => (
        <div key={flight.id} className="flight-card">
          <div className="flight-header">
            <div className="flight-route">
              <span className="airport">{flight.from}</span>
              <Plane size={16} className="flight-icon" />
              <span className="airport">{flight.to}</span>
            </div>
            <div className={`flight-status ${flight.status}`}>
              {flight.status}
            </div>
          </div>
          <div className="flight-details">
            <div className="flight-info">
              <Calendar size={14} />
              <span>{flight.date}</span>
            </div>
            <div className="flight-miles">
              <Star size={14} />
              <span>{flight.miles} miles</span>
            </div>
          </div>
        </div>
      ))}
    </div>
  </div>
);

export default FlightsView;
