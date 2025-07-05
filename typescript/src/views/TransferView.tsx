// TransferView.tsx
import React, { useState } from "react";
import { Plane } from 'lucide-react';
import { Airline } from "../data/interfaces";

interface TransferViewProps {
  mockAirlines: Airline[];
}

// Inline styles (replace with CSS module or global styles as needed)
const styles: { [key: string]: React.CSSProperties } = {
  container: {
    background: "rgba(0,0,0,0.3)",
    backdropFilter: "blur(8px)",
    border: "1px solid rgba(255,255,255,0.1)",
    borderRadius: "1rem",
    padding: "2rem",
    maxWidth: 600,
    margin: "2rem auto",
    color: "#fff",
  },
  label: {
    display: "block",
    fontSize: "1rem",
    fontWeight: 500,
    marginBottom: 8,
  },
  select: {
    width: "100%",
    background: "rgba(0,0,0,0.3)",
    border: "1px solid rgba(255,255,255,0.2)",
    borderRadius: 8,
    padding: "0.75rem 1rem",
    color: "#fff",
    outline: "none",
    marginBottom: 0,
  },
  input: {
    width: "100%",
    background: "rgba(0,0,0,0.3)",
    border: "1px solid rgba(255,255,255,0.2)",
    borderRadius: 8,
    padding: "0.75rem 1rem",
    color: "#fff",
    outline: "none",
    marginBottom: 0,
  },
  button: {
    background: "linear-gradient(90deg, #0538AF 0%, #3B82F6 100%)",
    color: "#fff",
    border: "none",
    borderRadius: 8,
    padding: "0.75rem 2rem",
    fontWeight: 600,
    fontSize: "1rem",
    cursor: "pointer",
    marginTop: "1.5rem",
    transition: "background 0.2s",
    display: "flex",
    alignItems: "center",
    gap: 8,
  },
  buttonDisabled: {
    opacity: 0.5,
    cursor: "not-allowed",
  },
  error: {
    color: "#F87171",
    marginTop: 12,
    fontSize: "0.95rem",
    textAlign: "center",
  },
  success: {
    color: "#34D399",
    marginTop: 12,
    fontSize: "0.95rem",
    textAlign: "center",
  },
};

const TransferView: React.FC<TransferViewProps> = ({ mockAirlines }) => {
  const [selectedFromAirline, setSelectedFromAirline] = useState<string>("");
  const [selectedToAirline, setSelectedToAirline] = useState<string>("");
  const [miles, setMiles] = useState<string>("");
  const [error, setError] = useState<string>("");
  const [success, setSuccess] = useState<string>("");

  // Reset success when inputs change
  React.useEffect(() => {
    setSuccess("");
    setError("");
  }, [selectedFromAirline, selectedToAirline, miles]);

  const handleTransfer = () => {
    setError("");
    setSuccess("");
    if (!selectedFromAirline || !selectedToAirline) {
      setError("Please select both airlines.");
      return;
    }
    if (selectedFromAirline === selectedToAirline) {
      setError("Cannot transfer miles to the same airline.");
      return;
    }
    const milesNum = Number(miles);
    if (!miles || isNaN(milesNum) || milesNum <= 0) {
      setError("Please enter a valid number of miles.");
      return;
    }
    const from = mockAirlines.find(a => a.id === selectedFromAirline);
    if (!from || milesNum > from.points) {
      setError("Not enough miles in the selected airline.");
      return;
    }
    setSuccess(
      `Successfully transferred ${milesNum} miles from ${from.name} to ${
        mockAirlines.find(a => a.id === selectedToAirline)?.name || ""
      }!`
    );
    // Here you would trigger your smart contract/transfer logic
  };

  const isButtonDisabled =
    !selectedFromAirline ||
    !selectedToAirline ||
    !miles ||
    selectedFromAirline === selectedToAirline;

  return (
    <div className="transfer-view" style={styles.container}>
      <div style={{ textAlign: "center", marginBottom: 32 }}>
        <h3 style={{ fontSize: "1.5rem", fontWeight: 700 }}>
          <Plane size={24} style={{ verticalAlign: "middle", marginRight: 8 }} />
          Transfer Miles
        </h3>
        <p style={{ color: "#d1d5db" }}>
          Move miles between airlines with smart contracts
        </p>
      </div>
      <div style={{ marginBottom: 24 }}>
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 24 }}>
          <div>
            <label style={styles.label}>From Airline</label>
            <select
              value={selectedFromAirline}
              onChange={e => setSelectedFromAirline(e.target.value)}
              style={styles.select}
              aria-label="From Airline"
            >
              <option value="">-- Select an airline --</option>
              {mockAirlines.map(airline => (
                <option key={airline.id} value={airline.id}>
                  {airline.name} ({airline.points} pts)
                </option>
              ))}
            </select>
          </div>
          <div>
            <label style={styles.label}>To Airline</label>
            <select
              value={selectedToAirline}
              onChange={e => setSelectedToAirline(e.target.value)}
              style={styles.select}
              aria-label="To Airline"
            >
              <option value="">-- Select an airline --</option>
              {mockAirlines.map(airline => (
                <option key={airline.id} value={airline.id}>
                  {airline.name} ({airline.points} pts)
                </option>
              ))}
            </select>
          </div>
        </div>
        <div style={{ marginTop: 24 }}>
          <label style={styles.label}>Miles to Transfer</label>
          <input
            type="number"
            min={1}
            value={miles}
            onChange={e => setMiles(e.target.value)}
            placeholder="Enter miles"
            style={styles.input}
            aria-label="Miles to Transfer"
          />
        </div>
        <button
          style={{
            ...styles.button,
            ...(isButtonDisabled ? styles.buttonDisabled : {}),
          }}
          onClick={handleTransfer}
          disabled={isButtonDisabled}
        >
          <Plane size={18} />
          Transfer Miles
        </button>
        {error && <div style={styles.error}>{error}</div>}
        {success && <div style={styles.success}>{success}</div>}
      </div>
    </div>
  );
};

export default TransferView;
