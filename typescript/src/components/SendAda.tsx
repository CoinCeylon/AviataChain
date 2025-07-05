import { useState } from 'react';
import { useWallet } from '@meshsdk/react';
import { Transaction, KoiosProvider } from '@meshsdk/core';

const koios = new KoiosProvider('preview'); // Use 'mainnet' for production

export default function SendAda() {
  const { connected, wallet } = useWallet();
  const [txHash, setTxHash] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  async function sendAda() {
    if (!wallet) return;
    setLoading(true);
    try {
      const tx = new Transaction({ initiator: wallet }).sendLovelace(
        { address: 'addr_test1qrp2cdalwhyd3pa8cgpqas2pakv4z0aamadu9u2e6dd57f2fza2gpdsahnggz0k2yg086fh977sppc8cjvds6l7lspzq0v445n', datum: undefined }, 
        '1000000'
      );
      const unsignedTx = await tx.build();
      const signedTx = await wallet.signTx(unsignedTx);
      const submittedTxHash = await wallet.submitTx(signedTx);
      setTxHash(submittedTxHash);
    } catch (err) {
      alert('Transaction failed: ' + (err as Error).message);
    }
    setLoading(false);
  }

  if (!connected) return null;

  return (
    <div>
      <button className="connect-button" onClick={sendAda} disabled={loading}>
        {loading ? 'Sending...' : 'Send 1 ADA'}
      </button>
      {txHash && (
        <div style={{ marginTop: 12 }}>
          Transaction submitted: <code>{txHash}</code>
        </div>
      )}
    </div>
  );
}
