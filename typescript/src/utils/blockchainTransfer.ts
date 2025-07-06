// utils/blockchainTransfer.ts
import axios from 'axios';
import {
  Address,
  TransactionBuilder,
  TransactionBuilderConfigBuilder,
  TransactionUnspentOutput,
  TransactionUnspentOutputs,
  TransactionOutput,
  LinearFee,
  BigNum,
  TransactionWitnessSet,
  Transaction,
  TransactionHash,
  TransactionInput,
  TransactionOutputs,
  Value,
  AssetName,
  MultiAsset,
  PrivateKey,
  ScriptHash, Assets, TransactionBody,  Vkeywitnesses, make_vkey_witness,
}  from '@emurgo/cardano-serialization-lib-browser';

const BLOCKFROST_API_URL = 'https://cardano-preview.blockfrost.io/api/v0';

interface TransferMilesParams {
  senderAddress: string;
  senderPrivateKeyHex: string;
  receiverAddress: string;
  amount: number;
  blockfrostApiKey: string;
  policyId: string;
  assetName: string;
}

// Helper: fetch UTXOs for an address
async function getUtxos(address: string, projectId: string) {
  const response = await axios.get(`${BLOCKFROST_API_URL}/addresses/${address}/utxos`, {
    headers: { project_id: projectId },
  });
  return response.data;
}

// Helper: submit signed transaction (CBOR hex)
async function submitTx(signedTxHex: string, projectId: string) {
  const response = await axios.post(
    `${BLOCKFROST_API_URL}/tx/submit`,
    Buffer.from(signedTxHex, 'hex'),
    {
      headers: {
        'Content-Type': 'application/cbor',
        project_id: projectId,
      },
    }
  );
  return response.data;
}

export async function transferMilesOnChain({
  senderAddress,
  senderPrivateKeyHex,
  receiverAddress,
  amount,
  blockfrostApiKey,
  policyId,
  assetName,
}: TransferMilesParams): Promise<string> {
  // 1. Fetch UTXOs
  const utxosData = await getUtxos(senderAddress, blockfrostApiKey);

  if (!utxosData.length) {
    throw new Error('No UTXOs found for sender address');
  }

  // 2. Setup Transaction Builder Config
  const txBuilderCfg = TransactionBuilderConfigBuilder.new()
    .fee_algo(LinearFee.new(BigNum.from_str('44'), BigNum.from_str('155381')))
    .coins_per_utxo_byte(BigNum.from_str('4310'))
    .pool_deposit(BigNum.from_str('500000000'))
    .key_deposit(BigNum.from_str('2000000'))
    .max_value_size(5000)
    .max_tx_size(16384)
    .build();

  const txBuilder = TransactionBuilder.new(txBuilderCfg);

  // 3. Add inputs from UTXOs
  for (const utxo of utxosData) {
  const input = TransactionUnspentOutput.from_bytes(Buffer.from(utxo.cbor, 'hex'));
  txBuilder.add_key_input(
    Address.from_bech32(senderAddress),
    input.input(),
    input.output().amount()
  );
}

  // 4. Create MultiAsset for airline miles token
  const multiAsset = MultiAsset.new();
  const assetNameObj = AssetName.new(Buffer.from(assetName));
  const policyScriptHash = ScriptHash.from_bytes(Buffer.from(policyId, 'hex'));

  let assets = multiAsset.get(policyScriptHash);
if (!assets) {
  assets = Assets.new();
  multiAsset.insert(policyScriptHash, assets); // Use insert, not set
}
  assets.insert(assetNameObj, BigNum.from_str(amount.toString()));

  // 5. Create output value (minimum ADA + token)
  const minAda = BigNum.from_str('1500000'); // Adjust min ADA as needed
  const value = Value.new(minAda);
  value.set_multiasset(multiAsset);

  txBuilder.add_output(TransactionOutput.new(Address.from_bech32(receiverAddress), value));

  // 6. Add change output if needed
  txBuilder.add_change_if_needed(Address.from_bech32(senderAddress));

  // 7. Build transaction body
  const txBody = txBuilder.build();

  // 8. Sign transaction
  const txHash = TransactionHash.from_bytes(txBody.to_bytes());
  const witnesses = TransactionWitnessSet.new();

 const privateKey = PrivateKey.from_normal_bytes(Buffer.from(senderPrivateKeyHex, 'hex'));
 let vkeyWitnesses = witnesses.vkeys();
if (!vkeyWitnesses) {
  vkeyWitnesses = Vkeywitnesses.new();
  witnesses.set_vkeys(vkeyWitnesses);
}
  const vkeyWitness = make_vkey_witness(txHash, privateKey);
  vkeyWitnesses.add(vkeyWitness);

  const tx = Transaction.new(txBody, witnesses);

  // 9. Submit transaction
  const txHex = Buffer.from(tx.to_bytes()).toString('hex');
  const txId = await submitTx(txHex, blockfrostApiKey);

  return txId;
}
