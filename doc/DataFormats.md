# ArbOS message / log formats



This specification describes the format of messages used for communication between clients, the EthBridge, and ArbOS.  It includes incoming messages that are sent to ArbOS, outgoing messages that are emitted by ArbOS and recorded on the L1 chain, and log items emitted by ArbOS.

In this specification, all integers are big-endian.   Uint means an unsigned integer. Unless otherwise specified, uints are 256 bits.

Hashing uses Ethereum's keccak256 algorithm, unless otherwise specified.

## Chain ID

Every Arbitrum chain has a 48-bit chain ID, which is the low-order 48 bits of the L1 Ethereum address of the chain's EthBridge contract.

## Incoming messages

Incoming messages are put into a chain's EthBridge-managed inbox, and received by a chain's instance of ArbOS.

An incoming message is a 6-tuple:

* message type (uint)
* L1 block number (uint): L1 block number when this message was inserted into the inbox
* L1 timestamp (uint): timestamp of L1 block when this message was inserted into the inbox
* Sender (address encoded as uint)
* RequestID: 0 for the first message inserted into the inbox; otherwise 1 + the requestID of the previous message inserted into the inbox
* Type-specific data: (byte array)

##### Message type 0: Eth deposit

This message type must be initiated by the EthBridge. It represents a transfer of Eth to  an account on the L2 chain.  

Type-specific data: 

* L2 address to receive the Eth (address encoded as uint)
* number of Wei (uint)

##### Message type 1: ERC20 deposit

This message type must be initiated by the EthBridge. It represents a transfer of ERC20 tokens to  an account on the L2 chain.  

Type-specific data: 

* address of the ERC20 token (address encoded as uint)
* L2 address to receive the tokens (address encoded as uint)
* number of Wei (uint)

##### Message type 2: ERC721 deposit

This message type must be initiated by the EthBridge. It represents a transfer of an ERC721 token to  an account on the L2 chain.  

Type-specific data: 

* address of the ERC721 token (address encoded as uint)
* L2 address to receive the tokens (address encoded as uint)
* token identifier (uint)

##### Message type 3: L2 message

This message type is initiated by a client, via a transaction to the EthBridge. Its purpose is to deliver L2 data which the EthBridge does not need to understand. The EthBridge simply passes on the type-specific data uninterpreted. ArbOS will parse and validate the L2 data.

Details of L2 message subtypes and formats are listed in a separate section below.

##### Message type 4: chain initialization message

This message type is initiated by the EthBridge, as part of the creation of a new L2 chain, in order to convey parameters of the chain to ArbOS. It must only be sent as the first message in the inbox of a new chain.  

Type-specific data:

* challenge period, in milliseconds (uint)
* ArbGas speed limit, in ArbGas per second (uint)
* maximum number of execution steps allowed in an assertion (uint)
* minimum stake requirement, in Wei (uint)
* address of the chain's owner (address encoded as uint)
* option data

Option data consists of a sequence of zero or more chunks.  ArbOS will ignore chunk IDs that it does not understand.

Each chunk is:

* option ID (64-bit uint)
* option payload length (64-bit uint)
* option payload

Option ID 1 sets parameters for an optional sequencer. Payload data is:

* sequencer address (address encoded as uint)
* delay on non-sequencer messages, in blocks (64-bit uint)

Option ID 3 sets parameters for optional validator reimbursement. Payload data is:

* total charge for time, in wei per second (uint)
* total charge for ArbGas, in wei per ArbGas (uint)
* total charge for storage, in wei per EVM storage cell (uint)
* number of invited validators (64-bit uint)
* addresses of validators (sequence of 160-bit addresses)

##### Message type 5: buddy contract creation

This message type is initiated by a call from an L1 contract to the EthBridge. The EthBridge must check that the call came from a contract, and reject it otherwise. 

This message type allows an L1 contract to deploy an L2 contract at an L2 address that is equal to the contract's L1 address.  This L2 deploy will be exactly like any ordinary L2 deploy, except for how the address of the deployed L2 contract is determined.

Type-specific data:

* 1 (byte)
* maximum ArbGas to use (uint)
* ArbGas price bid, in wei (uint)
* 0 (uint)
* Eth payment, in wei (uint)
* constructor code and data, encoded per Ethereum ABI (bytes)

The EthBridge need not check the validity of the type-specific data. That is the responsibility of ArbOS.

##### Message type 6: reserved

This message type is reserved for internal use by ArbOS. It should never appear in the inbox.

## L2 messages

As noted above, an L2 message is one type of incoming message that can be put into an L2 chain's inbox. The purpose of an L2 message is to convey information, typically a transaction request, to ArbOS. The EthBridge does not examine or interpret the contents of an L2 message.

An L2 message consists of:

* an L2 message subtype (byte)
* subtype-specific data (byte array)

**Subtype 0: unsigned tx from user** has subtype-specific data of:

* ArbGas limit (uint)
* ArbGas price bid, in wei (uint)
* sequence number (uint)
* destination address (uint)
* callvalue, in wei (uint)
* calldata (bytes)

**Subtype 1: tx from contract** has subtype-specific data of:

* ArbGas limit (uint)
* ArbGas price bid, in wei (uint)
* destination address (uint)
* callvalue, in wei (uint)
* calldata (bytes)

**Subtype 2: non-mutating call** has subtype-specific data of:

* ArbGas limit (uint)
* ArbGas price bid, in wei (uint)
* destination address (uint)
* calldata (bytes)

**Subtype 3: L2 message batch** has subtype-specific data consisting of a sequence of one or more items, where each item consists of:

* L2 message length (64-bit uint)
* L2 message (byte array)

The L2 messages in a batch will be separated, and treated as if each had arrived separately, in the order in which they appear in the batch.

**Subtype 4: signed tx from user** has subtype-specific data consisting of an RLP-encoded list containing:

* ArbGas limit (RLP-encoded uint)
* ArbGas price bid, in wei (RLP-encoded uint)
* sequence number (RLP-encoded uint)
* destination address (RLP-encoded address)
* callvalue, in wei (RLP-encoded uint)
* calldata (RLP-encoded byte array)
* v (RLP-encoded uint)
* r (RLP-encoded uint)
* s (RLP-encoded uint)

Here v, r, and s comprise an EIP-155 compliant ECDSA signature by the transaction's sender, based on the L2 chain's chainID.

**Subtype 5: sequencer batch** has subtype-specific data of:

* release block number (64-bit uint)
* a sequence of one or more items, in the same format as subtype 3

The release block number specifies an L1 block number. The sequencer is directing ArbOS to stop delaying any messages that arrived at or before the specified block number. ArbOS will discard a message of this subtype unless it was sent by the authorized sequencer.

**Subtype 6: heartbeat message** has no subtype-specific data.

## Logs

ArbOS emits two types of log items: transaction receipts and block summaries.

### Tx receipts

ArbOS emits one log item for each transaction request it receives. A transaction request is an L2 message of subtype 0, 1, or 4, which might arrive in its own incoming message or might be part of a message batch or sequencer batch. Regardless, each individual request will cause its own separate tx receipt to be emitted.

ArbOS will make its best effort to emit a tx receipt for each transaction request received, regardless of whether the transaction succeeds or fails; but this will not be possible for certain kinds of erroneous requests.

A tx receipt log item consists of:

* 0 (uint)
* incoming request info consisting of:
  * 3 (uint)
  * L1 block number (uint)
  * L2 timestamp (uint)
  * address of sender (address represented as uint)
  * requestID (uint).  [described below]
  * L2 message for the request (byte array)
* tx result info consisting of:
  * return code (uint)  [described below]
  * returndata (byte array)
  * EVM logs [format described below]
* ArbGas info consisting of:
  * ArbGas used (uint)
  * ArbGas price paid, in wei (uint)
* cumulative info in L1 block consisting of:
  * ArbGas used in current L1 block including this tx (uint)
  * index of this tx within this L1 block (uint)
  * number of EVM logs emitted in this L1 block before this tx (uint)

Possible return codes are:
	0: tx returned (success)
	1: tx reverted
	2: tx dropped due to L2 congestion
	3: insufficient funds to pay for ArbGas
	4: insufficient balance 
	5: bad sequence number
	6: message format error
	255: unknown error

EVM logs are formatted as [TODO]

##### Request IDs

A requestID is a uint that uniquely identifies a transaction request. 

For a signed transaction, the requestID is the same value that Ethereum would use for the same transaction: the hash of the RLP-encoded transaction data (which is the subtype-specific data for subtype 4).

For an unsigned transaction that is an L2 message of subtype 0, the requestID is computed as:
		hash(
				sender address (as uint),
				hash (
						chainID (uint),
						MarshalledDataHash(subtype-specific data)
				)
		)

For other transactions, the requestID is computed from incoming message contents as follows.  An incoming message is assigned a requestID of hash(chainID, inboxSeqNum), where inboxSeqNum is the value N such that this is the Nth message that has ever arrived in the chain's inbox.  If the incoming message includes a batch, the K'th item in the batch is assigned a requestID of hash(requestID of batch, index within batch).  If batches are nested, this rule is applied recursively.

It is infeasible to find two distinct requests that will have the same requestID.  This is true because requestIDs are the output of a collision-free hash function, and it is not possible to create two distinct requests that will have the same input to the hash function.  Signed transaction IDs cannot collide with the other types, because the other types' hash preimages both start with a zero byte (because sender address and chainID are zero-filled in the most-significant byte of a big-endian value) and the RLP encoding of a list cannot start with a zero byte.  The other two types cannot have the same hash preimage because subtype-0 messages use a hash output as their second word, which with overwhelming probabilit will be too large to be feasible as the sequence number or batch index that occupies the same position in the default request ID scheme.

##### MarshalledDataHash algorithm

TODO

## Outgoing messages

Outgoing messages reflect actions that require action at L1 or that need to be specifically visible to L1 contracts.

An outgoing message consists of:

* outgoing message type (uint)
* sender (address)
* type-specific data (byte array)

There are four outgoing message types.

**Type 0: Eth Withdrawal** has type-specific data of:

* destination address (address encoded as uint)
* amount, in wei (uint)

**Type 1: ERC20 Withdrawal** has type-specific data of:
* token address(address encoded as uint), 
* destination address(address encoded as uint),
*  amount (uint)

**Type 2: ERC721 Withdrawal** has type-specific data of:

* token address(address encoded as uint), 
* destination address(address encoded as uint),
*  token ID (uint)

**Type 5: Buddy contract notification** has no type-specific data.


