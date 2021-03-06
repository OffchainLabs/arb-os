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
* Size of type-specific data (uint)
* Type-specific data: (buffer)

The L1 block number and/or L1 timestamp fields can be set to zero. Zero values in these fields will be replaced, by ArbOS, with the value of the same field in the previous message. If there was no previous message, ArbOS will leave these values as zero. (Note that the EthBridge will never create messages with zeroed block number or timestamp fields. The treatment of zero block number and timestamp values exists only as a convenience for use in private executions of a chain.)

Each message type is associated with rules, imposed by the Arbitrum protocol, regarding which properties the EthBridge must verify before sending a specific message type. These rules are not described here because they are not a part of the data format.

##### Message type 0: Eth deposit

[This is no longer supported.]

##### Message type 1: ERC20 deposit

[This is no longer supported.]

##### Message type 2: ERC721 deposit

[This is no longer supported.]

##### Message type 3: L2 message

This message type is initiated by a client, via a transaction to the EthBridge. Its purpose is to deliver to ArbOS an L2 data payload which the EthBridge does not need to understand. The EthBridge simply passes on the type-specific data uninterpreted. ArbOS will parse and validate the L2 data.

Details of L2 message subtypes and formats are listed in a separate section below.

##### Message type 4: chain initialization message

This message type is initiated by the EthBridge, as part of the creation of a new L2 chain, in order to convey parameters of the chain to ArbOS. It must only be sent as the first message in the inbox of a new chain.  

Type-specific data:

* challenge period, in seconds (uint)
* ArbGas speed limit, in ArbGas per second (uint)
* maximum number of execution steps allowed in an assertion (uint)
* minimum stake requirement, in Wei (uint)
* address of the staking token, or zero if staking in ETH (address encoded as uint)
* address of the chain's owner (address encoded as uint)
* option data

Option data consists of a sequence of zero or more chunks.  ArbOS will ignore a chunk if it does not know how to handle that chunk's option ID.

Each chunk is:

* option ID (64-bit uint)
* option payload length (64-bit uint)
* option payload

At present. the following options are supported:

* [Option 0 is currently unused]
* [Option 1 is currently unused]
* Option 2: set charging parameters: 
  * speed limit per second (uint); 
  * L1 gas per L2 tx (uint); 
  * L1 gas per L2 calldata byte; 
  * L1 gas per storage unit allocated (uint); 
  * ratio of L1 gas price to base ArbGas price; 
  * network fee recipient (address encoded as uint); 
  * congestion fee recipient (address encoded as uint)
* Option 3: set default aggregator
  * Default aggregator address (address encoded as uint)

All other options are ignored at present.

##### Message type 5: buddy contract creation

This message type is initiated by a call from an L1 contract to the EthBridge. The EthBridge must check that the call came from a contract, and reject it otherwise. 

This message type allows an L1 contract to deploy an L2 contract at an L2 address that is equal to the contract's L1 address.  This L2 deploy will be exactly like any ordinary L2 deploy, except for how the address of the deployed L2 contract is determined.

Type-specific data:

* maximum ArbGas to use (uint)
* ArbGas price bid, in wei (uint)
* Callvalue, in wei (uint)
* constructor code and data, encoded per Ethereum ABI (bytes)

##### Message type 6: reserved

This message type is reserved for internal use by ArbOS. It should never appear in the inbox.

**Message type 7: L2 transaction funded by L1**

This message type encodes an L2 transaction that is funded by calldata provided at L1. The type-specific data must be the same as an L2 message of subtype 0 or 1.

**Message type 8: Rollup protocol event**

[This is not yet documented.]

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

The enclosed L2 message may not have subtype 5 (sequencer batch).  All other subtypes are allowed.

**Subtype 4: signed tx from user** has subtype-specific data that is identical to the standard Ethereum encoded transaction format. The subtype-specific data consists of an RLP-encoded list containing:

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

The destination address is encoded consistently with Ethereum: a zero address is encoded as an empty byte array, and any other value is encoded as an array of 20 bytes.  

**Subtype 5** is reserved for future use.

**Subtype 6: heartbeat message** has no subtype-specific data. This message has no effect, except to notify ArbOS that the block number and timestamp in the enclosing L1 message has been reached. ArbOS merely notes the block number and timestamp, then discards the message.

**Subtype 7: signed compressed transaction** encodes a signed, compressed transaction. The subtype-specific data is defined by the compression format, which is documented elsewhere.

**Subtype 8: BLS signed message batch** encodes a batch of transactions, which are signed using a BLS aggregate signature. The subtype-specific data is:

* number of messages in batch (RLP-encoded uint)
* BLS signature (2 uints)
* for each message: compressed message data (in compressed format that is described elsewhere)

## Logs

ArbOS emits three types of log items: transaction receipts, block summaries, and outgoing message contents.

### Tx receipts

ArbOS emits one log item for each transaction request it receives. A transaction request is an L2 message of subtype 0, 1, 4, or 7, which might arrive in its own incoming message or might be part of a batch. Regardless, each individual request will cause its own separate tx receipt to be emitted.

ArbOS will make its best effort to emit a tx receipt for each transaction request received, regardless of whether the transaction succeeds or fails; but this will not be possible for certain kinds of erroneous requests.

A tx receipt log item consists of:

* 0 (uint)
* incoming request info, an 8-tuple consisting of:
  * 3 (uint)
  * L1 block number (uint)
  * Arbitrum block number (uint)
  * L2 timestamp (uint)
  * address of sender (address represented as uint)
  * requestID (uint)  [described below]
  * a 2-tuple consisting of:
    * size of L2 message (uint)
    * contents of L2 message for the request (buffer)
  * a 3-tuple consisting of:
    * provenance info
    * aggregator info
    * whether message was artificially injected via sideload (boolean encoded as uint 0 or 1)
* tx result info, a 3-tuple consisting of:
  * return code (uint)  [described below]
  * returndata (2-tuple of size (uint) and contents (buffer))
  * EVM logs [format described below]
* ArbGas info, as 2-tuple consisting of:
  * ArbGas used (uint)
  * ArbGas price paid, in wei (uint)
* cumulative info in Arbitrum block, a 3-tuple consisting of:
  * ArbGas used in current Arbitrum block including this tx (uint)
  * index of this tx within this Arbitrum block (uint)
  * number of EVM logs emitted in this Arbitrum block before this tx (uint)
* fee information for the transaction, a 4-tuple consisting of:
  * a 4-tuple of prices of:
    * L2 transaction (uint)
    * L1 calldata bytes (uint)
    * L2 storage (uint)
    * L2 computation (uint)
  * a 4-tuple of units used of:
    * L2 transaction (uint, will always be 1)
    * L1 calldata bytes (uint)
    * L2 storage (uint)
    * L2 computation (uint)
  * a 4-tuple of wei paid for: [might not equal product of units and price, e.g. if user has insufficient funds to pay, or no aggregator was reimbursed]
    * L2 transaction (uint)
    * L1 calldata bytes (uint)
    * L2 storage (uint)
    * L2 computation (uint)
  * address of aggregator that was reimbursed (or zero if there wasn't one) (address encoded as uint)

Possible return codes are:
	0: tx returned (success)
	1: tx reverted
	2: tx dropped due to L2 congestion
	3: insufficient funds to pay for ArbGas
	4: insufficient balance
	5: bad sequence number
	6: message format error
    7: cannot deploy at requested address
	255: unknown error

EVM logs are formatted as an EVM value, as a linked list in reverse order, such as this: (*log3*, (*log2*, (*log1*, (*log0*, () ) ) ) ). In this example there are four EVM log items, with the first one being *log0* and the last being *log3*.  Each EVM log is structured as an AVM tuple *(address, (dataSize, dataBuffer), topic0, topic1, ...)*, with as many topics as are present in that particular EVM log item.

##### Request IDs

A requestID is a uint that uniquely identifies a transaction request. 

For a signed transaction, the requestID is the same value that Ethereum would use for the same transaction: the hash of the RLP-encoded transaction data (which is the subtype-specific data for subtype 4).

For an unsigned transaction that is an L2 message of subtype 0, the requestID is computed as:
		hash(
				sender address (as uint),
				hash (
						chainID (uint),
						hash(subtype-specific data)
				)
		)

For other transactions, the requestID is computed from incoming message contents as follows.  An incoming message is assigned a requestID of hash(chainID, inboxSeqNum), where inboxSeqNum is the value N such that this is the Nth message that has ever arrived in the chain's inbox.  If the incoming message includes a batch, the K'th item in the batch is assigned a requestID of hash(requestID of batch, K).  If batches are nested, this rule is applied recursively.

It is infeasible to find two distinct requests that have the same requestID.  This is true because requestIDs are the output of a collision-free hash function, and it is not possible to create two distinct requests that will have the same input to the hash function.  Signed transaction IDs cannot collide with the other types, because the other types' hash preimages both start with a zero byte (because sender address and chainID are zero-filled in the most-significant byte of a big-endian value) and the RLP encoding of a list cannot start with a zero byte.  The other two types cannot have the same hash preimage because subtype-0 messages use a hash output as their second word, which with overwhelming probability will be too large to be feasible as the sequence number or batch index that occupies the same position in the default request ID scheme.

### Block summary

A block summary is emitted at the end of every L1 block that contains any L2 transactions. No summary is emitted for a block that has no L2 activity.

A block summary item consists of:

* 1 (uint)
* block number (uint)
* timestamp (uint)
* current ArbGas limit per block (uint)
* statistics for this block: 5-tuple of
  * total ArbGas used (uint)
  * number of transactions (uint)
  * number of EVM logs (uint)
  * number of AVM logs (uint)
  * number of sends (uint)
* statistics for the chain since it was created (same format as previous item)
* gas accounting summary: 5-tuple of:
  * current ArbGas price in wei (uint)
  * size of current ArbGas pool (uint)
  * reserve funds in wei (uint)
  * total wei paid to validators over all time (uint)
  * address receiving validator payments (address encoded as uint)
* previous block number that had a block summary, or 0 if this is the first block to have a block summary

### Outgoing message contents

ArbOS supports sending messages from Arbitrum contracts to L1. The contents of each message are emitted as a log item; and a Merkle root covering a batch of outgoing messages will later be published to the L1 as an Arbitrum Send.

The log item to publish the contents of a message consists of:

* 2 (uint)
* outgoing message batch number (uint)
* index within outgoing message batch (uint)
* size of message (uint)
* message contents (buffer)

## Arbitrum Sends

Arbitrum Sends are values emitted by an Arbitrum chain that are recorded at L1.  A send consists of a sequence of bytes, encoded as a pair: size (uint) and contents (buffer).

The contents of a Send consist of:

* Send type (byte)
* Type-specific data

Currently only one type is supported: a message batch summary.  Its type-specific data consists of:

* batch number (uint)
* number of messages in batch (uint)
* Merkle root of message hashes (32 bytes)

The Merkle root is computed using the algorithm defined in the Arbitrum Solidity contracts.




