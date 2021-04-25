
pragma solidity >=0.4.21 <0.7.0;

/**
* @title precompiled contract in every Arbitrum chain for retryable transaction related data retrieval and interactions. Exists at 0x000000000000000000000000000000000000006E 
*/
interface ArbRetryableTx {

    /**
    * @notice Redeem a redeemable tx.
    * Revert if called by an L2 contract, or if txId does not exist, or if txId reverts.
    * If this returns, txId has been completed and is no longer available for redemption.
    * If this reverts, txId is still available for redemption (until it times out or is canceled).
    @param txId unique identifier of retryable message: keccak256(keccak256(ArbchainId, inbox-sequence-number), uint(0) )
     */
    function redeem(bytes32 txId) external;

    /** 
    * @notice Return the minimum lifetime of redeemable txn.
    * @return lifetime in seconds
    */
    function getLifetime() external view returns(uint);

    /**
    * @notice Return the timestamp when txId will age out, or zero if txId does not exist.
    * The timestamp could be in the past, because aged-out txs might not be discarded immediately.
    * @param txId unique identifier of retryable message: keccak256(keccak256(ArbchainId, inbox-sequence-number), uint(0) )
    * @return timestamp for txn's deadline  
    */
    function getTimeout(bytes32 txId) external view returns(uint);

    /** 
    * @notice Return the price, in wei, of submitting a new retryable tx with a given calldata size.
    * @param calldataSize call data size to get price of (in wei)
    * @return (price, nextUpdateTimestamp). Price is guaranteed not to change until nextUpdateTimestamp.
    */ 
    function getSubmissionPrice(uint calldataSize) external view returns (uint, uint);

    /** 
     * @notice Return the price, in wei, of extending the lifetime of txId by an additional lifetime period. Revert if txId doesn't exist.
     * @param txId unique identifier of retryable message: keccak256(keccak256(ArbchainId, inbox-sequence-number), uint(0) )
     * @return (price, nextUpdateTimestamp). Price is guaranteed not to change until nextUpdateTimestamp.
    */
    function getKeepalivePrice(bytes32 txId) external view returns(uint, uint);

    /** 
    @notice Deposits callvalue into the sender's L2 account, then adds one lifetime period to the life of txId.
    * If successful, emits LifetimeExtended event.
    * Revert if txId does not exist, or if the timeout of txId is already at least one lifetime in the future, or if the sender has insufficient funds (after the deposit).
    * @param txId unique identifier of retryable message: keccak256(keccak256(ArbchainId, inbox-sequence-number), uint(0) )
    * @return New timeout of txId.
    */
    function keepalive(bytes32 txId) external payable returns(uint);

    /**
    * @notice Return the beneficiary of txId.
    * Revert if txId doesn't exist.
    * @param txId unique identifier of retryable message: keccak256(keccak256(ArbchainId, inbox-sequence-number), uint(0) )
    * @return address of beneficiary for transaction 
    */
    function getBeneficiary(bytes32 txId) external view returns (address);

    /** 
    @notice Cancel txId and refund its callvalue to its beneficiary.
    * Revert if txId doesn't exist, or if called by anyone other than txId's beneficiary.
    @param txId unique identifier of retryable message: keccak256(keccak256(ArbchainId, inbox-sequence-number), uint(0) )
    */
    function cancel(bytes32 txId) external;

    event LifetimeExtended(bytes32 indexed txId, uint newTimeout);
    event Redeemed(bytes32 indexed txId);
    event Canceled(bytes32 indexed txId);
}

