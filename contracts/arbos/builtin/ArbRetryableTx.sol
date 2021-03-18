
pragma solidity >=0.4.21 <0.7.0;

interface ArbRetryableTx {
    // Redeem a redeemable tx.
    // Revert if called by an L2 contract, or if txId does not exist, or if txId reverts.
    // If this returns, txId has been completed and is no longer available for redemption.
    // If this reverts, txId is still available for redemption (until it times out or is canceled).
    function redeem(bytes32 txId) external;

    // Return the minimum lifetime of redeemable txs.
    function getLifetime() external view returns(uint);

    // Return the timestamp when txId will age out, or zero if txId does not exist.
    // The timestamp could be in the past, because aged-out txs might not be discarded immediately.
    function getTimeout(bytes32 txId) external view returns(uint);

    // Return the price, in wei, of submitting a new retryable tx with a given calldata size.
    // Return value is (price, nextUpdateTimestamp). Price is guaranteed not to change until nextUpdateTimestamp.
    function getSubmissionPrice(uint calldataSize) external view returns (uint, uint);

    // Return the price, in wei, of extending the lifetime of txId by an additional lifetime period.
    // Return value is (price, nextUpdateTimestamp). Price is guaranteed not to change until nextUpdateTimestamp.
    // Revert if txId doesn't exist.
    function getKeepalivePrice(bytes32 txId) external view returns(uint, uint);

    // Deposits callvalue into the sender's L2 account, then adds one lifetime period to the life of txId.
    // If successful, emits LifetimeExtended event, and returns the new timeout of txId.
    // Revert if txId does not exist, or if the timeout of txId is already at least one lifetime in the future,
    //        or if the sender has insufficient funds (after the deposit).
    function keepalive(bytes32 txId) external payable returns(uint);

    // Return the beneficiary of txId.
    // Revert if txId doesn't exist.
    function getBeneficiary(bytes32 txId) external view returns (address);

    // Cancel txId and refund its callvalue to its beneficiary.
    // Revert if txId doesn't exist, or if called by anyone other than txId's beneficiary.
    function cancel(bytes32 txId) external;

    event LifetimeExtended(bytes32 indexed txId, uint newTimeout);
    event Redeemed(bytes32 indexed txId);
    event Canceled(bytes32 indexed txId);
}

