
pragma solidity >=0.4.21 <0.7.0;

interface ArbRetryableTx {
    // Redeem a redeemable tx.
    // Reverts if called by an L2 contract, or if txId does not exist, or if txId reverts.
    // If this returns, txId has been completed and is no longer available for redemption.
    // If this reverts, txId is still available for redemption (until it times out).
    function redeem(uint txId) external;

    // Return the minimum lifetime of redeemable txs
    function getLifetime() external view returns(uint);

    // Return the timestamp when txId ages out, or zero if txId does not exist.
    // This could be in the past, because aged-out txs might not be discarded immediately.
    function getTimeout(uint txId) external view returns(uint);

    // Returns the price, in wei, of extending the lifetime of txId by an additional lifetime period.
    // Return value is (price, nextUpdateTimestamp). Price is guaranteed not to change until nextUpdateTimestamp.
    // Reverts if txId doesn't exist.
    function getKeepalivePrice(uint txId) external view returns(uint, uint);

    // Deposits callvalue into the sender's L2 account, then adds one lifetime period to the life of txId.
    // If successful, emits LifetimeExtended evcent, and returns the new timeout of txId.
    // Reverts if txId does not exist or the sender has insufficient funds (after the deposit).
    function keepalive(uint txId) external payable returns(uint);

    event LifetimeExtended(uint indexed txId, uint newTimeout);
    event Redeemed(uint indexed txId);
}

