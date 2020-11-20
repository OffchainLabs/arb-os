pragma solidity >=0.4.21 <0.7.0;

interface ArbAggregator {
    // Register the caller as an aggregator, and set the aggregator's fee per byte of calldata.
    // Revert if caller is already registered as an aggregator.
    function registerAsAggregator(uint feePerByte) external;

    // Set the caller's aggregator fee.
    // Revert if the caller is not registered as an aggregator.
    function setAggregatorFee(uint feePerByte) external;

    // Get an aggregator's fee level. Revert if addr is not a registered aggregator.
    function getAggregatorFee(address addr) external view returns(uint);

    // Withdraw the caller's registration as an aggregator.
    // Revert if caller is not registered as an aggregator.
    function withdrawAsAggregator() external;

    // Register as a client of an aggregator, giving a maxPrice you're willing to pay per byte of calldata.
    // Revert if the aggregator is not registered as an aggregator.
    function registerAsClient(address aggregator, uint maxPrice) external;

    // Get information about a client's registration with an aggregator.
    // Return (false, 0, 0) if aggregator is not registered, or client is not registered with aggregator.
    // Otherwise return (true, clientMaxFee, aggregatorFee)
    function getClientInfo(address client, address aggregator) external view returns(bool, uint, uint);

    // Stop being a client to an aggregator.
    // Revert if the aggregator is not registered, or if caller is not a client of the aggregator.
    function withdrawAsClient(address aggregator) external;
}

