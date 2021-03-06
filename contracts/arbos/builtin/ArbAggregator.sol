pragma solidity >=0.4.21 <0.7.0;

interface ArbAggregator {
    // Get the preferred aggregator for an address.
    // Returns (preferredAggregatorAddress, isDefault)
    //     isDefault is true if addr is set to prefer the default aggregator
    function getPreferredAggregator(address addr) external view returns (address, bool);

    // Set the caller's preferred aggregator.
    // If prefAgg is zero, this sets the caller to prefer the default aggregator
    function setPreferredAggregator(address prefAgg) external;

    // Get default aggregator.
    function getDefaultAggregator() external view returns (address);

    // Set the preferred aggregator.
    // Reverts unless called by the chain owner or the current default aggregator.
    function setDefaultAggregator(address newDefault) external;
}

