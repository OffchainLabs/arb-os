pragma solidity >=0.4.21 <0.7.0;

interface ArbGasInfo {
    // return gas prices in wei 
    //        (per L2 tx, per L1 calldata byte, per storage allocation, per ArbGas base, per ArbGas congestion, per ArbGas total)
    function getPricesInWei() external view returns (uint, uint, uint, uint, uint, uint);

    // return prices in ArbGas (per L2 tx, per L1 calldata byte, per storage allocation)
    function getPricesInArbGas() external view returns (uint, uint, uint);

    // return gas accounting parameters (speedLimitPerSecond, gasPoolMax, maxTxGasLimit)
    function getGasAccountingParams() external view returns (uint, uint, uint);
}

