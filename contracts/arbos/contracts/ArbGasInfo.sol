pragma solidity >=0.4.21 <0.7.0;

interface ArbGasInfo {
    function getPricesInWei() external view returns (uint, uint, uint, uint, uint, uint);
    function getPricesInArbGas() external view returns (uint, uint, uint);
}

