pragma solidity >=0.4.21 <0.7.0;

interface ArbosTest {
    function run(bytes calldata code, bytes calldata innerCalldata, bytes calldata initStorage) external returns(bytes memory); 
}





