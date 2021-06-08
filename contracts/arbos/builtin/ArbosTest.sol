pragma solidity >=0.4.21 <0.7.0;

interface ArbosTest {
    function installAccount(address addr, bool isEOA, uint balance, uint nonce, bytes calldata code, bytes calldata initStorage) external; 

    function setNonce(address addr, uint nonce) external;

    function setBalance(address addr, uint balance) external;

    function setStorage(address addr, bool isDiff, bytes calldata marshalledStorage) external;

    function setCode(address addr, bytes calldata evmCode) external;

    function getMarshalledStorage(address addr) external view;  // returns raw returndata

    function getAccountInfo(address addr) external view;  // returns raw returndata

    function burnArbGas(uint gasAmount) external view;
}





