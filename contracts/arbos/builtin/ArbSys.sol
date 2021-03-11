pragma solidity >=0.4.21 <0.7.0;

interface ArbSys {
    // Get ArbOS version number
    function arbOSVersion() external pure returns (uint);

    // Get Arbitrum block number
    function arbBlockNumber() external view returns (uint);

    // Send given amount of Eth to dest with from sender.
    // This is a convenience function, which is equivalent to calling sendTxToL1 with empty calldataForL1.
    // Returns a unique identifier for this L2-to-L1 transaction.
    function withdrawEth(address destination) external payable returns(uint);

    // Send a transaction to L1
    // Returns a unique identifier for this L2-to-L1 transaction.
    function sendTxToL1(address destination, bytes calldata calldataForL1) external payable returns(uint);

    // Return the number of transactions issued by the given external account
    // or the account sequence number of the given contract
    function getTransactionCount(address account) external view returns(uint256);

    // Return the value of the storage slot for the given account at the given index
    // This function is only callable from address 0 to prevent contracts from being
    // able to call it
    function getStorageAt(address account, uint256 index) external view returns (uint256);

    // Return true if the caller of this was called directly from L1
    function isTopLevelCall() external view returns (bool);

    event EthWithdrawal(address indexed destAddr, uint amount);
    event ERC20Withdrawal(address indexed destAddr, address indexed tokenAddr, uint amount);
    event ERC721Withdrawal(address indexed destAddr, address indexed tokenAddr, uint indexed id);

    event L2ToL1Transaction(address caller, address indexed destination, uint indexed uniqueId,
                            uint indexed batchNumber, uint indexInBatch,
                            uint arbBlockNum, uint ethBlockNum, uint timestamp,
                            uint callvalue, bytes data);
}

