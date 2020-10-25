pragma solidity >=0.4.21 <0.7.0;

interface ArbSys {
    // Send given amount of ERC-20 tokens to dest with token contract sender.
    // This is safe to freely call since the sender is authenticated and thus
    // you can only send fake tokens, not steal real ones
    function withdrawERC20(address dest, uint256 amount) external;

    // Send given ERC-721 token to dest with token contract sender.
    // This is safe by the above arguement
    function withdrawERC721(address dest, uint256 id) external;

    // Send given amount of Eth to dest with from sender.
    function withdrawEth(address dest) external payable;

    // Return block when current message was posted on-chain
    function currentMessageBlock() external view returns(uint);

    // Return timestamp when current message was posted on-chain
    function currentMessageTimestamp() external view returns(uint);

    // Return upper bound on the on-chain block number
    function blockUpperBound() external view returns(uint);

    // Return upper bound on the on-chain timestamp
    function timestampUpperBound() external view returns(uint);

    // Return the number of transactions issued by the given external account
    // or the account sequence number of the given contract
    function getTransactionCount(address account) external view returns(uint256);

    // Generate a new contract with the same code as the given contract
    // This function returns the address of the new contract
    // This is currently the only way to create new contracts in a compiled rollup instance
    function cloneContract(address account) external returns(address);

    event EthWithdrawal(address indexed destAddr, uint amount);
    event ERC20Withdrawal(address indexed destAddr, address indexed tokenAddr, uint amount);
    event ERC721Withdrawal(address indexed destAddr, address indexed tokenAddr, uint indexed id);
}

