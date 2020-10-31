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

    // Return the number of transactions issued by the given external account
    // or the account sequence number of the given contract
    function getTransactionCount(address account) external view returns(uint256);

    // Associate a BLS public key with the caller's address
    function registerBlsKey(uint x0, uint x1, uint y0, uint y1) external;

    // Get the BLS public key associated with an address (revert if there isn't one)
    function getBlsPublicKey(address addr) external view returns (uint, uint, uint, uint);

    // Upload a serialized function table and associate it with the caller's address
    // If caller already had a function table, this will overwrite the old one
    // Revert if buf is mal-formatted
    // (Caller will typically be an aggregator)
    function uploadFunctionTable(bytes calldata buf) external;

    // Get the size of addr's function table; revert if addr doesn't have a function table
    function functionTableSize(address addr) external view returns(uint);

    // Get the entry from addr's function table, at index; revert if addr has no table or index out of bounds
    // Returns (functionCode, isPayable, gasLimit)
    function functionTableGet(address addr, uint index) external view returns(uint, bool, uint);

    event EthWithdrawal(address indexed destAddr, uint amount);
    event ERC20Withdrawal(address indexed destAddr, address indexed tokenAddr, uint amount);
    event ERC721Withdrawal(address indexed destAddr, address indexed tokenAddr, uint indexed id);
}

