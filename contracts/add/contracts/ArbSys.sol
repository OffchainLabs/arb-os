pragma solidity >=0.4.21 <0.7.0;

interface ArbSys {
    // Get ArbOS version number
    function arbOSVersion() external pure returns (uint);

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

    // Register an address in the address table
    // Return index of the address (existing index, or newly created index if not already registered)
    function addressTable_register(address addr) external returns(uint);

    // Return index of an address in the address table (revert if address isn't in the table)
    function addressTable_lookup(address addr) external view returns(uint);

    // Check whether an address exists in the address table
    function addressTable_addressExists(address addr) external view returns(bool);

    // Get size of address table (= first unused index)
    function addressTable_size() external view returns(uint);

    // Return address at a given index in address table (revert if index is beyond end of table)
    function addressTable_lookupIndex(uint index) external view returns(address);

    // Read a compressed address from a bytes buffer
    // Return resulting address and updated offset into the buffer (revert if buffer is too short)
    function addressTable_decompress(bytes calldata buf, uint offset) external pure returns(address, uint);

    // Compress an address and return the result
    function addressTable_compress(address addr) external returns(bytes memory);

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

    // Generate a new contract with the same code as the given contract
    // This function returns the address of the new contract
    // This is currently the only way to create new contracts in a compiled rollup instance
    function cloneContract(address account) external returns(address);

    event EthWithdrawal(address indexed destAddr, uint amount);
    event ERC20Withdrawal(address indexed destAddr, address indexed tokenAddr, uint amount);
    event ERC721Withdrawal(address indexed destAddr, address indexed tokenAddr, uint indexed id);
}

