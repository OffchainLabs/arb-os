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

    function addressTable_lookupAddress(address addr, bool allocIfMissing) external returns(uint);

    function addressTable_lookupIndex(uint index) external returns(address);

    function addressTable_decompress(bytes calldata buf) external returns(address, bool);

    function addressTable_compress(address addr) external returns(bytes memory);

    function registerBlsKey(uint x0, uint x1, uint y0, uint y1) external;

    function getBlsPublicKey(address addr) external returns (uint, uint, uint, uint);

    function uploadFunctionTable(bytes calldata buf) external;
}
