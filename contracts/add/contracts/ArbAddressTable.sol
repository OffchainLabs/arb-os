pragma solidity >=0.4.21 <0.7.0;

interface ArbAddressTable {
    // Register an address in the address table
    // Return index of the address (existing index, or newly created index if not already registered)
    function register(address addr) external returns(uint);

    // Return index of an address in the address table (revert if address isn't in the table)
    function lookup(address addr) external view returns(uint);

    // Check whether an address exists in the address table
    function addressExists(address addr) external view returns(bool);

    // Get size of address table (= first unused index)
    function size() external view returns(uint);

    // Return address at a given index in address table (revert if index is beyond end of table)
    function lookupIndex(uint index) external view returns(address);

    // Read a compressed address from a bytes buffer
    // Return resulting address and updated offset into the buffer (revert if buffer is too short)
    function decompress(bytes calldata buf, uint offset) external pure returns(address, uint);

    // Compress an address and return the result
    function compress(address addr) external returns(bytes memory);
}

