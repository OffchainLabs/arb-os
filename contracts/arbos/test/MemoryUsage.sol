pragma solidity >=0.4.21 <0.7.0;

contract MemoryUsage {
    function test(uint offset) public {
        address addr = address(offset);
        assembly {
            mstore(addr, offset)
        }
    }
}
