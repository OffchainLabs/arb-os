pragma solidity >=0.4.21 <0.9.0;

contract MemoryUsage {
    function test(uint offset) public {
        address addr = address(uint160(offset));
        assembly {
            mstore(addr, offset)
        }
    }
}
