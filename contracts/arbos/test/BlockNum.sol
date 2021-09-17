pragma solidity >=0.4.21 <0.7.0;

import "../builtin/ArbSys.sol";


contract BlockNum {
    uint public currBlock;

    function setBlock() public {
        currBlock = getBlock();
    }

    function getBlock() public view returns (uint) {
        return block.number;
    }

    function getSender() public view returns (address) {
        return msg.sender;
    }

    function getL1CallerInfo() public view returns (bool, address) {
        return (
            ArbSys(address(100)).wasMyCallersAddressAliased(),
            ArbSys(address(100)).myCallersAddressWithoutAliasing()
        );
    }

    function recursiveCall(uint depth, bool shouldRevert) public {
        if (depth > 0) {
            this.recursiveCall(depth-1, shouldRevert);
        }
        require(!shouldRevert);
    }
}
