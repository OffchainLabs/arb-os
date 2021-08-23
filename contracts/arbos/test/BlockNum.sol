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
        return ArbSys(address(100)).getL1CallerInfo();
    }
}
