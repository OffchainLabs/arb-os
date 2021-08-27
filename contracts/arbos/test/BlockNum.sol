pragma solidity >=0.4.21 <0.7.0;

contract BlockNum {
    uint public currBlock;

    function setBlock() public {
        currBlock = getBlock();
    }

    function getBlock() public view returns (uint) {
        return block.number;
    }

    function getOrigin() public returns (address) {
        return tx.origin;
    }

    function getBlockNumTimestamp() public returns (uint, uint) {
        return (block.number, block.timestamp);
    }
}
