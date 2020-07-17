pragma solidity >=0.4.21 <0.7.0;

import "./ArbSys.sol";


contract Add {
    function add(uint x, uint y) public pure returns (uint) {
        return x+y;
    }

    function mult(uint x, uint y) public pure returns (uint) {
        return x*y;
    }

    function pythag(uint x, uint y) public pure returns (uint) {
        return add(mult(x,x), mult(y,y));
    }

    function getSeqNum() public returns (uint) {
        uint256 txCount = ArbSys(address(100)).getTransactionCount(msg.sender);
        return txCount;
    }

    function withdrawMyEth() public payable {
	ArbSys(address(100)).withdrawEth.value(msg.value)(address(1025));
    }

    function currentBlockNum() public returns (uint) {
        return block.number;
    }
}
