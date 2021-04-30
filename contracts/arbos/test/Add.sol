pragma solidity >=0.4.21 <0.7.0;

import "./Fibonacci.sol";
import "../builtin/ArbSys.sol";


contract Add {
    constructor() public payable {}
    
    function add(uint x, uint y) public pure returns (uint) {
        return x+y;
    }

    function mult(uint x, uint y) public pure returns (uint) {
        return x*y;
    }

    function pythag(uint x, uint y) public pure returns (uint) {
        return add(mult(x,x), mult(y,y));
    }

    function getSeqNum() public view returns (uint) {
        uint256 txCount = ArbSys(address(0xa4b0500000000000000000000000000000000000)).getTransactionCount(msg.sender);
        return txCount;
    }

    function withdrawMyEth() public payable {
	ArbSys(address(0xa4b0500000000000000000000000000000000000)).withdrawEth.value(msg.value)(address(1025));
    }

    function withdraw5000() public {
	ArbSys(address(0xa4b0500000000000000000000000000000000000)).withdrawEth.value(5000)(address(1025));
    }

    function isTopLevel() public returns (bool) {
        return ArbSys(address(0xa4b0500000000000000000000000000000000000)).isTopLevelCall();
    }

    function isNotTopLevel() public returns (bool) {
        Fibonacci fib = new Fibonacci();
        return fib.isTopLevel();
    }
}
