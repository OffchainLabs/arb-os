pragma solidity >=0.4.21 <0.7.0;

import "../../arbos/contracts/ArbSys.sol";


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
        uint256 txCount = ArbSys(address(100)).getTransactionCount(msg.sender);
        return txCount;
    }

    function withdrawMyEth() public payable {
	ArbSys(address(100)).sendTxToL1.value(msg.value)(address(1025), "");
    }

    function withdraw5000() public {
	ArbSys(address(100)).sendTxToL1.value(5000)(address(1025), "");
    }
}
