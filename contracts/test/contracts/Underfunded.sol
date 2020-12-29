pragma solidity ^0.5.16;

contract Underfunded {
    function() external payable {
        require(false, "no deposits");
    }
    function nestedCall(uint256 value) external {
        address(this).call.value(value)("");
    }
}

