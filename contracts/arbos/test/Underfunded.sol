pragma solidity >=0.4.21 <0.7.0;

contract Underfunded {
    fallback() external payable {
        require(false, "no deposits");
    }
    function nestedCall(uint256 value) external {
        address(this).call.value(value)("");
    }
}

