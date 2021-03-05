//SPDX-License-Identifier: Unlicensed

pragma solidity >=0.4.21 <0.7.0;

import "../builtin/ArbSys.sol";

contract Callback {
    function sendDummies() external {
        emit DummyEvent(1, 11, 21);
        emit DummyEvent(2, 12, 22);
        emit DummyEvent(3, 13, 23);
    }

    function doCallback() external {
        ArbSys(address(100)).sendTxToL1(address(42), "hello world");
    }

    event DummyEvent(uint indexed a, uint b, uint c);
}


