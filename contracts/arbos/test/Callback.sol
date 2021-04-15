//SPDX-License-Identifier: Unlicensed

pragma solidity >=0.4.21 <0.7.0;

import "../builtin/ArbSys.sol";

contract Callback {
    function sendDummies() external {
        emit DummyEvent(1, 11, 21);
        emit DummyEvent(2, 12, 22);
        emit DummyEvent(3, 13, 23);
    }

    function doCallback() external returns(uint, uint) {
        emit DummyEvent(1, 2, 3);
        emit DummyEvent(4, 5, 6);
        uint r1 = ArbSys(address(100)).sendTxToL1(address(42), "hello world");
        emit DummyEvent(7, 8, 9);
        emit DummyEvent(10, 11, 12);
        emit DummyEvent(13, 14, 15);
        uint r2 = ArbSys(address(100)).sendTxToL1(address(43), "hello there world");
        emit DummyEvent(16, 17, 18);
        return(r1, r2);
    }

    event DummyEvent(uint indexed a, uint b, uint c);
}


