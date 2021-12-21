//SPDX-License-Identifier: Unlicense
pragma solidity >=0.4.21 <0.9.0;

contract Worker {
    mapping(uint256 => uint256) public values;

    function work() public {
        for (uint256 i = 0; i < 100; i++) values[i] += 1;
    }
}

contract Base is Worker {
    event GasPre(uint256 gas);
    event GasPost(uint256 gas);

    Worker worker;

    constructor() public {
        worker = new Worker();
    }
}

contract Greeter is Base {
    function test() external {
        emit GasPre(gasleft());

        work();
        // worker.work();

        emit GasPost(gasleft());
    }
}

contract Delegator is Base {
    function testDelegate(address implementation) external {
        (bool success,) = implementation.delegatecall(abi.encodeWithSelector(Greeter.test.selector));
        require(success, "FAIL_DELEGATE");
    }
}