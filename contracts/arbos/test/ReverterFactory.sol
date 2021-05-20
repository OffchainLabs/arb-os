//SPDX-License-Identifier: Unlicense

contract Reverter {
  constructor(uint mode) public {
      require(mode == 0, "Mode not 0");
  }
}

contract ReverterFactory {
    constructor(uint mode) public {
        new Reverter(mode);
    }
}

contract ConstructorCallback {
    event TestEvent(uint256 dataLength);
    event TestEvent2(address dataLength);

    constructor() public payable {
        emit TestEvent(msg.data.length);
        ConstructorCallback2(msg.sender).test2();
    }

    function test(address data) external {
        emit TestEvent2(data);
    }
}

contract ConstructorCallback2 {
    event TestEvent3(bool indexed success, bytes returnData);

    function test() external payable {
        new ConstructorCallback();
    }

    function test2() external payable {
        (bool success, bytes memory returnData) =
            address(msg.sender).call(
                abi.encodeWithSelector(ConstructorCallback.test.selector, msg.sender)
            );
        emit TestEvent3(success, returnData);
    }
}
