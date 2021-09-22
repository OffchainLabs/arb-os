pragma solidity >=0.4.21 <0.7.0;

import "../builtin/ArbSys.sol";
import "../builtin/ArbosTest.sol";


contract BlockNum {
    uint public currBlock;
    mapping(uint => uint) aMap;

    function setBlock() public {
        currBlock = getBlock();
    }

    function getBlock() public view returns (uint) {
        return block.number;
    }

    function getSender() public view returns (address) {
        return msg.sender;
    }

    function getL1CallerInfo() public view returns (bool, address) {
        return (
            ArbSys(address(100)).wasMyCallersAddressAliased(),
            ArbSys(address(100)).myCallersAddressWithoutAliasing()
        );
    }

    function recursiveCall(uint depth, bool shouldRevert) public {
        if (depth > 0) {
            this.recursiveCall(depth-1, shouldRevert);
        }
        require(!shouldRevert);
    }

    function useGasDownTo(uint targetGas) public {
        uint gas = gasleft();
        require(gas > targetGas);
        ArbosTest(address(105)).burnArbGas(gas-targetGas);
    }

    function setMap(uint index, uint value) public {
        aMap[index] = value;
    }

    function rewriteStorage(uint index1, uint val1, uint index2, uint val2, uint index3, uint val3) public {
        this.setMap(index1, val1);
        this.setMap(index2, val2);
        this.setMap(index3, val3);
    }
}
