pragma solidity >=0.4.21 <0.9.0;

import "./SelfDestructor.sol";


contract Destroyer {
    function destroy(address victim, address payable beneficiary) public {
        SelfDestructor(victim).die(beneficiary);
    }
}
