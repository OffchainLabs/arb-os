pragma solidity >=0.4.21 <0.7.0;

import "./SelfDestructor.sol";


contract ConstructorSD {
    constructor(address victim, address payable beneficiary) public {
        SelfDestructor(victim).suicide(beneficiary);
    }
}