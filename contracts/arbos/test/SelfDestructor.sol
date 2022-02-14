pragma solidity >=0.4.21 <0.9.0;


contract SelfDestructor {
    function die(address payable beneficiary) public {
        selfdestruct(beneficiary);
    }
}
