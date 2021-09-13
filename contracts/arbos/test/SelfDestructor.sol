pragma solidity >=0.4.21 <0.7.0;


contract SelfDestructor {
    function die(address payable beneficiary) public {
        selfdestruct(beneficiary);
    }
}
