pragma solidity >=0.4.21 <0.7.0;


contract SelfDestructor {
    function suicide(address payable beneficiary) public {
        selfdestruct(beneficiary);
    }
}
