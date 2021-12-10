pragma solidity >=0.4.21 <0.7.0;

contract PRConstructor {

    constructor(uint val) payable public {

        require(val == 0);

    }

}
