pragma solidity >=0.4.21 <0.7.0;

import "../builtin/ArbSys.sol";

contract Fibonacci {

uint[] fibseries;

  event TestEvent(uint number);

  // n = how many in the series to return
  function generateFib(uint n) public payable {

    // set 1st and 2nd entries
    fibseries.push(1);
    fibseries.push(1);

    // generate subsequent entries
    for (uint i=2; i < n ; i++) {
      fibseries.push(fibseries[i-1] + fibseries[i-2]);
    }

    emit TestEvent(n);

  }

  function doFib(uint n) public returns (uint) {
    generateFib(n+1);
    return getFib(n);
  }

  function getFib(uint n) public view returns (uint) {
    return fibseries[n];
  }

  // function getFib(uint n) public view returns (uint, uint) {
  //   return (fibseries[n], fibseries[n + 1]);
  // }

  function isTopLevel() public returns (bool) {
    return ArbSys(address(100)).isTopLevelCall();
  }
}
