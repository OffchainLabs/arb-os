# EVM serialized state output format

This documents the format of serialized EVM state, as produced by the `ArbosTest.getSerializedEVMState` method.

The state is serialized as:

* account address (address encoded as uint256)
* next nonce (uint256)
* eth balance in wei (uint256)
* 1 if account is a contract, 0 otherwise (byte)
* if account is a contract:
  * size in bytes of contract code (uint256)
  * contract code (sequence of bytes)
  * number of non-zero storage cells (uint256)
  * for each storage cell, in sequence:
    * offset (uint256)
    * value (uint256)
* 1 if account has aggregator info, 0 otherwise (byte)
* if account has aggregator info:
  * address that fees should be paid to (address encoded as uint256)
  * per-tx fee to charge in L1 gas (uint256)
* 1 if account has a preferred aggregator, 0 otherwise (byte)
* if account has a preferred aggregator:
  * preferred aggregator address (address encoded as uint256)