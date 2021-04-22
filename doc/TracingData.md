# Tracing output from ArbOS

ArbOS produces debugprints useful for tracing transaction execution. This document describes the tracing output.

## Transaction call/return/revert/create

When a top-level transaction completes, ArbOS will emit a debugprint to give a transaction trace of the contract's execution, including internal transactions, and *create/create2* operations.

ArbOS will debugprint the 2-tuple (20000, *traceData*).  traceData is a linked list of trace items. An element of the list is a 2-tuple (*item, restOfList*), where restOfList is an empty tuple if this is the end of the list. The items appear on the list in the order that they occurred during the transaction's execution.

Every item has the structure *(itemType, typeSpecificData)*. 

#### Item type 0: call

Type-specific data is an 8-tuple:

* call type (uint). [call: 0, callcode: 1, delegatecall: 2, staticcall: 3, constructor: 4]
* calldata size (uint)
* calldata contents (buffer)
* callvalue (uint)
* caller (address encoded as uint)
* destination address (address encoded as uint)
* max gas (uint)
* gas price (uint)

#### Item type 1: return or revert

Type-specific data is a 4-tuple:

* result code (uint). [success: 0, revert: 1, other codes per ArbOS tx result codes]
* returndata size (uint)
* returndata contents (buffer)
* gas used (uint)

#### Item type 2: create

Type-specific data is a 3-tuple:

* EVM code size (uint)
* EVM code (buffer)
* deployed at address (address encoded as uint)

#### Item type 3: create2

Type-specific data is a 5-tuple:

* EVM code size (uint)
* EVM code (buffer)
* requesting address (address encoded as uint)
* salt (bytes32)
* deployed at address (address encoded as uint)

