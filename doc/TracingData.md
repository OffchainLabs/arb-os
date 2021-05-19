# Tracing Data Emitted by ArbOS

ArbOS emits tracing data for any nodes that want to get more information about the execution of EVM transactions. This document describes how that data is emitted, and its format.

## Emitting trace info as debugprints

Soon after emitting the log item that is a transaction receipt, ArbOS does a debugprint to give tracing information about that transaction.  The debugprint contents are a 2-tuple `(20000, traceData)` where `traceData` is a tuple structure described below.

ArbOS will emit a trace debugprint for almost every transaction that it runs. It typically will not emit a trace debugprint for submitted transactions that cannot run, such as ones with bad sequence numbers, or inadequate funds for callvalue, or other failure cases that would make a transaction not runnable on Ethereum. For these failing transactions, ArbOS will emit a transaction receipt giving the failure reason but will not emit a tracing debugprint.

There is one case where ArbOS does run a transaction and emit a receipt for it, but does not emit trace information: a successful redemption of a retryable transaction ticket.  At present, ArbOS emits trace information for the "outer" transaction that makes the call to redeem the ticket, but it does not emit trace information for the "inner" redemption transaction, even though there will be a receipt for the outer transaction (always) and a separate receipt for the inner transaction (if it succeeds).  [The treatment of this use case is likely to change in the future.]

## Format of the traceData

The traceData is a tuple-structured linked list, representing a sequence of events.  The list is either an empty tuple denoting an empty list, or a 2-tuple `(firstEvent, restOfList)`.

An event is a 2-tuple `(eventType, eventData)`, where `eventType` is an integer and the format of `eventData` depends on the event type.

#### Event type 0: call

Type-specific data is a 7-tuple:

* call type (uint) [0: call, 1: callcode, 2: delegatecall, 3: staticcall]
* calldata (3-tuple: nbytes, offset, buffer)
* callvalue (uint)
* from (address formatted as uint)
* to (address formatted as uint; 0 means this is a contract deploy)
* gas (uint)
* gas price paid (uint)

#### Event type 1: return/revert

Type-specific data is a 3-tuple:

* result code (uint)
* returndata (3-tuple: nbytes, offset, buffer)
* gas used (uint)

#### Event type 2: create

Type-specific data is a 2-tuple:

* code (3-tuple: nbytes, offset, buffer)
* deploy address (address encoded as uint)

#### Event type 3: create2

Type-specific data is a 4-tuple:

* code (3-tuple: nbytes, offset, buffer)
* address that invoked create2 (address encoded as uint)
* salt (uint)
* deploy address (address encoded as uint)

