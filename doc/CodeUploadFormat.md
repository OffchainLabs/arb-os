# Serialization format for code upload



This specification describes the format used for serializing code to be uploaded to ArbOS.  Code in this format will be accepted by the code upload methods in the ArbOwner contract.

## Overall structure

A serialized segment is a binary format that describes a new code segment that can be created. The code segment cannot contain codepoint references to other segments, because the format does not provide any way to represent a cross-segment codepoint reference.

A serialized segment is the concatenation of serialized instructions, with the instructions in reverse order, so that the last instruction in the segment appears first in the serialized segment, and the first instruction of the segment appears last in the serialzied segment. 

Each instruction is given an index, with the first segment to be deserialized given index 1, and the index incrementing for each subsequent instruction that is deserialized.

Every AVM segment must end with an error instruction. This is not included in the serialized segment. It is assigned index zero.

#### Uploading in chunks

A serialized code segment can be uploaded to ArbOS in chunks. Chunks can be of any size, but each chunk must end on a boundary between instructions.

The result of uploading a series of chunks is as if the chunks were concatenated together, and then the final result was deserialized.  (The implementation will deserialize chunks incrementally, but this will not affect the final result.)

## Serializing an instruction

An instruction is serialized as:

* opcode (byte)
* immediate flag (byte: 1 if instruction has an immediate value; 0 otherwise)
* if immediate flag is 1, the serialized immediate value

## Serializing a data value

A serialized value is represented as a type byte followed by type-specific data.

###### Type 0: unsigned integer

Type-specific data is the RLP encoding of the integer value.

###### Type 1: codepoint

Type-specific data is the RLP encoding of an instruction index within the code segment that is currently being deserialized.  The index must be less than the index of the instruction that is currently being deserialized.

###### Type 2: empty buffer

This has no type-specific data.  (Currently non-empty buffers cannot be immediate values for AVM instructions.)

###### Types 10 through 18: tuple

Type 10+N represents an N-tuple.

Type-specific data consists of the serialized data for the N fields of the tuple.