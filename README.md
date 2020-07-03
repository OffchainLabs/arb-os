# ArbOS and Mini compiler

ArbOS is the "operating system" that runs at Layer 2 on an Arbitrum chain, to manage the chain's operation, maintain security, isolate contracts from each other, manage contract lifecycles, and account for and charge for resource usage.

ArbOS is written in the Mini language. The Mini compiler is also in this repo.  It compiles programs written in Mini, generating code to run on the Arbitrum Virtual Machine.  The compiler is accompanied by an AVM emulator, with associated debugger and profiler.

To build and test everything, do `make clean` then `make`.
