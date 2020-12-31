CARGORUN = cargo run --release --
ARBOSDIR = arb_os
BUILTINDIR = builtin
STDDIR = stdlib
TESTCONTRACTDIR = contracts/test
TCSRCDIR = $(TESTCONTRACTDIR)/contracts
TCBUILDDIR = $(TESTCONTRACTDIR)/build/contracts
ARBCONTRACTDIR = contracts/arbos
ACSRCDIR = $(ARBCONTRACTDIR)/contracts
ACBUILDDIR = $(ARBCONTRACTDIR)/build/contracts
ARBOS = $(ARBOSDIR)/arbos.mexe

TEMPLATES = $(ARBOSDIR)/contractTemplates.mini
TESTFILES = $(BUILTINDIR)/kvstest.mexe $(STDDIR)/queuetest.mexe $(BUILTINDIR)/arraytest.mexe $(BUILTINDIR)/globaltest.mexe $(STDDIR)/priorityqtest.mexe $(STDDIR)/bytearraytest.mexe $(STDDIR)/keccaktest.mexe $(STDDIR)/biguinttest.mexe $(STDDIR)/rlptest.mexe $(STDDIR)/storageMapTest.mexe $(BUILTINDIR)/maptest.mexe $(STDDIR)/sha256test.mexe $(STDDIR)/ripemd160test.mexe minitests/codeloadtest.mexe $(STDDIR)/fixedpointtest.mexe $(STDDIR)/blstest.mexe
TESTCONTRACTS = $(TCBUILDDIR)/Add.json $(TCBUILDDIR)/ArbSys.json $(TCBUILDDIR)/Fibonacci.json $(TCBUILDDIR)/Migrations.json $(TCBUILDDIR)/PaymentChannel.json $(TCBUILDDIR)/Underfunded.json
ARBOSCONTRACTS = $(ACBUILDDIR)/ArbAddressTable.json $(ACBUILDDIR)/ArbBLS.json $(ACBUILDDIR)/ArbERC20.json $(ACBUILDDIR)/ArbERC721.json $(ACBUILDDIR)/ArbFunctionTable.json $(ACBUILDDIR)/ArbInfo.json $(ACBUILDDIR)/ArbOwner.json $(ACBUILDDIR)/ArbSys.json $(ACBUILDDIR)/ArbosTest.json

COMPILEFLAGS = -i

all: $(TESTFILES) $(TESTCONTRACTS) $(ARBOSCONTRACTS) $(TEMPLATES) $(ARBOS) test
contracts: $(TESTCONTRACTS) $(ARBOSCONTRACTS)

$(ARBOSDIR)/contractTemplates.mini: $(ARBOSCONTRACTS)
	$(CARGORUN) make-templates

$(BUILTINDIR)/kvstest.mexe: $(BUILTINDIR)/kvstest.mini
	$(CARGORUN) compile $(BUILTINDIR)/kvstest.mini -o $(BUILTINDIR)/kvstest.mexe $(COMPILEFLAGS)

$(STDDIR)/queuetest.mexe: $(STDDIR)/queuetest.mini
	$(CARGORUN) compile $(STDDIR)/queuetest.mini -o $(STDDIR)/queuetest.mexe $(COMPILEFLAGS)

$(BUILTINDIR)/arraytest.mexe: $(BUILTINDIR)/arraytest.mini
	$(CARGORUN) compile $(BUILTINDIR)/arraytest.mini -o $(BUILTINDIR)/arraytest.mexe $(COMPILEFLAGS)

$(BUILTINDIR)/globaltest.mexe: $(BUILTINDIR)/globaltest.mini
	$(CARGORUN) compile $(BUILTINDIR)/globaltest.mini -o $(BUILTINDIR)/globaltest.mexe $(COMPILEFLAGS)

$(STDDIR)/priorityqtest.mexe: $(STDDIR)/priorityqtest.mini
	$(CARGORUN) compile $(STDDIR)/priorityqtest.mini -o $(STDDIR)/priorityqtest.mexe $(COMPILEFLAGS)

$(STDDIR)/storageMapTest.mexe: $(STDDIR)/storageMapTest.mini
	$(CARGORUN) compile $(STDDIR)/storageMapTest.mini -o $(STDDIR)/storageMapTest.mexe $(COMPILEFLAGS)

$(STDDIR)/bytearraytest.mexe: $(STDDIR)/bytearraytest.mini
	$(CARGORUN) compile $(STDDIR)/bytearraytest.mini -o $(STDDIR)/bytearraytest.mexe $(COMPILEFLAGS)

$(STDDIR)/blstest.mexe: $(BUILTINMAOS) $(STDDIR)/blstest.mini $(STDDIR)
	$(CARGORUN) compile $(STDDIR)/blstest.mini $(STDLIB) -o $(STDDIR)/blstest.mexe

$(STDDIR)/fixedpointtest.mexe: $(STDDIR)/fixedpointtest.mini $(STDDIR)/fixedpoint.mini
	$(CARGORUN) compile $(STDDIR)/fixedpointtest.mini -o $(STDDIR)/fixedpointtest.mexe

$(STDDIR)/bytearraybench.mexe: $(BUILTINMAOS) $(STDDIR)/bytearraybench.mini $(STDLIB)
	$(CARGORUN) compile $(STDDIR)/bytearraybench.mini $(STDLIB) -o $(STDDIR)/bytearraybench.mexe

$(STDDIR)/bufferopcodetest.mexe: $(BUILTINMAOS) $(STDDIR)/bufferopcodetest.mini
	$(CARGORUN) compile $(STDDIR)/bufferopcodetest.mini -o $(STDDIR)/bufferopcodetest.mexe

minitests/codeloadtest.mexe: minitests/codeloadtest.mini
	$(CARGORUN) compile minitests/codeloadtest.mini -o minitests/codeloadtest.mexe $(COMPILEFLAGS)

$(STDDIR)/keccaktest.mexe: $(STDDIR)/keccaktest.mini $(STDDIR)/keccak.mini $(STDDIR)/bytearray.mini $(STDDIR)/expandingIntArray.mini
	$(CARGORUN) compile $(STDDIR)/keccaktest.mini -o $(STDDIR)/keccaktest.mexe $(COMPILEFLAGS)

$(STDDIR)/biguinttest.mexe: $(STDDIR)/biguinttest.mini $(STDDIR)/biguint.mini
	$(CARGORUN) compile $(STDDIR)/biguinttest.mini -o $(STDDIR)/biguinttest.mexe

$(STDDIR)/sha256test.mexe: $(STDDIR)/sha256test.mini $(STDDIR)/sha256.mini
	$(CARGORUN) compile $(STDDIR)/sha256test.mini -o $(STDDIR)/sha256test.mexe $(COMPILEFLAGS)

$(STDDIR)/ripemd160test.mexe: $(STDDIR)/ripemd160test.mini $(STDDIR)/ripemd160.mini
	$(CARGORUN) compile $(STDDIR)/ripemd160test.mini -o $(STDDIR)/ripemd160test.mexe

$(STDDIR)/rlptest.mexe: $(BUILTINMAOS) $(STDDIR)/rlptest.mini
	$(CARGORUN) compile $(STDDIR)/rlptest.mini -o $(STDDIR)/rlptest.mexe $(COMPILEFLAGS)

$(BUILTINDIR)/maptest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/maptest.mini
	$(CARGORUN) compile $(BUILTINDIR)/maptest.mini -o $(BUILTINDIR)/maptest.mexe $(COMPILEFLAGS)

$(ARBOSDIR)/arbos.mexe: $(ARBOSDIR) $(STDDIR) $(BUILTINDIR) src/compile/miniconstants.rs
	$(CARGORUN) compile "arb_os" -o "arb_os/arbos.mexe"

$(TESTCONTRACTS): $(TCSRCDIR) $(ACSRCDIR)/ArbSys.sol
	(cd contracts/test; truffle compile)

$(ARBOSCONTRACTS): $(ACSRCDIR)
	(cd contracts/arbos; truffle compile)

run:
	cargo run --release -- run "arb_os/arbos.mexe"

test: $(TEMPLATES) $(ARBOS)
	cargo test --release 

evmtest: $(ARBOS)
	$(CARGORUN) evm-tests

evmtestlogs: $(ARBOS)
	rm -rf evm-test-logs
	mkdir evm-test-logs
	$(CARGORUN) evm-tests --savelogs

testlogs: $(TEMPLATES) $(ARBOS)
	rm -rf testlogs
	mkdir testlogs
	$(CARGORUN) make-test-logs >/dev/null

evmdebug: all
	$(CARGORUN) evm-debug

benchmark: $(TEMPLATES) $(ARBOS)
	$(CARGORUN) make-benchmarks

clean:
	rm -f $(BUILTINDIR)/*.mexe $(STDDIR)/*.mexe $(ARBOSDIR)/*.mexe minitests/*.mexe $(ARBOSDIR)/contractTemplates.mini $(TCBUILDDIR)/*.json $(ACBUILDDIR)/*.json