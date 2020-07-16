
CARGORUN = cargo run --release
BUILTINDIR = builtin
STDDIR = stdlib

test: all
	cargo test --release

testlogs: all
	rm -rf testlogs
	mkdir testlogs
	$(CARGORUN) maketestlogs >/dev/null

evmdebug: all
	$(CARGORUN) evmdebug

TESTEXES = $(BUILTINDIR)/kvstest.mexe $(STDDIR)/queuetest.mexe $(BUILTINDIR)/arraytest.mexe $(BUILTINDIR)/globaltest.mexe $(STDDIR)/priorityqtest.mexe $(STDDIR)/bytearraytest.mexe $(STDDIR)/keccaktest.mexe $(STDDIR)/rlptest.mexe $(BUILTINDIR)/maptest.mexe minitests/codeloadtest.mexe
BUILTINMAOS = $(BUILTINDIR)/array.mao $(BUILTINDIR)/kvs.mao
STDLIBMAOS = $(STDDIR)/bytearray.mao $(STDDIR)/priorityq.mao $(STDDIR)/random.mao $(STDDIR)/queue.mao $(STDDIR)/keccak.mao $(STDDIR)/bytestream.mao $(STDDIR)/stack.mao $(STDDIR)/rlp.mao
STDLIB = $(STDLIBMAOS)

all: $(TESTEXES) arbos

$(BUILTINDIR)/kvstest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/kvstest.mini
	$(CARGORUN) compile $(BUILTINDIR)/kvstest.mini -o $(BUILTINDIR)/kvstest.mexe

$(STDDIR)/queuetest.mexe: $(BUILTINMAOS) $(STDDIR)/queuetest.mini $(STDLIB)
	$(CARGORUN) compile $(STDDIR)/queuetest.mini $(STDLIB) -o $(STDDIR)/queuetest.mexe

$(BUILTINDIR)/arraytest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/arraytest.mini
	$(CARGORUN) compile $(BUILTINDIR)/arraytest.mini -o $(BUILTINDIR)/arraytest.mexe

$(BUILTINDIR)/globaltest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/globaltest.mini
	$(CARGORUN) compile $(BUILTINDIR)/globaltest.mini -o $(BUILTINDIR)/globaltest.mexe

$(STDDIR)/priorityqtest.mexe: $(BUILTINMAOS) $(STDDIR)/priorityqtest.mini $(STDLIB)
	$(CARGORUN) compile $(STDDIR)/priorityqtest.mini $(STDLIB) -o $(STDDIR)/priorityqtest.mexe

$(STDDIR)/bytearraytest.mexe: $(BUILTINMAOS) $(STDDIR)/bytearraytest.mini $(STDLIB)
	$(CARGORUN) compile $(STDDIR)/bytearraytest.mini $(STDLIB) -o $(STDDIR)/bytearraytest.mexe

minitests/codeloadtest.mexe: minitests/codeloadtest.mini
	$(CARGORUN) compile minitests/codeloadtest.mini -o minitests/codeloadtest.mexe

$(STDDIR)/keccaktest.mexe: $(BUILTINMAOS) $(STDDIR)/keccaktest.mini $(STDDIR)/keccak.mao $(STDDIR)/bytearray.mao
	$(CARGORUN) compile $(STDDIR)/keccaktest.mini $(STDDIR)/keccak.mao $(STDDIR)/bytearray.mao -o $(STDDIR)/keccaktest.mexe

$(STDDIR)/rlptest.mexe: $(BUILTINMAOS) $(STDDIR)/rlptest.mini $(STDLIB)
	$(CARGORUN) compile $(STDDIR)/rlptest.mini $(STDLIB) -o $(STDDIR)/rlptest.mexe

$(STDDIR)/priorityq.mao: $(BUILTINMAOS) $(STDDIR)/priorityq.mini
	$(CARGORUN) compile $(STDDIR)/priorityq.mini -c -o $(STDDIR)/priorityq.mao

$(STDDIR)/queue.mao: $(BUILTINMAOS) $(STDDIR)/queue.mini
	$(CARGORUN) compile $(STDDIR)/queue.mini -c -o $(STDDIR)/queue.mao

$(STDDIR)/bytearray.mao: $(BUILTINMAOS) $(STDDIR)/bytearray.mini
	$(CARGORUN) compile $(STDDIR)/bytearray.mini -c -o $(STDDIR)/bytearray.mao

$(STDDIR)/bytestream.mao: $(BUILTINMAOS) $(STDDIR)/bytestream.mini
	$(CARGORUN) compile $(STDDIR)/bytestream.mini -c -o $(STDDIR)/bytestream.mao

$(STDDIR)/random.mao: $(STDDIR)/random.mini
	$(CARGORUN) compile $(STDDIR)/random.mini -c -o $(STDDIR)/random.mao

$(STDDIR)/stack.mao: $(STDDIR)/stack.mini
	$(CARGORUN) compile $(STDDIR)/stack.mini -c -o $(STDDIR)/stack.mao

$(STDDIR)/keccak.mao: $(STDDIR)/keccak.mini		
	$(CARGORUN) compile $(STDDIR)/keccak.mini -c -o $(STDDIR)/keccak.mao

$(STDDIR)/rlp.mao: $(STDDIR)/rlp.mini
	$(CARGORUN) compile $(STDDIR)/rlp.mini -c -o $(STDDIR)/rlp.mao

$(BUILTINDIR)/maptest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/maptest.mini
	$(CARGORUN) compile $(BUILTINDIR)/maptest.mini -o $(BUILTINDIR)/maptest.mexe

$(BUILTINDIR)/array.mao: $(BUILTINDIR)/array.mini
	$(CARGORUN) compile $(BUILTINDIR)/array.mini -c -o $(BUILTINDIR)/array.mao

$(BUILTINDIR)/kvs.mao: $(BUILTINDIR)/kvs.mini
	$(CARGORUN) compile $(BUILTINDIR)/kvs.mini -c -o $(BUILTINDIR)/kvs.mao

ARBOSDIR = arb_os
ARBOSAOS = $(ARBOSDIR)/main.mao $(ARBOSDIR)/accounts.mao $(ARBOSDIR)/messages.mao $(ARBOSDIR)/inbox.mao $(ARBOSDIR)/evmCallStack.mao $(ARBOSDIR)/evmOps.mao $(ARBOSDIR)/codeSegment.mao $(ARBOSDIR)/evmlogs.mao $(ARBOSDIR)/errorHandler.mao $(ARBOSDIR)/gasAccounting.mao $(ARBOSDIR)/contractTemplates.mao $(ARBOSDIR)/tokens.mao $(ARBOSDIR)/arbsys.mao $(ARBOSDIR)/messageBatch.mao $(ARBOSDIR)/chainParameters.mao $(ARBOSDIR)/precompiles.mao $(ARBOSDIR)/signedTx.mao
ARBOS = $(ARBOSDIR)/arbos.mexe

arbos: $(ARBOS)

$(ARBOSDIR)/accounts.mao: $(ARBOSDIR)/accounts.mini
	$(CARGORUN) compile $(ARBOSDIR)/accounts.mini -c -o $(ARBOSDIR)/accounts.mao

$(ARBOSDIR)/messages.mao: $(ARBOSDIR)/messages.mini
	$(CARGORUN) compile $(ARBOSDIR)/messages.mini -c -o $(ARBOSDIR)/messages.mao

$(ARBOSDIR)/main.mao: $(ARBOSDIR)/main.mini
	$(CARGORUN) compile $(ARBOSDIR)/main.mini -c -o $(ARBOSDIR)/main.mao

$(ARBOSDIR)/inbox.mao: $(ARBOSDIR)/inbox.mini
	$(CARGORUN) compile $(ARBOSDIR)/inbox.mini -c -o $(ARBOSDIR)/inbox.mao

$(ARBOSDIR)/evmCallStack.mao: $(ARBOSDIR)/evmCallStack.mini
	$(CARGORUN) compile $(ARBOSDIR)/evmCallStack.mini -c -o $(ARBOSDIR)/evmCallStack.mao

$(ARBOSDIR)/evmOps.mao: $(ARBOSDIR)/evmOps.mini
	$(CARGORUN) compile $(ARBOSDIR)/evmOps.mini -c -o $(ARBOSDIR)/evmOps.mao

$(ARBOSDIR)/codeSegment.mao: $(ARBOSDIR)/codeSegment.mini
	$(CARGORUN) compile $(ARBOSDIR)/codeSegment.mini -c -o $(ARBOSDIR)/codeSegment.mao

$(ARBOSDIR)/evmlogs.mao: $(ARBOSDIR)/evmlogs.mini
	$(CARGORUN) compile $(ARBOSDIR)/evmlogs.mini -c -o $(ARBOSDIR)/evmlogs.mao

$(ARBOSDIR)/errorHandler.mao: $(ARBOSDIR)/errorHandler.mini
	$(CARGORUN) compile $(ARBOSDIR)/errorHandler.mini -c -o $(ARBOSDIR)/errorHandler.mao

$(ARBOSDIR)/gasAccounting.mao: $(ARBOSDIR)/gasAccounting.mini
	$(CARGORUN) compile $(ARBOSDIR)/gasAccounting.mini -c -o $(ARBOSDIR)/gasAccounting.mao

$(ARBOSDIR)/contractTemplates.mao: $(ARBOSDIR)/contractTemplates.mini
	$(CARGORUN) compile $(ARBOSDIR)/contractTemplates.mini -c -o $(ARBOSDIR)/contractTemplates.mao

$(ARBOSDIR)/contractTemplates.mini: src/contracttemplates.rs
	$(CARGORUN) maketemplates

$(ARBOSDIR)/tokens.mao: $(ARBOSDIR)/tokens.mini
	$(CARGORUN) compile $(ARBOSDIR)/tokens.mini -c -o $(ARBOSDIR)/tokens.mao

$(ARBOSDIR)/arbsys.mao: $(ARBOSDIR)/arbsys.mini
	$(CARGORUN) compile $(ARBOSDIR)/arbsys.mini -c -o $(ARBOSDIR)/arbsys.mao

$(ARBOSDIR)/messageBatch.mao: $(ARBOSDIR)/messageBatch.mini
	$(CARGORUN) compile $(ARBOSDIR)/messageBatch.mini -c -o $(ARBOSDIR)/messageBatch.mao

$(ARBOSDIR)/chainParameters.mao: $(ARBOSDIR)/chainParameters.mini
	$(CARGORUN) compile $(ARBOSDIR)/chainParameters.mini -c -o $(ARBOSDIR)/chainParameters.mao

$(ARBOSDIR)/precompiles.mao: $(ARBOSDIR)/precompiles.mini
	$(CARGORUN) compile $(ARBOSDIR)/precompiles.mini -c -o $(ARBOSDIR)/precompiles.mao

$(ARBOSDIR)/signedTx.mao: $(ARBOSDIR)/signedTx.mini
	$(CARGORUN) compile $(ARBOSDIR)/signedTx.mini -c -o $(ARBOSDIR)/signedTx.mao

$(ARBOS): $(ARBOSAOS) $(STDLIB) $(BUILTINMAOS)
	$(CARGORUN) compile $(ARBOSAOS) $(STDLIB) -o $(ARBOS)

arbos.pretty: $(ARBOSAOS) $(STDLIB) $(BUILTINMAOS)
	$(CARGORUN) compile $(ARBOSAOS) $(STDLIB) -f pretty >arbos.pretty

run: arbos
	$(CARGORUN) run $(ARBOS)

compiler: 
	cargo build

clean: 
	rm -f $(BUILTINMAOS) $(TESTEXES) $(STDLIBMAOS) $(ARBOSAOS) $(ARBOSDIR)/*.mexe $(ARBOSDIR)/contractTemplates.mini
