
BUILTINDIR = builtin
STDDIR = stdlib

test: all
	cargo test --release

testlogs: all
	rm -rf testlogs
	mkdir testlogs
	cargo run --release maketestlogs >/dev/null

evmdebug: all
	cargo run evmdebug

TESTEXES = $(BUILTINDIR)/kvstest.mexe $(BUILTINDIR)/cuckookvstest.mexe $(STDDIR)/queuetest.mexe $(BUILTINDIR)/arraytest.mexe $(BUILTINDIR)/globaltest.mexe $(STDDIR)/priorityqtest.mexe $(STDDIR)/bytearraytest.mexe $(STDDIR)/keccaktest.mexe $(BUILTINDIR)/maptest.mexe minitests/codeloadtest.mexe
BUILTINMAOS = $(BUILTINDIR)/array.mao $(BUILTINDIR)/kvs.mao $(BUILTINDIR)/cuckookvs.mao
STDLIBMAOS = $(STDDIR)/bytearray.mao $(STDDIR)/priorityq.mao $(STDDIR)/random.mao $(STDDIR)/queue.mao $(STDDIR)/keccak.mao $(STDDIR)/bytestream.mao $(STDDIR)/stack.mao
STDLIB = $(STDLIBMAOS)

all: $(TESTEXES) arbos

$(BUILTINDIR)/kvstest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/kvstest.mini
	cargo run compile $(BUILTINDIR)/kvstest.mini -o $(BUILTINDIR)/kvstest.mexe

$(BUILTINDIR)/cuckookvstest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/cuckookvstest.mini
	cargo run compile $(BUILTINDIR)/cuckookvstest.mini $(BUILTINDIR)/cuckookvs.mao -o $(BUILTINDIR)/cuckookvstest.mexe

$(STDDIR)/queuetest.mexe: $(BUILTINMAOS) $(STDDIR)/queuetest.mini $(STDLIB)
	cargo run compile $(STDDIR)/queuetest.mini $(STDLIB) -o $(STDDIR)/queuetest.mexe

$(BUILTINDIR)/arraytest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/arraytest.mini
	cargo run compile $(BUILTINDIR)/arraytest.mini -o $(BUILTINDIR)/arraytest.mexe

$(BUILTINDIR)/globaltest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/globaltest.mini
	cargo run compile $(BUILTINDIR)/globaltest.mini -o $(BUILTINDIR)/globaltest.mexe

$(STDDIR)/priorityqtest.mexe: $(BUILTINMAOS) $(STDDIR)/priorityqtest.mini $(STDLIB)
	cargo run compile $(STDDIR)/priorityqtest.mini $(STDLIB) -o $(STDDIR)/priorityqtest.mexe

$(STDDIR)/bytearraytest.mexe: $(BUILTINMAOS) $(STDDIR)/bytearraytest.mini $(STDLIB)
	cargo run compile $(STDDIR)/bytearraytest.mini $(STDLIB) -o $(STDDIR)/bytearraytest.mexe

minitests/codeloadtest.mexe: minitests/codeloadtest.mini
	cargo run compile minitests/codeloadtest.mini -o minitests/codeloadtest.mexe

$(STDDIR)/keccaktest.mexe: $(BUILTINMAOS) $(STDDIR)/keccaktest.mini $(STDDIR)/keccak.mao $(STDDIR)/bytearray.mao
	cargo run compile $(STDDIR)/keccaktest.mini $(STDDIR)/keccak.mao $(STDDIR)/bytearray.mao -o $(STDDIR)/keccaktest.mexe

$(STDDIR)/priorityq.mao: $(BUILTINMAOS) $(STDDIR)/priorityq.mini
	cargo run compile $(STDDIR)/priorityq.mini -c -o $(STDDIR)/priorityq.mao

$(STDDIR)/queue.mao: $(BUILTINMAOS) $(STDDIR)/queue.mini
	cargo run compile $(STDDIR)/queue.mini -c -o $(STDDIR)/queue.mao

$(STDDIR)/bytearray.mao: $(BUILTINMAOS) $(STDDIR)/bytearray.mini
	cargo run compile $(STDDIR)/bytearray.mini -c -o $(STDDIR)/bytearray.mao

$(STDDIR)/bytestream.mao: $(BUILTINMAOS) $(STDDIR)/bytestream.mini
	cargo run compile $(STDDIR)/bytestream.mini -c -o $(STDDIR)/bytestream.mao

$(STDDIR)/random.mao: $(STDDIR)/random.mini
	cargo run compile $(STDDIR)/random.mini -c -o $(STDDIR)/random.mao

$(STDDIR)/stack.mao: $(STDDIR)/stack.mini
	cargo run compile $(STDDIR)/stack.mini -c -o $(STDDIR)/stack.mao

$(STDDIR)/keccak.mao: $(STDDIR)/keccak.mini		
	cargo run compile $(STDDIR)/keccak.mini -c -o $(STDDIR)/keccak.mao

$(BUILTINDIR)/maptest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/maptest.mini
	cargo run compile $(BUILTINDIR)/maptest.mini -o $(BUILTINDIR)/maptest.mexe

$(BUILTINDIR)/array.mao: $(BUILTINDIR)/array.mini
	cargo run compile $(BUILTINDIR)/array.mini -c -o $(BUILTINDIR)/array.mao

$(BUILTINDIR)/kvs.mao: $(BUILTINDIR)/kvs.mini
	cargo run compile $(BUILTINDIR)/kvs.mini -c -o $(BUILTINDIR)/kvs.mao

$(BUILTINDIR)/cuckookvs.mao: $(BUILTINDIR)/cuckookvs.mini
	cargo run compile $(BUILTINDIR)/cuckookvs.mini -c -o $(BUILTINDIR)/cuckookvs.mao

ARBOSDIR = arb_os
ARBOSAOS = $(ARBOSDIR)/main.mao $(ARBOSDIR)/accounts.mao $(ARBOSDIR)/messages.mao $(ARBOSDIR)/inbox.mao $(ARBOSDIR)/evmCallStack.mao $(ARBOSDIR)/evmOps.mao $(ARBOSDIR)/codeSegment.mao $(ARBOSDIR)/evmlogs.mao $(ARBOSDIR)/errorHandler.mao $(ARBOSDIR)/gasAccounting.mao $(ARBOSDIR)/contractTemplates.mao $(ARBOSDIR)/tokens.mao $(ARBOSDIR)/arbsys.mao $(ARBOSDIR)/messageBatch.mao
ARBOS = $(ARBOSDIR)/arbos.mexe

arbos: $(ARBOS)

$(ARBOSDIR)/accounts.mao: $(ARBOSDIR)/accounts.mini
	cargo run compile $(ARBOSDIR)/accounts.mini -c -o $(ARBOSDIR)/accounts.mao

$(ARBOSDIR)/messages.mao: $(ARBOSDIR)/messages.mini
	cargo run compile $(ARBOSDIR)/messages.mini -c -o $(ARBOSDIR)/messages.mao

$(ARBOSDIR)/main.mao: $(ARBOSDIR)/main.mini
	cargo run compile $(ARBOSDIR)/main.mini -c -o $(ARBOSDIR)/main.mao

$(ARBOSDIR)/inbox.mao: $(ARBOSDIR)/inbox.mini
	cargo run compile $(ARBOSDIR)/inbox.mini -c -o $(ARBOSDIR)/inbox.mao

$(ARBOSDIR)/evmCallStack.mao: $(ARBOSDIR)/evmCallStack.mini
	cargo run compile $(ARBOSDIR)/evmCallStack.mini -c -o $(ARBOSDIR)/evmCallStack.mao

$(ARBOSDIR)/evmOps.mao: $(ARBOSDIR)/evmOps.mini
	cargo run compile $(ARBOSDIR)/evmOps.mini -c -o $(ARBOSDIR)/evmOps.mao

$(ARBOSDIR)/codeSegment.mao: $(ARBOSDIR)/codeSegment.mini
	cargo run compile $(ARBOSDIR)/codeSegment.mini -c -o $(ARBOSDIR)/codeSegment.mao

$(ARBOSDIR)/evmlogs.mao: $(ARBOSDIR)/evmlogs.mini
	cargo run compile $(ARBOSDIR)/evmlogs.mini -c -o $(ARBOSDIR)/evmlogs.mao

$(ARBOSDIR)/errorHandler.mao: $(ARBOSDIR)/errorHandler.mini
	cargo run compile $(ARBOSDIR)/errorHandler.mini -c -o $(ARBOSDIR)/errorHandler.mao

$(ARBOSDIR)/gasAccounting.mao: $(ARBOSDIR)/gasAccounting.mini
	cargo run compile $(ARBOSDIR)/gasAccounting.mini -c -o $(ARBOSDIR)/gasAccounting.mao

$(ARBOSDIR)/contractTemplates.mao: $(ARBOSDIR)/contractTemplates.mini
	cargo run compile $(ARBOSDIR)/contractTemplates.mini -c -o $(ARBOSDIR)/contractTemplates.mao

$(ARBOSDIR)/contractTemplates.mini: src/contracttemplates.rs
	cargo run maketemplates

$(ARBOSDIR)/tokens.mao: $(ARBOSDIR)/tokens.mini
	cargo run compile $(ARBOSDIR)/tokens.mini -c -o $(ARBOSDIR)/tokens.mao

$(ARBOSDIR)/arbsys.mao: $(ARBOSDIR)/arbsys.mini
	cargo run compile $(ARBOSDIR)/arbsys.mini -c -o $(ARBOSDIR)/arbsys.mao

$(ARBOSDIR)/messageBatch.mao: $(ARBOSDIR)/messageBatch.mini
	cargo run compile $(ARBOSDIR)/messageBatch.mini -c -o $(ARBOSDIR)/messageBatch.mao

$(ARBOS): $(ARBOSAOS) $(STDLIB) $(BUILTINMAOS)
	cargo run compile $(ARBOSAOS) $(STDLIB) -o $(ARBOS)

arbos.pretty: $(ARBOSAOS) $(STDLIB) $(BUILTINMAOS)
	cargo run compile $(ARBOSAOS) $(STDLIB) -f pretty >arbos.pretty

run: arbos
	cargo run run $(ARBOS)

compiler: 
	cargo build

clean: 
	rm -f $(BUILTINMAOS) $(TESTEXES) $(STDLIBMAOS) $(ARBOSAOS) $(ARBOSDIR)/*.mexe $(ARBOSDIR)/contractTemplates.mini
