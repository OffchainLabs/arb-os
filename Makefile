
BUILTINDIR = builtin
STDDIR = stdlib

test: all
	cargo test --release

evmdebug: all
	cargo run evmdebug

TESTEXES = $(BUILTINDIR)/kvstest.mexe $(BUILTINDIR)/treekvstest.mexe $(BUILTINDIR)/cuckookvstest.mexe $(STDDIR)/queuetest.mexe $(BUILTINDIR)/arraytest.mexe $(BUILTINDIR)/globaltest.mexe $(STDDIR)/priorityqtest.mexe $(STDDIR)/bytearraytest.mexe $(STDDIR)/keccaktest.mexe $(BUILTINDIR)/maptest.mexe minitests/codeloadtest.mexe
BUILTINMAOS = $(BUILTINDIR)/array.mao $(BUILTINDIR)/kvs.mao $(BUILTINDIR)/treekvs.mao $(BUILTINDIR)/cuckookvs.mao
STDLIBMAOS = $(STDDIR)/bytearray.mao $(STDDIR)/priorityq.mao $(STDDIR)/random.mao $(STDDIR)/queue.mao $(STDDIR)/keccak.mao $(STDDIR)/bytestream.mao $(STDDIR)/stack.mao
STDLIB = $(STDLIBMAOS)

all: $(TESTEXES) runtime

$(BUILTINDIR)/kvstest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/kvstest.mini
	cargo run compile $(BUILTINDIR)/kvstest.mini -o $(BUILTINDIR)/kvstest.mexe

$(BUILTINDIR)/treekvstest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/treekvstest.mini
	cargo run compile $(BUILTINDIR)/treekvstest.mini $(BUILTINDIR)/treekvs.mao -o $(BUILTINDIR)/treekvstest.mexe

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

$(BUILTINDIR)/treekvs.mao: $(BUILTINDIR)/treekvs.mini
	cargo run compile $(BUILTINDIR)/treekvs.mini -c -o $(BUILTINDIR)/treekvs.mao

$(BUILTINDIR)/cuckookvs.mao: $(BUILTINDIR)/cuckookvs.mini
	cargo run compile $(BUILTINDIR)/cuckookvs.mini -c -o $(BUILTINDIR)/cuckookvs.mao

RUNTIMEDIR = arbruntime
RUNTIMEMAOS = $(RUNTIMEDIR)/main.mao $(RUNTIMEDIR)/accounts.mao $(RUNTIMEDIR)/messages.mao $(RUNTIMEDIR)/inbox.mao $(RUNTIMEDIR)/evmCallStack.mao $(RUNTIMEDIR)/evmOps.mao $(RUNTIMEDIR)/codeSegment.mao $(RUNTIMEDIR)/evmlogs.mao $(RUNTIMEDIR)/errorHandler.mao $(RUNTIMEDIR)/gasAccounting.mao
RUNTIME = $(RUNTIMEDIR)/runtime.mexe

runtime: $(RUNTIME)

$(RUNTIMEDIR)/accounts.mao: $(RUNTIMEDIR)/accounts.mini
	cargo run compile $(RUNTIMEDIR)/accounts.mini -c -o $(RUNTIMEDIR)/accounts.mao

$(RUNTIMEDIR)/messages.mao: $(RUNTIMEDIR)/messages.mini
	cargo run compile $(RUNTIMEDIR)/messages.mini -c -o $(RUNTIMEDIR)/messages.mao

$(RUNTIMEDIR)/main.mao: $(RUNTIMEDIR)/main.mini
	cargo run compile $(RUNTIMEDIR)/main.mini -c -o $(RUNTIMEDIR)/main.mao

$(RUNTIMEDIR)/inbox.mao: $(RUNTIMEDIR)/inbox.mini
	cargo run compile $(RUNTIMEDIR)/inbox.mini -c -o $(RUNTIMEDIR)/inbox.mao

$(RUNTIMEDIR)/evmCallStack.mao: $(RUNTIMEDIR)/evmCallStack.mini
	cargo run compile $(RUNTIMEDIR)/evmCallStack.mini -c -o $(RUNTIMEDIR)/evmCallStack.mao

$(RUNTIMEDIR)/evmOps.mao: $(RUNTIMEDIR)/evmOps.mini
	cargo run compile $(RUNTIMEDIR)/evmOps.mini -c -o $(RUNTIMEDIR)/evmOps.mao

$(RUNTIMEDIR)/codeSegment.mao: $(RUNTIMEDIR)/codeSegment.mini
	cargo run compile $(RUNTIMEDIR)/codeSegment.mini -c -o $(RUNTIMEDIR)/codeSegment.mao

$(RUNTIMEDIR)/evmlogs.mao: $(RUNTIMEDIR)/evmlogs.mini
	cargo run compile $(RUNTIMEDIR)/evmlogs.mini -c -o $(RUNTIMEDIR)/evmlogs.mao

$(RUNTIMEDIR)/errorHandler.mao: $(RUNTIMEDIR)/errorHandler.mini
	cargo run compile $(RUNTIMEDIR)/errorHandler.mini -c -o $(RUNTIMEDIR)/errorHandler.mao

$(RUNTIMEDIR)/gasAccounting.mao: $(RUNTIMEDIR)/gasAccounting.mini
	cargo run compile $(RUNTIMEDIR)/gasAccounting.mini -c -o $(RUNTIMEDIR)/gasAccounting.mao

$(RUNTIME): $(RUNTIMEMAOS) $(STDLIB) $(BUILTINMAOS)
	cargo run compile $(RUNTIMEMAOS) $(STDLIB) -o $(RUNTIME)

runtime.pretty: $(RUNTIMEMAOS) $(STDLIB) $(BUILTINMAOS)
	cargo run compile $(RUNTIMEMAOS) $(STDLIB) -f pretty >runtime.pretty

run: runtime
	cargo run run $(RUNTIME)

compiler: 
	cargo build

clean: 
	rm -f $(BUILTINMAOS) $(TESTEXES) $(STDLIBMAOS) $(RUNTIMEMAOS) $(RUNTIMEDIR)/*.mexe
