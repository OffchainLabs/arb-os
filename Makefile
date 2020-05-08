
BUILTINDIR = builtin
STDDIR = stdlib

test: all
	cargo test

TESTEXES = $(BUILTINDIR)/kvstest.mexe $(STDDIR)/queuetest.mexe $(BUILTINDIR)/arraytest.mexe $(BUILTINDIR)/globaltest.mexe $(STDDIR)/priorityqtest.mexe $(STDDIR)/bytearraytest.mexe $(BUILTINDIR)/maptest.mexe
BUILTINMAOS = $(BUILTINDIR)/array.mao $(BUILTINDIR)/kvs.mao

all: $(TESTEXES)

$(BUILTINDIR)/kvstest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/kvstest.mini
	cargo run compile $(BUILTINDIR)/kvstest.mini -o $(BUILTINDIR)/kvstest.mexe

$(STDDIR)/queuetest.mexe: $(BUILTINMAOS) $(STDDIR)/queuetest.mini $(STDDIR)/queue.mini
	cargo run compile $(STDDIR)/queuetest.mini $(STDDIR)/queue.mini -o $(STDDIR)/queuetest.mexe

$(BUILTINDIR)/arraytest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/arraytest.mini
	cargo run compile $(BUILTINDIR)/arraytest.mini -o $(BUILTINDIR)/arraytest.mexe

$(BUILTINDIR)/globaltest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/globaltest.mini
	cargo run compile $(BUILTINDIR)/globaltest.mini -o $(BUILTINDIR)/globaltest.mexe

$(STDDIR)/priorityqtest.mexe: $(BUILTINMAOS) $(STDDIR)/priorityqtest.mini $(STDDIR)/priorityq.mini
	cargo run compile $(STDDIR)/priorityqtest.mini $(STDDIR)/priorityq.mini -o $(STDDIR)/priorityqtest.mexe
	
$(STDDIR)/bytearraytest.mexe: $(BUILTINMAOS) $(STDDIR)/bytearraytest.mini $(STDDIR)/bytearray.mini
	cargo run compile $(STDDIR)/bytearraytest.mini $(STDDIR)/bytearray.mini -o $(STDDIR)/bytearraytest.mexe

$(BUILTINDIR)/maptest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/maptest.mini
	cargo run compile $(BUILTINDIR)/maptest.mini -o $(BUILTINDIR)/maptest.mexe

$(BUILTINDIR)/array.mao: $(BUILTINDIR)/array.mini
	cargo run compile $(BUILTINDIR)/array.mini -c -o $(BUILTINDIR)/array.mao

$(BUILTINDIR)/kvs.mao: $(BUILTINDIR)/kvs.mini
	cargo run compile $(BUILTINDIR)/kvs.mini -c -o $(BUILTINDIR)/kvs.mao

compiler: 
	cargo build

clean: 
	rm -f $(BUILTINMAOS) $(TESTEXES)