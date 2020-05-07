
test: all
	cargo test

EXES = kvstest.mexe queuetest.mexe arraytest.mexe globaltest.mexe priorityqtest.mexe bytearraytest.mexe maptest.mexe
BUILTINMAOS = builtin/array.mao builtin/kvs.mao

all: $(EXES)

kvstest.mexe: $(BUILTINMAOS) kvstest.mini
	cargo run compile kvstest.mini -o kvstest.mexe

queuetest.mexe: $(BUILTINMAOS) queuetest.mini queue.mini
	cargo run compile queuetest.mini queue.mini -o queuetest.mexe

arraytest.mexe: $(BUILTINMAOS) arraytest.mini
	cargo run compile arraytest.mini -o arraytest.mexe

globaltest.mexe: $(BUILTINMAOS) globaltest.mini
	cargo run compile globaltest.mini -o globaltest.mexe

priorityqtest.mexe: $(BUILTINMAOS) priorityqtest.mini priorityq.mini
	cargo run compile priorityqtest.mini priorityq.mini -o priorityqtest.mexe
	
bytearraytest.mexe: $(BUILTINMAOS) bytearraytest.mini bytearray.mini
	cargo run compile bytearraytest.mini bytearray.mini -o bytearraytest.mexe

maptest.mexe: $(BUILTINMAOS) maptest.mini
	cargo run compile maptest.mini -o maptest.mexe

builtin/array.mao: builtin/array.mini
	cargo run compile builtin/array.mini -c -o builtin/array.mao

builtin/kvs.mao: builtin/kvs.mini
	cargo run compile builtin/kvs.mini -c -o builtin/kvs.mao

compiler: 
	cargo build

clean: 
	rm -f builtin/array.mao builtin/kvs.mao kvstest.mexe queuetest.mexe arraytest.mexe globaltest.mexe priorityqtest.mexe bytearraytest.mexe