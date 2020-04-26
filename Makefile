
test: all
	cargo test

all: kvstest.mexe queuetest.mexe arraytest.mexe globaltest.mexe

kvstest.mexe: builtin/array.mao kvstest.mini kvs.mini
	cargo run compile kvstest.mini kvs.mini -o kvstest.mexe

queuetest.mexe: builtin/array.mao queuetest.mini queue.mini
	cargo run compile queuetest.mini queue.mini -o queuetest.mexe

arraytest.mexe: builtin/array.mao arraytest.mini
	cargo run compile arraytest.mini -o arraytest.mexe

globaltest.mexe: builtin/array.mao globaltest.mini
	cargo run compile globaltest.mini -o globaltest.mexe

builtin/array.mao: builtin/array.mini
	cargo run compile builtin/array.mini -c -o builtin/array.mao

compiler: 
	cargo build

clean: 
	rm -f builtin/array.mao kvstest.mexe arraytest.mexe