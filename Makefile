
test: kvstest.mexe arraytest.mexe
	cargo test

kvstest.mexe: builtin/array.mao kvstest.mini kvs.mini
	cargo run compile kvstest.mini kvs.mini -o kvstest.mexe

arraytest.mexe: builtin/array.mao arraytest.mini
	cargo run compile arraytest.mini -o arraytest.mexe

builtin/array.mao: builtin/array.mini
	cargo run compile builtin/array.mini -c -o builtin/array.mao

compiler: 
	cargo build

clean: 
	rm -f builtin/array.mao kvstest.mexe arraytest.mexe