CARGORUN = cargo run --release --
ARBOSDIR = arb_os
BUILTINDIR = builtin
STDDIR = stdlib

TEMPLATES = $(ARBOSDIR)/contractTemplates.mini
TESTFILES = $(BUILTINDIR)/kvstest.mexe $(STDDIR)/queuetest.mexe $(BUILTINDIR)/arraytest.mexe $(BUILTINDIR)/globaltest.mexe $(STDDIR)/priorityqtest.mexe $(STDDIR)/bytearraytest.mexe $(STDDIR)/keccaktest.mexe $(STDDIR)/rlptest.mexe $(STDDIR)/storageMapTest.mexe $(BUILTINDIR)/maptest.mexe minitests/codeloadtest.mexe
ARBOS = $(ARBOSDIR)/arbos.mexe

all: $(TEMPLATES) $(TESTFILES) $(ARBOS) test

$(ARBOSDIR)/contractTemplates.mini:
	$(CARGORUN) maketemplates

$(BUILTINDIR)/kvstest.mexe: $(BUILTINDIR)/kvstest.mini
	$(CARGORUN) compile $(BUILTINDIR)/kvstest.mini -o $(BUILTINDIR)/kvstest.mexe

$(STDDIR)/queuetest.mexe: $(STDDIR)/queuetest.mini
	$(CARGORUN) compile $(STDDIR)/queuetest.mini -o $(STDDIR)/queuetest.mexe

$(BUILTINDIR)/arraytest.mexe: $(BUILTINDIR)/arraytest.mini
	$(CARGORUN) compile $(BUILTINDIR)/arraytest.mini -o $(BUILTINDIR)/arraytest.mexe

$(BUILTINDIR)/globaltest.mexe: $(BUILTINDIR)/globaltest.mini
	$(CARGORUN) compile $(BUILTINDIR)/globaltest.mini -o $(BUILTINDIR)/globaltest.mexe

$(STDDIR)/priorityqtest.mexe: $(STDDIR)/priorityqtest.mini $(STDLIB)
	$(CARGORUN) compile $(STDDIR)/priorityqtest.mini $(STDLIB) -o $(STDDIR)/priorityqtest.mexe

$(STDDIR)/storageMapTest.mexe: $(STDDIR)/storageMapTest.mini $(STDLIB)
	$(CARGORUN) compile $(STDDIR)/storageMapTest.mini $(STDLIB) -o $(STDDIR)/storageMapTest.mexe

$(STDDIR)/bytearraytest.mexe: $(STDDIR)/bytearraytest.mini $(STDLIB)
	$(CARGORUN) compile $(STDDIR)/bytearraytest.mini $(STDLIB) -o $(STDDIR)/bytearraytest.mexe

minitests/codeloadtest.mexe: minitests/codeloadtest.mini
	$(CARGORUN) compile minitests/codeloadtest.mini -o minitests/codeloadtest.mexe

$(STDDIR)/keccaktest.mexe: $(STDDIR)/keccaktest.mini
	$(CARGORUN) compile $(STDDIR)/keccaktest.mini $(STDDIR)/keccak.mao $(STDDIR)/bytearray.mao $(STDDIR)/expandingIntArray.mao -o $(STDDIR)/keccaktest.mexe

$(STDDIR)/rlptest.mexe: $(STDDIR)/rlptest.mini $(STDLIB)
	$(CARGORUN) compile $(STDDIR)/rlptest.mini $(STDLIB) -o $(STDDIR)/rlptest.mexe

$(BUILTINDIR)/maptest.mexe: $(BUILTINMAOS) $(BUILTINDIR)/maptest.mini
	$(CARGORUN) compile $(BUILTINDIR)/maptest.mini -o $(BUILTINDIR)/maptest.mexe

$(ARBOSDIR)/arbos.mexe:
	$(CARGORUN) compile "arb_os" -o "arb_os/arbos.mexe"

run:
	cargo run --release -- run "arb_os/arbos.mexe"

test:
	cargo test --release

testlogs: all
	rm -rf testlogs
	mkdir testlogs
	$(CARGORUN) maketestlogs >/dev/null

evmdebug: all
	$(CARGORUN) evmdebug

benchmarks: arbos
	$(CARGORUN) makebenchmarks

clean:
	rm -f $(BUILTINDIR)/*.mexe $(STDDIR)/*.mexe $(ARBOSDIR)/*.mexe $(ARBOSDIR)/contractTemplates.mini