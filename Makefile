
# Add your new mexe here
minitest_outputs = codeloadtest simple-closure closure
upgrade_outputs = regcopy_new regcopy_old upgrade1_new upgrade1_old
looptest_outputs = upgrade2_new upgrade2_old
builtin_outputs = arraytest globaltest kvstest maptest
stdlib_outputs = addressSetTest biguinttest blstest bytearraytest expandingIntArrayTest fixedpointtest keccaktest priorityqtest queuetest ripemd160test rlptest storageMapTest sha256test

builtin_mexes = $(patsubst %,builtin/%.mexe, $(builtin_outputs))
stdlib_mexes = $(patsubst %,stdlib/%.mexe, $(stdlib_outputs))
minitest_mexes = $(patsubst %,minitests/%.mexe, $(minitest_outputs))
upgrade_mexes = $(patsubst %,upgradetests/%.mexe, $(upgrade_outputs))
looptest_mexes = $(patsubst %,looptest/%.mexe, $(looptest_outputs))

test_mexes = $(builtin_mexes) $(stdlib_mexes) $(minitest_mexes) $(upgrade_mexes) $(looptest_mexes)

compile = ./target/release/mini compile $(compile_options)
upgrade = ./target/release/mini gen-upgrade-code
run     = ./target/release/mini

mini_files = */*.mini

consts = arb_os/constants.json
done = "\e[38;5;161;1mdone!\e[0;0m\n"

# user targets

.make/all: always arb_os/arbos.mexe $(test_mexes) arb_os/arbos-upgrade.mexe arb_os/upgrade.json .make/test
	@printf "\e[38;5;161;1mdone building %s\e[0;0m\n" $$(expr $$(echo $? | wc -w) - 1)
	@touch .make/all

arbos: arb_os/arbos.mexe
	@printf $(done)

upgrade: arb_os/arbos-upgrade.mexe arb_os/upgrade.json
	@printf $(done)

contracts: .make/solidity
	@printf $(done)

paramslist: parameters.json
	@printf $(done)

evmtest: arb_os/arbos.mexe .make/tools
	$(run) evm-tests

evmlogs: evm-test-logs
	@printf $(done)

evmdebug: arb_os/arbos.mexe .make/tools
	$(run) evm-debug

replay: replayTests
	@printf $(done)

coverage: always lcov-mini.info
	@printf $(done)

benchmark: arb_os/arbos.mexe .make/tools
	$(run) make-benchmarks

test: .make/test
	cargo test --release

push: .make/push
	@printf "\e[38;5;161;1mReady for push!\e[0;0m\n"

ci: .make/all replayTests lcov-mini.info
	@printf "Made ci products\n"

clean:
	@rm -f {builtin,stdlib,upgradetests,minitests,looptest}/*.mexe arb_os/{arbos,arbos-upgrade}.mexe
	@rm -f arbos/{upgrade.json,contractTemplates.mini}
	@rm -rf contracts/artifacts contracts/cache
	@rm -f */*.cov lcov.info lcov-mini.info .make/*


# pattern rules

builtin/%.mexe: builtin/%.mini builtin/*.mini $(consts) .make/tools
	$(compile) -c $(consts) $< -o $@ -t

stdlib/%.mexe: stdlib/%.mini stdlib/*.mini builtin/*.mini $(consts) .make/tools
	$(compile) -c $(consts) $< -o $@ -t

minitests/%.mexe: minitests/%.mini minitests/*.mini stdlib/*.mini builtin/*.mini $(consts) .make/tools
	$(compile) -c $(consts) $< -o $@ -t

upgradetests/%.mexe: upgradetests/%.mini upgradetests/*.mini stdlib/*.mini builtin/*.mini $(consts) .make/tools
	$(compile) -c $(consts) $< -o $@ -t

arb_os/arbos.mexe: arb_os/*.mini arb_os/bridge_arbos_versions.mini arb_os/contractTemplates.mini stdlib/*.mini builtin/*.mini $(consts) .make/tools
	$(compile) arb_os -o $@ -m

parameters.json: arb_os/constants.json .make/tools
	$(run) make-parameters-list -c arb_os/constants.json > $<

replayTests: arb_os/arbos.mexe .make/tools
	mkdir -p testlogs replayTests
	$(run) make-test-logs > /dev/null
	mv testlogs/* replayTests/

evm-test-logs: arb_os/arbos.mexe .make/tools
	rm -rf evm-test-logs
	mkdir -p evm-test-logs
	$(run) evm-tests --savelogs

arb_os/contractTemplates.mini: .make/tools
	$(run) make-templates


# Upgrade tests

upgradetests/upgrade1_new.mexe: upgradetests/upgrade1_new.mini $(consts) .make/tools
	$(compile) -c $(consts) $< -o $@

looptest/upgrade2_new.mexe: looptest/upgrade2_new.mini looptest/*.mini looptest/upgrade2.toml .make/tools
	$(compile) looptest/upgrade2_new.mini -c $(consts) -o $@

looptest/bridge2.mini upgradetests/impl2.mini &: looptest/upgrade2_base.mexe looptest/upgrade2_old.mexe looptest/upgrade2.toml .make/tools
	$(upgrade) looptest/upgrade2_old.mexe $< looptest/bridge2.mini impl2 looptest/upgrade2.toml

looptest/upgrade2_base.mexe: looptest/upgrade2_new.mini .make/tools
	$(compile) $< -c arb_os/constants.json -o looptest/upgrade2_base.mexe

looptest/upgrade2_old.mexe: looptest/upgrade2_old.mini stdlib/*.mini builtin/*.mini .make/tools
	$(compile) -c $(consts) $< -o $@ -t


# ArbOS upgrade
arbos_source_all = $(wildcard arb_os/*.mini) $(consts) stdlib/*.mini builtin/*.mini arb_os/contractTemplates.mini
arbos_source_no_bridge = $(filter-out arb_os/bridge_arbos_versions.mini, $(arbos_source_all))

arb_os/upgrade.json: arb_os/arbos-upgrade.mexe .make/tools
	$(run) serialize-upgrade arb_os/arbos-upgrade.mexe > $@

arb_os/arbos-upgrade.mexe: arb_os/arbos-upgrade-base.mexe arb_os/bridge_arbos_versions.mini .make/tools
	$(compile) arb_os -o $@ -m

arb_os/bridge_arbos_versions.mini: arb_os/arbos-upgrade-base.mexe arb_os/arbos_before.mexe arb_os/customize_arbos_bridge_versions.mini arb_os/upgrade.toml .make/tools
	$(upgrade) arb_os/arbos_before.mexe $< $@ customize_arbos_bridge_versions arb_os/upgrade.toml

arb_os/arbos-upgrade-base.mexe: $(arbos_source_no_bridge) .make/tools
	cp arb_os/dummy_version_bridge.mini arb_os/bridge_arbos_versions.mini
	$(compile) arb_os -o $@ -m


# strategic rules to minimize dependency building

.make/tools: .make/solidity .make/compiler .make/install
	@touch .make/tools

.make/test: arb_os/arbos.mexe $(test_mexes) arb_os/arbos-upgrade.mexe arb_os/upgrade.json
	cargo test --release
	@touch .make/test

.make/fmt: src/*.rs src/*/*.rs Cargo.* .make/install $(mini_files)
	cargo fmt
	@touch .make/fmt

.make/push: .make/fmt
	make $(MAKEFLAGS) compile_options="$(compile_options)" replayTests .make/test
	@touch .make/push

.make/compiler: src/*.rs src/*/*.rs Cargo.* .make/install
	cargo build --release
	@touch .make/compiler

.make/solidity: contracts/arbos/*/*.sol .make/install
	yarn --cwd contracts build
	@touch .make/solidity

.make/install:
	mkdir -p .make
	yarn --cwd contracts install
	@touch .make/install


# CLI tooling
cov_files = $(wildcard coverage/*.cov)
cov_files_no_upgrade = $(filter-out coverage/test_upgrade_arbos_to_different_version.cov, $(cov_files))

lcov-mini.info: coverage/alltests.all ./coverage/mini-coverage.sh
	./coverage/mini-coverage.sh $< > $@

coverage/alltests.all: coverage/avmcodebuilder.partial $(cov_files_no_upgrade) .make/test
	cat $^ | sort -r | uniq | sort | uniq -f 1 | sort -k2,2 -k3,3n | grep -v test | grep -v Test > $@

coverage/avmcodebuilder.partial: coverage/test_upgrade_arbos_to_different_version.cov
	grep avmcodebuilder $< > $@

coverage/test_upgrade_arbos_to_different_version.cov: .make/test

# Makefile settings

always:              # use this to force other rules to always build
.DELETE_ON_ERROR:    # causes a failure to delete its target
