###.PHONY: dhdl

all: dhdl

ctags:
	cd ${HYPER_HOME} && sbt "; project forge; gen-ctags"

dhdl:
	bin/init-dsl.sh

clean:
	bin/clean-dsl.sh
