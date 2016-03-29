# =============================================================================
# Variables
# =============================================================================

PREFIX ?= "/usr"
BIN := "$(PREFIX)/bin"

# =============================================================================
# Rules
# =============================================================================
.PHONY= all test clean repl shell build test analyze configure install

all: test

clean:
	rm -rf rebar.lock
	rm -rf _build
	rm -rf .cache

repl:
	nix-shell --run "erl"

shell:
	nix-shell --run "bash"

configure:
	nix-shell --pure --command 'eval "$$configurePhase"; echo $?'

build: configure
	nix-shell --pure --command 'eval "$$buildPhase"'

analyze: bootstrap
	nix-shell --pure --run "TERM=dumb HOME=$(CURDIR) rebar3 do escriptize,dialyzer"

test: build
	nix-shell --pure --run "TERM=dumb HOME=$(CURDIR) rebar3 do escriptize,dialyzer,eunit"

install:
	rebar3-nix-bootstrap
	HOME=$(CURDIR) rebar3 escriptize
	mkdir -p "$(BIN)"
	cp "$(CURDIR)/_build/default/bin/hex2nix" "$(BIN)"
	chmod a+x "$(BIN)/hex2nix"
