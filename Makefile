# The plutus-pab commands, contracts and hoogle environment
# are made availible by the nix shell defined in shell.nix.
# In most cases you should execute Make after entering nix-shell.

.PHONY: hoogle build test watch ghci readme_contents \
	format lint refactor requires_nix_shell haddock

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available options:"
	@echo "  FLAGS   -- Additional options passed to --ghc-options"
	@echo
	@echo "Available commands:"
	@echo "  hoogle              -- Start local hoogle"
	@echo "  build               -- Run cabal v2-build"
	@echo "  watch               -- Track files and run 'make build' on change"
	@echo "  test                -- Run cabal v2-test"
	@echo "  costing             -- Run cost-estimation benchmark"
	@echo "  coverage            -- Generate a coverage report of the tests"
	@echo "  ghci                -- Run stack ghci"
	@echo "  format              -- Apply source code formatting with fourmolu"
	@echo "  format_check        -- Check source code formatting without making changes"
	@echo "  cabalfmt            -- Apply cabal formatting with cabal-fmt"
	@echo "  cabalfmt_check      -- Check cabal files for formatting errors without making changes"
	@echo "  nixpkgsfmt          -- Apply nix formatting with nixfmt"
	@echo "  nixpkgsfmt_check    -- Check nix files for format errors"
	@echo "  lint                -- Check the sources with hlint"
	@echo "  refactor            -- Automatically apply hlint refactors, with prompt"
	@echo "  readme_contents     -- Add table of contents to README"
	@echo "  update_plutus       -- Update plutus version with niv"

hoogle: requires_nix_shell
	pkill hoogle || true
	hoogle generate --local=.haddock --database=.hoogle/local.hoo
	hoogle server --local -p 8080 >> /dev/null &
	hoogle server --local --database=.hoogle/local.hoo -p 8081 >> /dev/null &

STACK_EXE_PATH = $(shell stack $(STACK_FLAGS) path --local-install-root)/bin

ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif

build: requires_nix_shell apropos.cabal
	cabal v2-build $(GHC_FLAGS)

watch: requires_nix_shell apropos.cabal
	while sleep 1; do find plutus-extra.cabal src test | entr -cd make build; done

test: requires_nix_shell apropos.cabal
	cabal v2-test

ghci: requires_nix_shell apropos.cabal
	cabal v2-repl $(GHC_FLAGS)

coverage: apropos.cabal
	nix-build --arg doCoverage true -A projectCoverageReport

# Source dirs to run fourmolu on
FORMAT_SOURCES := $(shell find -name '*.hs' -not -path './dist-*/*')

# Extensions we need to tell fourmolu about
FORMAT_EXTENSIONS := -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -XBangPatterns -o -fplugin=RecordDotPreprocessor

# Run fourmolu formatter
format: requires_nix_shell
	fourmolu --mode inplace --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Check formatting (without making changes)
format_check: requires_nix_shell
	fourmolu --mode check --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

CABAL_SOURCES := $(shell fd -e cabal)

cabalfmt: requires_nix_shell
	cabal-fmt --inplace $(CABAL_SOURCES)

cabalfmt_check: requires_nix_shell
	cabal-fmt --check $(CABAL_SOURCES)

# Nix files to format
NIX_SOURCES := $(shell fd -e nix)

nixpkgsfmt: requires_nix_shell
	nixpkgs-fmt $(NIX_SOURCES)

nixpkgsfmt_check: requires_nix_shell
	nixpkgs-fmt --check $(NIX_SOURCES)

# Check with hlint, currently I couldn't get --refactor to work
lint: requires_nix_shell
	hlint $(FORMAT_SOURCES)

# Apply automatic hlint refactors, with prompt
refactor: requires_nix_shell
	for src in $(FORMAT_SOURCES) ; do hlint --refactor --refactor-options='-i -s' $$src ; done

readme_contents:
	echo "this command is not nix-ified, you may receive an error from npx"
	npx markdown-toc ./README.md --no-firsth1

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ -v IN_NIX_SHELL ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ -v IN_NIX_SHELL ] || (echo "    run 'nix-shell --pure' first" && false)


PLUTUS_BRANCH = $(shell jq '.plutus.branch' ./nix/sources.json )
PLUTUS_REPO = $(shell jq '.plutus.owner + "/" + .plutus.repo' ./nix/sources.json )
PLUTUS_REV = $(shell jq '.plutus.rev' ./nix/sources.json )
PLUTUS_SHA256 = $(shell jq '.plutus.sha256' ./nix/sources.json )

update_plutus:
	@echo "Updating plutus version to latest commit at $(PLUTUS_REPO) $(PLUTUS_BRANCH)"
	niv update plutus
	@echo "Latest commit: $(PLUTUS_REV)"
	@echo "Sha256: $(PLUTUS_SHA256)"
	@echo "Make sure to update the plutus rev in cabal.project with:"
	@echo "    commit: $(PLUTUS_REV)"
	@echo "This may require further resolution of dependency versions."

haddock: requires_nix_shell
	cabal haddock --haddock-html --haddock-hoogle --builddir=.haddock

format_apply_all:
	make format cabalfmt nixpkgsfmt lint refactor
