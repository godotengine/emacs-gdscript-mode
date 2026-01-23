SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

.PHONY: clean checkdoc lint package install compile test

# TODO: add `lint` if we can?
ci: clean install-deps package install compile checkdoc

package:
	@echo "Packaging..."
	$(EASK) package

install-deps:
	@echo "Installing dependencies..."
	$(EASK) install-deps --dev

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

test:
	@echo "Testing..."
	$(EASK) ert ./test/*.el

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) lint checkdoc

lint:
	@echo "Run package-lint..."
	$(EASK) lint package

clean:
	$(EASK) clean all
