EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	$(CASK) exec ert-runner

compile:
	$(CASK) build

clean-elc:
	$(CASK) clean-elc

.PHONY: all test compile clean-elc
