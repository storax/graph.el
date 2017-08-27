.PHONY : test

EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .

ELPA_DIR = \
.cask/$(shell $(EMACS) -Q --batch --eval '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')/elpa

test: elpa
	$(CASK) exec ert-runner --reporter ert

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
