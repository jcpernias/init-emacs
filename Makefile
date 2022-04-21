SHELL := /bin/sh

RM := /bin/rm
INSTALL := /usr/bin/install
emacsbin := /urs/bin/emacs
envbin := /usr/bin/env

-include local.mk

EMACS := $(emacsbin) -batch

rootdir := .
testdir := $(rootdir)/.emacs.d


all: $(rootdir)/init.el

$(rootdir)/init.el: $(rootdir)/init.org
	$(EMACS) --visit=$< --eval="(org-babel-tangle)"


.PHONY: test
test: $(rootdir)/init.el clean-test
	$(INSTALL) -m 0755 -d $(testdir)
	$(INSTALL) -m 0644 -T $< $(testdir)/init.el
	$(ENV) HOME=$(realpath $(rootdir)) $(emacsbin) &


.PHONY: clean-test
clean-test:
	-@$(RM) -rf $(testdir)




dotemacsdir := $(HOME)/.emacs.d
jcpinitdir := $(dotemacsdir)/jcp-init
snippetsdir := $(dotemacsdir)/snippets
orgsnippetsdir := $(snippetsdir)/org-mode
auctexdir := $(dotemacsdir)/auctex-styles

all_deps := \
	$(dotemacsdir)/init.el \
	$(jcpinitdir)/jcp-base.el \
	$(jcpinitdir)/jcp-darwin.el \
	$(jcpinitdir)/jcp-extensions.el \
	$(jcpinitdir)/jcp-functions.el \
	$(jcpinitdir)/jcp-global-keys.el \
	$(orgsnippetsdir)/equation \
	$(orgsnippetsdir)/gather \
	$(auctexdir)/elsarticle.el

$(dotemacsdir)/init.el: $(rootdir)/init.el | $(dotemacsdir)
	$(INSTALL) -m 0644 -T $< $@

$(jcpinitdir)/%.el: ./jcp-init/%.el | $(jcpinitdir)
	$(INSTALL) -m 0644 -T $< $@

$(orgsnippetsdir)/%: ./snippets/org-mode/% | $(orgsnippetsdir)
	$(INSTALL) -m 0644 -T $< $@

$(auctexdir)/%: ./auctex-styles/% | $(auctexdir)
	$(INSTALL) -m 0644 -T $< $@

$(dotemacsdir):
	$(INSTALL) -m 0755 -d $@

$(jcpinitdir):
	$(INSTALL) -m 0755 -d $@

$(orgsnippetsdir):
	$(INSTALL) -m 0755 -d $@

$(auctexdir):
	$(INSTALL) -m 0755 -d $@


# .PHONY: clean
# clean:
# 	-@$(RM) -rf $(dotemacsdir)
