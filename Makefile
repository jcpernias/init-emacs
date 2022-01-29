SHELL := /bin/sh

INSTALL := /usr/bin/install

-include local.mk

dotemacsdir := $(HOME)/.emacs.d
jcpinitdir := $(dotemacsdir)/jcp-init
snippetsdir := $(dotemacsdir)/snippets
orgsnippetsdir := $(snippetsdir)/org-mode

all_deps := \
	$(dotemacsdir)/init.el \
	$(jcpinitdir)/jcp-base.el \
	$(jcpinitdir)/jcp-darwin.el \
	$(jcpinitdir)/jcp-extensions.el \
	$(jcpinitdir)/jcp-functions.el \
	$(jcpinitdir)/jcp-global-keys.el \
	$(orgsnippetsdir)/equation \
	$(orgsnippetsdir)/gather

all: $(all_deps)

$(dotemacsdir)/init.el: ./init.el | $(dotemacsdir)
	$(INSTALL) -m 0644 -T $< $@

$(jcpinitdir)/%.el: ./jcp-init/%.el | $(jcpinitdir)
	$(INSTALL) -m 0644 -T $< $@

$(orgsnippetsdir)/%: ./snippets/org-mode/% | $(orgsnippetsdir)
	$(INSTALL) -m 0644 -T $< $@

$(dotemacsdir):
	$(INSTALL) -m 0755 -d $@

$(jcpinitdir):
	$(INSTALL) -m 0755 -d $@

$(orgsnippetsdir):
	$(INSTALL) -m 0755 -d $@

clean:
	-@$(RM) -rf $(dotemacsdir)
