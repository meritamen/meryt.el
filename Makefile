DRONES_DIR = $(shell git config "borg.drones-directory" || echo "site-lisp")
EMDIR=/Applications/Emacs.app/Contents/MacOS
EMDUMPDIR=$(EMDIR)
EMCONFDIR=/Users/meritamen/.emacs.d
DUMP_EL=$(EMCONFDIR)/dump.el

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
		@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
		--url git@github.com:emacscollective/borg.git
		@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
		@cd $(DRONES_DIR)/borg; git reset --hard HEAD

redump:
		@$(EMDIR)/Emacs --batch --load $(DUMP_EL)
		@mv $(EMCONFDIR)/Emacs.pdmp $(EMDUMPDIR)/Emacs.pdmp
