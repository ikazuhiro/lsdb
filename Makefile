#
# Makefile for LSDB.
#

PACKAGE = lsdb
API	= 0.1
RELEASE = 0

RM	= /bin/rm -f

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l LSDB-MK

PREFIX	= NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc

VERSION	= $(API).$(RELEASE)

elc:
	$(EMACS) $(FLAGS) -f compile-lsdb $(PREFIX) $(LISPDIR)

install: elc
	$(EMACS) $(FLAGS) -f install-lsdb $(PREFIX) $(LISPDIR)

package:
	$(XEMACS) $(FLAGS) -f compile-lsdb-package $(PACKAGEDIR)

install-package:	package
	$(XEMACS) $(FLAGS) -f install-lsdb-package $(PACKAGEDIR)

ChangeLog: *.el README Makefile
	cvs2cl --prune --no-wrap --usermap ./cvs2cl/usermap --gmt --stdout | \
	ruby -p -i ./cvs2cl/fmtlog.rb > ChangeLog

.PHONY: clean
clean:
	-$(RM) $(GOMI)
