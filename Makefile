# Makefile for codepad.el
#
# Copyright (C) 2010 by Rüdiger Sonderfeld <ruediger@c-plusplus.de>
#
# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2, or (at your option) any later
# version.
#
# This is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING. If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.
#

EMACS    := emacs
PREFIX   := /usr/local
ELISPDIR := $(PREFIX)/share/emacs/site-lisp
INSTALL  := install

.PHONY: all install uninstall clean
all: codepad.elc

codepad.elc: codepad.el
	$(EMACS) -batch -q -f batch-byte-compile codepad.el

install: codepad.elc
	$(INSTALL) codepad.elc codepad.el $(ELISPDIR)

uninstall:
	rm -f $(ELISPDIR)/codepad.elc $(ELISPDIR)/codepad.el

clean:
	rm -f codepad.elc
