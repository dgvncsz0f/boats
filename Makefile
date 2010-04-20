# Copyright (C) 2006-2007 Diego Souza <paravinicius@yahoo.com.br>
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

export BUILD_DIR = $(CURDIR)/build
export MKDIR     = /bin/mkdir -p
export AWK       = /usr/bin/awk
export GREP      = /bin/grep
export HEAD      = /usr/bin/head
export TAR       = /bin/tar
export CD        = cd
export CP        = /bin/cp
export LN        = /bin/ln -sf
export FIND      = /usr/bin/find
export HC        = /home/dsouza/usr/bin/ghc
export HCFLAGS   = -Wall -W

VERSION = $(shell $(GREP) "Version" CHANGELOG | $(AWK) {'print $$2; exit'})

.PHONY: compile
compile: _prepare
	$(MAKE) -C src/main compile

.PHONY: build
build: _prepare compile

.PHONY: test
test: _prepare

.PHONY: run
run: HCFLAGS+=-O2
run: build
	-(cd ./src/main; ./haskell/battleship)

.PHONY: clean
clean:
	$(MAKE) -C src/main $@
	#$(MAKE) -C src/test $@
	-$(RM) -r $(BUILD_DIR)

.PHONY: _prepare
_prepare:
	@$(MKDIR) $(BUILD_DIR)
