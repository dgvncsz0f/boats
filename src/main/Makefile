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

SRCDIR=$(CURDIR)

OBJSRC:=$(wildcard $(SRCDIR)/haskell/Battleship/*.hs) \
	$(wildcard $(SRCDIR)/haskell/Battleship/UI/*.hs) \
	$(wildcard $(SRCDIR)/haskell/Battleship/Game/*.hs)
PRGSRC=$(SRCDIR)/haskell/battleship.hs

OBJ=$(patsubst %.hs,%.o,$(OBJSRC))
PRG=$(basename $(PRGSRC))

$(PRG): $(OBJSRC)

.PHONY: compile
compile: $(PRG)

.PHONY: clean
clean:
	-$(RM) $(patsubst %.hs,%.hi,$(OBJSRC)) $(OBJ)
	-$(RM) $(patsubst %.hs,%.hi,$(PRGSRC)) $(patsubst %.hs,%.o,$(PRGSRC)) $(PRG)

.SUFFIXES: .hs
%.o: %.hs
	$(HC) $(HCFLAGS) -i$(SRCDIR)/haskell/: -c "$<"

%: %.hs
	$(HC) $(HCFLAGS) -i$(SRCDIR)/haskell/: --make "$<"
