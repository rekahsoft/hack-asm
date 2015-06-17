-- (C) Copyright Collin J. Doering 2015
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- File: HackAsm.hs
-- Author: Collin J. Doering <collin.doering@rekahsoft.ca>
-- Date: Jun 17, 2015

{-|
Module      : RekahSoft.Asmblr.Parser
Description : Parse hack assembly into its machine language representation
Copyright   : (c) Collin J. Doering, 2015
License     : GPL-3
Maintainer  : collin.doering@rekahsoft.ca
Stability   : stable
Portability : POSIX

TODO: describe the assemblers operation in more detail
-}
module RekahSoft.HackAsm (parseHackAsm, parseHackAsmFile) where

import RekahSoft.HackAsm.Parser
