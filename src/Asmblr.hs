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

-- File: Asmblr.hs
-- Author: Collin J. Doering <collin.doering@rekahsoft.ca>
-- Date: Jun  5, 2015

----------------------------------------------------------------------------

{-|
Module      : Asmblr
Description : Program that takes a hack assembly and converts it to its
              machine language representation
Copyright   : (c) Collin J. Doering, 2015
License     : GPL-3
Maintainer  : collin.doering@rekahsoft.ca
Stability   : stable
Portability : POSIX

TODO: describe the assemblers operation in more detail
-}
module Asmblr (defaultMain) where

import System.IO
import System.FilePath (dropExtension)
import System.Console.GetOpt
import System.Environment (getArgs)

import Asmblr.Parser

----------------------------------------------------------------------------

-- | TODO: Documentation
data Flag = Verbose
          | Version
          | Help
          | Output (Maybe String)
          deriving (Eq, Show)

-- | TODO: Documentation
options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg Verbose) "chatty output on stderr"
          , Option ['V'] ["version"] (NoArg Version) "show version number"
          , Option ['h'] ["help"]    (NoArg Help)    "show program usage"
          , Option ['o'] ["output"]  (OptArg Output "FILE") "output file or '-' for stdout" ]

-- | TODO: Documentation
isVersion :: Flag -> Bool
isVersion Version = True
isVersion _       = False

-- | TODO: Documentation
isHelp :: Flag -> Bool
isHelp    Help    = True
isHelp    _       = False

-- | TODO: Documentation
hasOutput :: [Flag] -> Maybe String
hasOutput []                    = Nothing
hasOutput ((Output (Just x)):_) = Just x
hasOutput (_:xs)                = hasOutput xs

-- | TODO: Documentation
progVersion :: String
progVersion = unlines
  [ "Assmblr 0.1.0.0"
  , "Copyright (C) 2014 RekahSoft, Ltd."
  , "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>."
  , "This is free software: you are free to change and redistribute it."
  , "There is NO WARRANTY, to the extent permitted by law."
  , ""
  , "Written by Collin J. Doering." ]

-- | TODO: Documentation
defaultMain :: IO ()
defaultMain = do
  argv <- getArgs

  case getOpt Permute options argv of
   (o, _, []) | any isVersion o -> putStr progVersion
   (o, _, []) | any isHelp    o -> putStr $ usageInfo header options
   (o, [i], []) -> do
     --curDir <- getWorkingDirectory
     inFile <- if i == "-"
                 then return stdin
                 else openFile i ReadMode
     outFile <- case hasOutput o of
                 Nothing -> if i /= "-"
                              then openFile (dropExtension i ++ ".hack") WriteMode
                              else return stdout
                 Just x  | x == "-" -> return stdout
                 Just x -> openFile x WriteMode

     hGetContents inFile >>= parseHackAsm >>= hPutStr outFile
     hClose inFile
     hClose outFile
   (o, xs, []) -> ioError (userError $ "Gave more then one input file.\n" ++ usageInfo header options)
   (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: Asmblr [OPTION...] file"
