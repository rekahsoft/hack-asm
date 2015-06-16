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

-- File: Parser.hs
-- Author: Collin J. Doering <collin.doering@rekahsoft.ca>
-- Date: Jun 16, 2015

{-|
Module      : Asmblr.Parser
Description : Parse hack assembly into its machine language representation
Copyright   : (c) Collin J. Doering, 2015
License     : GPL-3
Maintainer  : collin.doering@rekahsoft.ca
Stability   : stable
Portability : POSIX

TODO: describe the assemblers operation in more detail
-}
module Asmblr.Parser (parseHackAsm, parseHackAsmFile) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (modifyState)
import Text.Parsec.Char (endOfLine)

import System.IO
import Control.Monad (liftM)
import Control.Exception hiding (try)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import qualified Data.Map as Map

----------------------------------------------------------------------------

-- | TODO: Documentation
type Label = String

-- | TODO: Documentation
type SymbolTable = Map.Map Label Int

-- | TODO: Documentation
data Instruction = AInstr Int
                 | CInstr String String String
                 deriving (Show, Read, Eq)

----------------------------------------------------------------------------

-- | TODO: Documentation
aInstrAddr :: GenParser Char st Instruction
aInstrAddr = AInstr . read <$> many1 digit

-- | TODO: Documentation
aInstrSym :: GenParser Char (Int, SymbolTable) Instruction
aInstrSym = do
  var <- symbol

  (curMem, symTbl) <- getState
  case Map.lookup var symTbl of
   Nothing -> do
     setState (curMem + 1, Map.insert var curMem symTbl)
     return $ AInstr curMem
   Just varCurMem -> return $ AInstr varCurMem

-- | TODO: Documentation
aInstr :: GenParser Char (Int, SymbolTable) Instruction
aInstr = do
  char '@'
  aInstrAddr <|> aInstrSym

-- | TODO: Documentation
cInstrDest :: GenParser Char st (Label, String)
cInstrDest = choice [ try (string "AMD") >> return ("AMD", "111")
                    , try (string "AD")  >> return ("AD",  "110")
                    , try (string "AM")  >> return ("AM",  "101")
                    , try (string "MD")  >> return ("MD",  "011")
                    , char 'A'           >> return ("A",   "100")
                    , char 'D'           >> return ("D",   "010")
                    , char 'M'           >> return ("M",   "001") ]

-- | TODO: Documentation
cInstrJump :: GenParser Char st (Label, String)
cInstrJump = char 'J' >>
               choice [      string "MP"  >> return ("JMP", "111")
                      , try (string "LE") >> return ("JLE", "110")
                      ,      string "LT"  >> return ("JLT", "100")
                      ,      string "NE"  >> return ("JNE", "101")
                      ,      string "EQ"  >> return ("JEQ", "010")
                      , try (string "GE") >> return ("JGE", "011")
                      ,      string "GT"  >> return ("JGT", "001") ]

-- | TODO: Documentation
cInstrAluOps :: GenParser Char st (Label, String)
cInstrAluOps = choice [      char   '0'    >> return ("0",   "0101010")
                      ,      char   '1'    >> return ("1",   "0111111")
                      
                      , try (string "-1")  >> return ("-1",  "0111010")
                      , try (string "-D")  >> return ("-D",  "0001111")
                      , try (string "-A")  >> return ("-A",  "0110011")
                      ,      string "-M"   >> return ("-M",  "1110011")

                      , try (string "D+1") >> return ("D+1", "0011111")
                      , try (string "D-1") >> return ("D-1", "0001110")
                      , try (string "D+A") >> return ("D+A", "0000010")
                      , try (string "D-A") >> return ("D-A", "0010011")
                      , try (string "D&A") >> return ("D&A", "0000000")
                      , try (string "D|A") >> return ("D|A", "0010101")
                      , try (string "D+M") >> return ("D+M", "1000010")
                      , try (string "D-M") >> return ("D-M", "1010011")
                      , try (string "D&M") >> return ("D&M", "1000000")
                      , try (string "D|M") >> return ("D|M", "1010101")
                      ,      char   'D'    >> return ("D",   "0001100")
                      
                      , try (string "!D")  >> return ("!D",  "0001101")
                      , try (string "!A")  >> return ("!A",  "0110001")
                      ,      string "!M"   >> return ("!M",  "1110001")

                      , try (string "A+1") >> return ("A+1", "0110111")
                      , try (string "A-1") >> return ("A-1", "0110010")
                      , try (string "A-D") >> return ("A-D", "0000111")
                      ,      char   'A'    >> return ("A",   "0110000")

                      , try (string "M+1") >> return ("M+1", "1110111")
                      , try (string "M-1") >> return ("M-1", "1110010")
                      , try (string "M-D") >> return ("M-D", "1000111")
                      ,      char   'M'    >> return ("M",   "1110000") ]

-- | TODO: Documentation
cInstrNoJump :: GenParser Char st Instruction
cInstrNoJump = do
  (_, dest) <- cInstrDest
  char '='
  (_, aluOp) <- cInstrAluOps
  
  return $ CInstr aluOp dest "000"

-- | TODO: Documentation
cInstrNoDest :: GenParser Char st Instruction
cInstrNoDest = do
  (_, aluOp) <- cInstrAluOps
  char ';'
  (_, jump) <- cInstrJump

  return $ CInstr aluOp "000" jump

-- | TODO: Documentation
cInstrDestJump :: GenParser Char st Instruction
cInstrDestJump = do
  (_, dest) <- cInstrDest
  char '='
  (_, aluOp) <- cInstrAluOps
  char ';'
  (_, jump) <- cInstrJump

  return $ CInstr aluOp dest jump

-- | TODO: Documentation
cInstr :: GenParser Char st Instruction
cInstr = try cInstrDestJump <|> try cInstrNoDest <|> cInstrNoJump

-- | TODO: Documentation
instr :: GenParser Char (Int, SymbolTable) Instruction
instr = aInstr <|> cInstr

-- | TODO: Documentation
symbol :: GenParser Char st String
symbol = many1 (alphaNum <|> oneOf "_.$:")

-- | TODO: Documentation
comment :: GenParser Char st String
comment = do
  string "//" >> manyTill anyChar (lookAhead endOfLine)
  return ""

-- | TODO: Documentation
labelLine :: GenParser Char (a, Map.Map Label a) String
labelLine= do
  lbl <- between (char '(') (char ')') symbol
  optional lineSpaces
  optional comment

  (lineNum, symTbl) <- getState
  case Map.lookup lbl symTbl of
   Nothing -> setState (lineNum, Map.insert lbl lineNum symTbl)
   Just _  -> error $ "Aleady used label \"" ++ lbl ++ "\""
  return ""

-- | TODO: Documentation
instrLine :: GenParser Char (Int, a) String
instrLine = do
  ret <- aInstr' <|> cInstr'
  optional lineSpaces
  optional comment

  modifyState $ \(l, tbl) -> (l + 1, tbl)
  
  return ret
    where aInstr' = do
            char '@'
            str <- many1 digit <|> symbol
            return $ "@" ++ str

          cInstr' = do
            dest <- optionMaybe $ try $ do
              (d, _) <- cInstrDest
              char '='
              return d
            (op, _)   <- cInstrAluOps
            jump <- optionMaybe $ try $ do
              char ';'
              (j, _) <- cInstrJump
              return j

            case (dest, jump) of
             (Nothing, Nothing) -> error "Must specify either dest or jump"
             (Just dest', Nothing) -> return $ dest' ++ "=" ++ op
             (Nothing, Just jump') -> return $ op ++ ";" ++ jump'
             (Just dest', Just jump') -> return $ dest' ++ "=" ++ op ++ ";" ++ jump'

-- | TODO: Documentation
emptyLine :: GenParser Char st String
emptyLine = manyTill space (lookAhead endOfLine)

-- | TODO: Documentation
lineSpaces :: GenParser Char st String
lineSpaces = many $ oneOf " \t"

-- | TODO: Documentation
firstPass :: GenParser Char (Int, SymbolTable) (String, SymbolTable)
firstPass = do
  str <- liftM (unlines . filter (not . null)) $ (flip sepEndBy) endOfLine $ do
          optional lineSpaces
          comment <|> emptyLine <|> labelLine <|> instrLine
  eof
  (_, symTbl) <- getState
  return (str, symTbl)

-- | TODO: Documentation
secondPass :: GenParser Char (Int, SymbolTable) [Instruction]
secondPass = sepEndBy instr endOfLine

-- | @parseHackAsm s@ parses @s@ and returns its hack machine language equivalent
parseHackAsm :: Monad m => String -> m String
parseHackAsm str = case runParser firstPass (0, varSymbols) "" str of
  Left  err            -> throw . userError $ show err
  Right (str', symTbl) -> case runParser secondPass (16, symTbl) "" str' of
                           Left  err -> throw . userError $ show err
                           Right out -> return $ genHackML out
  where varSymbols = Map.fromList [ ("R0", 0), ("SP", 0)
                                  , ("R1", 1), ("LCL", 1)
                                  , ("R2", 2), ("ARG", 2)
                                  , ("R3", 3), ("THIS", 3)
                                  , ("R4", 4), ("THAT", 4)
                                  , ("R5", 5)
                                  , ("R6", 6)
                                  , ("R7", 7)
                                  , ("R8", 8)
                                  , ("R9", 9)
                                  , ("R10", 10)
                                  , ("R11", 11)
                                  , ("R12", 12)
                                  , ("R13", 13)
                                  , ("R14", 14)
                                  , ("R15", 15)
                                  , ("SCREEN", 16384)
                                  , ("KBD", 24576) ]

-- | The expression @parseHackAsmFile f@ will parse the file given by @f@
--   and output the resulting hack machine language to stdout
parseHackAsmFile :: FilePath -> IO ()
parseHackAsmFile f = withFile f ReadMode $ \h -> do
  hGetContents h >>= parseHackAsm >>= putStr

-- | Given a list of @Instructions@ returns its hack machine language equivalent
genHackML :: [Instruction] -> String
genHackML xs = unlines $ map instrToML xs
  where instrToML (AInstr n)            = leftPad 16 '0' $ showIntAtBase 2 intToDigit n ""
        instrToML (CInstr op dest jump) = "111" ++ op ++ dest ++ jump
        leftPad n a ys = replicate (n - (length ys)) a ++ ys
