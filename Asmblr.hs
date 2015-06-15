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

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (modifyState)
import Text.Parsec.Char (endOfLine)

import System.IO
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)

import Control.Monad (liftM)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import qualified Data.Map as Map

----------------------------------------------------------------------------

type SymbolTable = Map.Map String Int

type Label = String

data Instruction = AInstr Int
                 | CInstr String String String
                 deriving (Show, Read, Eq)

----------------------------------------------------------------------------

aInstrAddr :: GenParser Char st Instruction
aInstrAddr = AInstr . read <$> many1 digit

aInstrSym :: GenParser Char (Int, SymbolTable) Instruction
aInstrSym = do
  var <- symbol

  (curMem, symTbl) <- getState
  case Map.lookup var symTbl of
   Nothing -> do
     setState (curMem + 1, Map.insert var curMem symTbl)
     return $ AInstr curMem
   Just varCurMem -> return $ AInstr varCurMem

aInstr :: GenParser Char (Int, SymbolTable) Instruction
aInstr = do
  char '@'
  aInstrAddr <|> aInstrSym

cInstrDest :: GenParser Char st (String, String)
cInstrDest = choice [ try (string "AMD") >> return ("AMD", "111")
                    , try (string "AD")  >> return ("AD",  "110")
                    , try (string "AM")  >> return ("AM",  "101")
                    , try (string "MD")  >> return ("MD",  "011")
                    , char 'A'           >> return ("A",   "100")
                    , char 'D'           >> return ("D",   "010")
                    , char 'M'           >> return ("M",   "001") ]

cInstrJump :: GenParser Char st (String, String)
cInstrJump = char 'J' >>
               choice [      string "MP"  >> return ("JMP", "111")
                      , try (string "LE") >> return ("JLE", "110")
                      ,      string "LT"  >> return ("JLT", "100")
                      ,      string "NE"  >> return ("JNE", "101")
                      ,      string "EQ"  >> return ("JEQ", "010")
                      , try (string "GE") >> return ("JGE", "011")
                      ,      string "GT"  >> return ("JGT", "001") ]

cInstrAluOps :: GenParser Char st (String, String)
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

cInstrNoJump :: GenParser Char st Instruction
cInstrNoJump = do
  (_, dest) <- cInstrDest
  char '='
  (_, aluOp) <- cInstrAluOps
  
  return $ CInstr aluOp dest "000"

cInstrNoDest :: GenParser Char st Instruction
cInstrNoDest = do
  (_, aluOp) <- cInstrAluOps
  char ';'
  (_, jump) <- cInstrJump

  return $ CInstr aluOp "000" jump

cInstrDestJump :: GenParser Char st Instruction
cInstrDestJump = do
  (_, dest) <- cInstrDest
  char '='
  (_, aluOp) <- cInstrAluOps
  char ';'
  (_, jump) <- cInstrJump

  return $ CInstr aluOp dest jump

cInstr :: GenParser Char st Instruction
cInstr = try cInstrDestJump <|> try cInstrNoDest <|> cInstrNoJump

instr :: GenParser Char (Int, SymbolTable) Instruction
instr = aInstr <|> cInstr

symbol :: GenParser Char st String
symbol = many1 (alphaNum <|> oneOf "_.$:")

comment :: GenParser Char st String
comment = do
  optional $ many $ oneOf " \t"
  string "//" >> manyTill anyChar (lookAhead endOfLine)
  return ""

labelLine :: GenParser Char (a, Map.Map String a) String
labelLine= do
  optional $ many $ oneOf " \t"
  lbl <- between (char '(') (char ')') symbol
  optional comment

  (lineNum, symTbl) <- getState
  case Map.lookup lbl symTbl of
   Nothing -> setState (lineNum, Map.insert lbl lineNum symTbl)
   Just _  -> error $ "Aleady used label \"" ++ lbl ++ "\""
  return ""

instrLine :: GenParser Char (Int, a) String
instrLine = do
  optional $ many $ oneOf " \t"
  ret <- aInstr' <|> cInstr'
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

emptyLine :: GenParser Char st String
emptyLine = manyTill space (lookAhead endOfLine)

firstPass :: GenParser Char (Int, SymbolTable) (String, SymbolTable)
firstPass = do
  str <- liftM (unlines . filter (not . null)) $
           sepEndBy (comment <|> emptyLine <|> labelLine <|> instrLine) endOfLine
  (_, symTbl) <- getState
  return (str, symTbl)

secondPass :: GenParser Char (Int, SymbolTable) [Instruction]
secondPass = sepEndBy instr endOfLine

parseHackAsm :: Monad m => String -> m String
parseHackAsm str = case runParser firstPass (0, varSymbols) "" str of
  Left  err            -> return $ show err
  Right (str', symTbl) -> case runParser secondPass (16, symTbl) "" str' of
                           Left  err -> return $ show err
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

genHackML :: [Instruction] -> String
genHackML xs = unlines $ map instrToML xs
  where instrToML (AInstr n)            = leftPad 16 '0' $ showIntAtBase 2 intToDigit n ""
        instrToML (CInstr op dest jump) = "111" ++ op ++ dest ++ jump
        leftPad n a ys = replicate (n - (length ys)) a ++ ys

parseHackAsmFile :: FilePath -> IO String
parseHackAsmFile f = withFile f ReadMode $ \h -> do
  hGetContents h >>= parseHackAsm

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
      outFilename = (reverse $ drop 4 (reverse filename)) ++ ".hack"
  parseHackAsmFile filename >>= writeFile outFilename
