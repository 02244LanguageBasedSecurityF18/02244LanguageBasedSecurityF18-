{-

AIF Omega version 2016

Developed under BSD license as part of OFMC

(C) Copyright Alessandro Bruni 2015,2016
(C) Copyright DTU 2010,2016
(C) Copyright Paolo Modesti 2010
(C) Copyright IBM Corp. 2010

All Rights Reserved.

-}

module Main where
import FPASLan
import FPASLanLexer
import FPASLanParser
import AIFOm
import Types
import Modesti
import PrettyPrinter
import System.Environment
import System.Exit
import System.Process
import System.IO
import Data.List
import Control.Monad

--data Input = AIF | AIFOm -- | SetPi
--           deriving (Eq, Show)
data Run = Run { fileName :: String,
                 --input :: Input,
                 output :: Output,
                 prove :: Bool }
           deriving (Eq, Show)
defaultOpts = Run { fileName = "", --input = AIF,
                    output = Proverif,
                    prove = False }

parseArgs = foldl (\ opts x ->
  case x of
    "-spass" -> opts { output = SPASS }
    "-proverif" -> opts { output = Proverif }
    --"-AIFOm" -> opts { input = AIFOm }
    --"-AIF" -> opts { input = AIF }
    -- "-SetPi" -> opts { input = SetPi }
    "-prove" -> opts { prove = True }
    ('-':_) -> error $ "Parameter " ++ x ++ " not recognised\n\n" ++ usage
    _ -> opts { fileName = x }
                --input =
                --  if ".aifom" `isSuffixOf` x then AIFOm
                --  else if ".aif" `isSuffixOf` x then AIF
                      --  else if ".sp" `isSuffixOf` x then SetPi
                --       else input opts }
  )

usage = "Usage: aifomega [-spass] filename\n"++
  "     -proverif  (default) generate Horn clauses in ProVerif syntax\n"++
  "     -spass     generate Horn clauses in SPASS syntax\n"
-- \    -prove     run the theorem prover"
-- \    -AIFOm     force parsing of AIF-Omega file format\n\
-- \               (default for files ending in .aifom)\n\
-- \    -AIF       force parsing of AIF file format\n\
-- \               (default for files ending in .aif)\n\
-- \    -SetPi     force parsing of SetPi file format\n\
-- \               (default for files ending in .sp)\n\

main =
  do args <- getArgs
     let opts = parseArgs defaultOpts args
     let prettyPrinter =
           case output opts of
             SPASS -> pASLan
             Proverif -> pvASLan
             _ -> error "Sorry, this output format is currently not supported."
     when (fileName opts == "") (die usage)
     file <- readFile (fileName opts)
     let paslan = parser . alexScanTokens $ file
     {-
     case input opts of
       AIF -> do
         putStr "Parsing as AIF (compatibility)\n"
         let (ppaslan@(PASLan name types sets pubfun privfun facts definitions rules)) = mkPrepros paslan
         -- <paolo>--
         if debug then error . dbgDefs . parser . alexScanTokens $ file else putStr ""
         -- </paolo>--
         let symbtab = mkSymbtab ppaslan
         -- putStr $ Map.showTree symbtab
         putStr $ if spassout then "% Checking rules: "++show (checkrules symbtab rules)++"\n" else "(* Checking rules: "++show (checkrules symbtab rules)++"*)\n"
         (rules',numsets) <- return $ absRules symbtab (quantelims symbtab rules)
         putStr (prettyPrinter symbtab numsets name rules')
       AIFOm -> do -}
     --- when (output opts /= Proverif) $ error "Sorry, in this version, the new extensions of AIFOm over AIF are only implemented for ProVerif as prover."
     -- TODO: call normalize to accept AIF's syntactic sugar
     -- TODO: rule checks
     let p = normalize paslan
     let hornclausesAS = compile p
     let hornclausesCS = prettyPrinter hornclausesAS
     if prove opts
       then do
       (tmpPath, h) <- openTempFile "." (fileName opts)
       hPutStr h hornclausesCS
       hClose h
       rawSystem "proverif" [tmpPath]
       rawSystem "rm" [tmpPath]
       return ()
       else
       putStr hornclausesCS
      --  SetPi -> do
      --    if (output opts /= Proverif)
      --      then error "Only supporting ProVerif output with AIFOm input"
      --      else return ()
      --    error "Not implemented yet"
