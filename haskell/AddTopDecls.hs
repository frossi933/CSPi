{-# LANGUAGE TemplateHaskell #-}

module AddTopDecls where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

predNames :: [(String, String)]
predNames = unsafePerformIO (do f <- readFile "pred.aux"
                                return $ (map (\l -> let (l1,l2)=span (/=' ') l in (l1,drop 1 l2)) (lines f)))

actNames :: [(String, String)]
actNames = unsafePerformIO (do f <- readFile "act.aux"
                               return $ (map (\l -> let (l1,l2)=span (/=' ') l in (l1,drop 1 l2)) (lines f)))

funNames :: [(String, String)]
funNames = unsafePerformIO (do f <- readFile "fun.aux"
                               return $ (map (\l -> let (l1,l2)=span (/=' ') l in (l1,drop 1 l2)) (lines f)))
