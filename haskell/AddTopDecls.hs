{-# LANGUAGE TemplateHaskell #-}

module AddTopDecls where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax


importPred :: String -> ExpQ
importPred pname = do
    n <- newName pname
    d <- forImpD CCall unsafe pname n [t|IO Bool|]
    addTopDecls [d]
    [|$(varE n)|]

importAct :: String -> ExpQ
importAct aname = do
    n <- newName aname
    d <- forImpD CCall unsafe aname n [t|IO ()|]
    addTopDecls [d]
    [|$(varE n)|]
{-
8.2.4.1. Foreign imports and multi-threading

When you call a foreign imported function that is annotated as safe (the default), and the program was linked using -threaded, then the call will run concurrently with other running Haskell threads. If the program was linked without -threaded, then the other Haskell threads will be blocked until the call returns.

This means that if you need to make a foreign call to a function that takes a long time or blocks indefinitely, then you should mark it safe and use -threaded. Some library functions make such calls internally; their documentation should indicate when this is the case.

If you are making foreign calls from multiple Haskell threads and using -threaded, make sure that the foreign code you are calling is thread-safe. In particularly, some GUI libraries are not thread-safe and require that the caller only invokes GUI methods from a single thread. If this is the case, you may need to restrict your GUI operations to a single Haskell thread, and possibly also use a bound thread (see Section 8.2.4.2, “The relationship between Haskell threads and OS threads”).

-}

