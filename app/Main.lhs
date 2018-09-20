> -- |
> -- Module      : Main
> -- Description : The main module of our executable
> -- Copyright   : (c) Kove W. Ochre-Salter, 2018
> -- License     : MIT
> -- Maintainer  : kove.w.o.salter@gmail.com
> -- Stability   : experimental
> -- Portability : Cross platform
> -- 
> -- The main module.

> module Main (main) where

> import Puffin (puffin)

> import System.IO (hFlush, stdout)

> import System.Environment (getArgs)

> main :: IO ()
> main =
>   do args <- getArgs
>      (input, output) <- assert args
>      puffin input output
>           

> -- |If an insufficient number of elements is in
> -- a list of strings, retrieve the missing.

> assert :: [String] -> IO (FilePath, FilePath)
> assert args =
>   case args of
>     [input] ->
>       do [output] <- prompt ["output-file: "]
>          return (input, output)
>     [input, output] -> return (input, output)
>     _ ->
>       do [input, output] <- prompt [ "input-file: ", "output-file: "]
>          return (input, output)

> -- |Prompt for each item in a list of strings.

> prompt :: [String] -> IO [String]
> prompt = mapM prompt'
>   where prompt' message =
>           do putStr' message
>              getLine
>         putStr' xs =
>           do putStr xs
>              hFlush stdout
