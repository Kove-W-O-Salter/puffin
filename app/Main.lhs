
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

> prompt :: [String] -> IO [String]
> prompt = mapM prompt'
>   where prompt' message =
>           do putStr' message
>              getLine
>         putStr' xs =
>           do putStr xs
>              hFlush stdout
