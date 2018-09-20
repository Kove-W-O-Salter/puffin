> -- |
> -- Module      : Puffin                        
> -- Description : Puffin                        
> -- Copyright   : (c) Kove W. Ochre-Salter, 2018
> -- License     : MIT
> -- Maintainer  : kove.w.o.salter@gmail.com
> -- Stability   : experimental
> -- Portability : Cross platform
> --
> -- This module contains the a function for converting
> -- Puffin-styled literate-Haskell documentation into
> -- normal-styled Haskell documentation.

> module Puffin (puffin) where

> import Puffin.Section (Section (..))

> import Puffin.Parser (fromBird)

> import System.Directory (doesFileExist)

> import Data.List (intersperse)

> import System.Eol (eol)

> -- |Given an input filename and an output filename, translate
> -- the contents of the input file into a general data-representation,
> -- convert that general data-representation into normal Haskell and
> -- write the normal Haskell to the output file.
> -- For example:
> --   say we have the following literate-Haskell file named "PuffinExample.lhs"
> --   in are working directory:
> --
> --  @
> --    | This module is an example.
> --    | This is an example module.
> --  
> --    > module PuffinExample (puffinExample) where
> --  
> --    | This function is an example.
> --    | This is an example function.
> --  
> --    > puffinExample :: IO ()
> --    > puffinExample =
> --    >   do putStrLn "This function is an example."
> --    >      putStrLn "This is an example function."
> --  @
> --
> --  in GHCi we could run:
> --
> --  >>> puffin "PuffinExample.lhs" "PuffinExample.hs"
> --
> --  which will create the file "PuffinExample.hs" in our working directory:
> --
> --  @
> --    -- |This module is an example.
> --    -- This is an example module.
> --  
> --    module PuffinExample (puffinExample) where
> --  
> --    -- |This function is an example.
> --    -- This is an example function.
> --  
> --    puffinExample :: IO ()
> --    puffinExample =
> --      do putStrLn "This function is an example."
> --         putStrLn "This is an example function."
> -- @

> puffin :: FilePath -> FilePath -> IO ()
> puffin input output =
>   mif (doesFileExist input)
>     (do contents <- readFile input
>         case fromBird input contents of
>           Left  error   -> print error
>           Right general ->
>             mif (doesFileExist output)
>               (reject output "file already exists")
>               (writeFile output $ toNormal general))
>     (reject input "cannot find file")
>   where
>     reject subject body =
>       putStrLn $ concat ["error: '", subject, "': ", body, "."]

> -- |Given a monadic computation that will result in a boolean
> -- and two monadic computations that result in nothing, evaulate      
> -- the first expression; if it's true evauluate the second expression,
> -- else evaulate the third expression.

> mif :: Monad m => m Bool -> m () -> m () -> m ()
> mif condition true false =
>   do condition' <- condition
>      if condition' then
>        true
>      else
>        false

> -- |Given a list of sections, pretty-print those sections as normal Haskell.
> -- For example:
> --
> -- >>> toNormal [EmptyLine, CodeLine "main = putStrLn \"Hello, World.\"", EmptyLine]
> -- "\nmain = putStrLn \"Hello, World.\"\n"

> toNormal :: [Section] -> String
> toNormal []     = []
> toNormal (s:ss) = toNormal' s ++ toNormal ss
>   where
>     toNormal' EmptyLine        = eol
>     toNormal' CommentLine      = []
>     toNormal' (CppLine line)   = "#" ++ line ++ eol
>     toNormal' (CodeLine line)  = line ++ eol
>     toNormal' (DocBlock block) = "-- |" ++ (concat $ intersperse "-- " $ map (++eol) block)
