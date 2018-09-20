> -- |
> -- Module      : Puffin.Parser
> -- Description : Parser for Puffin
> -- Copyright   : (c) Kove W. Ochre-Salter, 2018
> -- License     : MIT
> -- Maintainer  : kove.w.o.salter@gmail.com
> -- Stability   : experimental
> -- Portability : Cross platform
> -- 
> -- This module contains a parser for the Puffin project.

> module Puffin.Parser (fromBird) where

> import Puffin.Section (Section (..))

> import Text.ParserCombinators.Parsec

> import qualified System.Eol (eol)

> -- |The parser for converting literate-Haskell
> -- in the Puffin style - into a general data form.
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
> --  >>> let myPuffinExample = readFile "./PuffinExample.lhs"
> --  >>> fromBird myPuffinExample
> --  [DocBlock ["This module is an example.", "This is an example module."], EmptyLine, CodeLine "module PuffinExample (puffinExample) where", EmptyLine, DocBlock ["This function is an example.", "This is an example function."], EmptyLine, CodeLine "puffinExample :: IO ()", CodeLine "puffinExample =",CodeLine "  do putStrLn \"This function is an example.\"", CodeLine "     putStrLn \"This is an example function.\""]

> fromBird :: String -> FilePath -> Either ParseError [Section]
> fromBird = parse sections

> -- |Match zero or more sections.

> sections :: Parser [Section]
> sections = manyTill section eof
>   where
>     section =  emptyLine
>            <|> commentLine
>            <|> cppLine
>            <|> codeLine
>            <|> docBlock

> -- |Match an empty line.

> emptyLine :: Parser Section
> emptyLine =
>   do eol
>      return EmptyLine

> -- |Match a comment line.

> commentLine :: Parser Section
> commentLine =
>   do noneOf "#|>"
>      line
>      return CommentLine

> -- |Match a CPP line.

> cppLine :: Parser Section
> cppLine =
>   do char '#'
>      line' <- line
>      return $ CppLine line'

> -- |Match a code line.

> codeLine :: Parser Section
> codeLine =
>   do string "> "
>      line' <- line
>      return $ CodeLine line'

> -- |Match a documentation block.

> docBlock :: Parser Section
> docBlock =
>   do lines <- many docLine
>      return $ DocBlock lines
>   where
>     docLine =
>       do string "| "
>          line

> -- | Match a line.

> line :: Parser String
> line = manyTill anyChar eol

> -- |Match a system-independent line ending.

> eol :: Parser ()
> eol =
>   do string System.Eol.eol
>      return ()
