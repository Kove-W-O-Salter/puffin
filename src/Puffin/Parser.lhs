
> module Puffin.Parser (fromBird) where

> import Puffin.Section (Section (..))

> import Text.ParserCombinators.Parsec

> fromBird :: String -> FilePath -> Either ParseError [Section]
> fromBird = parse sections

> sections :: Parser [Section]
> sections = manyTill section eof
>   where
>     section =  emptyLine
>            <|> commentLine
>            <|> cppLine
>            <|> codeLine
>            <|> docBlock

> emptyLine :: Parser Section
> emptyLine =
>   do eol
>      return EmptyLine

> commentLine :: Parser Section
> commentLine =
>   do noneOf "#|>"
>      line
>      return CommentLine

> cppLine :: Parser Section
> cppLine =
>   do char '#'
>      line' <- line
>      return $ CppLine line'

> codeLine :: Parser Section
> codeLine =
>   do string "> "
>      line' <- line
>      return $ CodeLine line'

> docBlock :: Parser Section
> docBlock =
>   do lines <- many docLine
>      return $ DocBlock lines
>   where
>     docLine =
>       do string "| "
>          line

> line :: Parser String
> line = manyTill anyChar eol

> eol :: Parser ()
> eol =
>   do string "\n"
>      return ()
