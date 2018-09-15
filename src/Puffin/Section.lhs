
> module Puffin.Section (Section (..)) where

> data Section = EmptyLine
>              | CommentLine
>              | CppLine String
>              | CodeLine String
>              | DocBlock [String]
>              deriving Show
