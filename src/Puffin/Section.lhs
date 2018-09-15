
> module Puffin.Section (Section (..)) where

> data Section = EmptyLine
>              | CppLine String
>              | CodeLine String
>              | DocBlock [String]
>              deriving Show
