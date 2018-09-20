> -- |
> -- Module      : Puffin.Section
> -- Description : Abstract Syntax Tree for Puffin
> -- Copyright   : (c) Kove W. Ochre-Salter, 2018
> -- License     : MIT
> -- Maintainer  : kove.w.o.salter@gmail.com
> -- Stability   : experimental
> -- Portability : Cross platform
> -- 
> -- This module contains an Abstract Syntax Tree (AST) for
> -- the Puffin project.

> module Puffin.Section (Section (..)) where

> -- |A section of code, which may be:
> --
> --   * An empty line ('EmptyLine');
> --   * A comment line ('CommentLine');
> --   * A CPP expression ('CppLine');
> --   * A line of code ('CodeLine');
> --   * A documentation block ('DocBlock').

> data Section = EmptyLine
>              | CommentLine
>              | CppLine String
>              | CodeLine String
>              | DocBlock [String]
>              deriving Show
