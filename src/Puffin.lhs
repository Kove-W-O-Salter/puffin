
> module Puffin (puffin) where

> import Puffin.Section (Section (..))

> import Puffin.Parser (fromBird)

> import System.Directory (doesFileExist)

> import Data.List (intersperse)

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

> mif :: Monad m => m Bool -> m () -> m () -> m ()
> mif condition true false =
>   do condition' <- condition
>      if condition' then
>        true
>      else
>        false

> toNormal :: [Section] -> String
> toNormal []     = []
> toNormal (s:ss) = toNormal' s ++ toNormal ss
>   where
>     toNormal' EmptyLine        = "\n"
>     toNormal' CommentLine      = []
>     toNormal' (CppLine line)   = "#" ++ line ++ "\n"
>     toNormal' (CodeLine line)  = line ++ "\n"
>     toNormal' (DocBlock block) = concat $ intersperse "\n" $ concat [["{-|"], block, ["-}\n"]]
