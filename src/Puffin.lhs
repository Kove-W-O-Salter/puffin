
> module Puffin (puffin) where

> import Puffin.Section (Section (..))

> import Puffin.Parser (toSections)

> puffin :: FilePath -> FilePath -> IO ()
> puffin birdFile outputFile =
>   case fromBird birdFile of
>     Left  error   -> print error
>     Right general ->
>       do outputExists <- doesFileExist outputFile
>          if outputExists
>            then putStrLn $ concat [ "The output-file \'"
>                                   , outputFile
>                                   , "\' already exists."
>                                   ]
>            else writeFile outputFile (toNormal general)

> toNormal :: [Section] -> String
> toNormal []     = []
> toNormal (s:ss) = toNormal' s ++ toNormal ss
>   where toNormal' EmptyLine      = "\n"
>         toNormal' CppLine line   = "#" ++ line ++ "\n"
>         toNormal' CodeLine line  = line ++ "\n"
>         toNormal' DocBlock block = intersperse "\n" $ concat [ "{-|"
>                                                              , block
>                                                              , "-}\n"
>                                                              ]
