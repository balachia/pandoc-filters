{-
Pandoc filter to allow for sideways tables.
Compile with:

    ghc --make pandoc-internalref.hs
    
and use in pandoc with

    --filter [PATH]/pandoc-internalref
 -}
module Main where
import Text.Pandoc.JSON
import Text.Pandoc.Writers.LaTeX
import Data.List (stripPrefix)

sidefig :: Block -> Block
sidefig (Para [Image txt (src,'f':'i':'g':'[':tit)])
    | options == "sideways" = 
    where
        options = takeWhile (\a -> a /= ']') tit
        Just postoptions = stripPrefix (options ++ "]") tit
sidefig x = x


main = toJSONFilter makeref

makeref :: Inline -> [Inline]
makeref (Link txt ('#':ident, x)) 
    | Just subident <- stripPrefix "fig:" ident = reflink 
    | Just subident <- stripPrefix "tab:" ident = reflink 
    where reflink = [Link [RawInline (Format "latex") ("\\ref*{" ++ ident ++ "}")] ("#" ++ ident, x)]
makeref x = [x]
