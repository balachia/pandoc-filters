{-
Pandoc filter to allow for sideways tables.
Compile with:

    ghc --make pandoc-internalref.hs
    
and use in pandoc with

    --filter [PATH]/pandoc-internalref
 -}
module Main where
import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk, walkM)
import Text.Pandoc.Writers.LaTeX
import Data.List (stripPrefix)

sidefig :: Block -> Block
sidefig (Div (ident, classes, _) [Para [Image caps (src, _)]]) 
    | "sideways" `elem` classes = Para [
        RawInline (Format "latex") "\\begin{sidewaysfigure}\n\\centering\n",
        RawInline (Format "latex") ("\\includegraphics{" ++ src ++ "}\n"),
        RawInline (Format "latex") ("\\caption{" ++ (inlineListToLaTeX caps) ++ "}\n"),
        RawInline (Format "latex") "\\end{sidewaysfigure}"
        ]
sidefig x = x


main = toJSONFilter sidefig
