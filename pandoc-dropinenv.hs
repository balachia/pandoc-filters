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

-- replacing the actual tex code seems mind-numbingly difficult
-- because pandoc exports nothing of its internals.
-- instead, we're going to rely on a post-processing hook in the tex file for a python script.
-- hackish approach, but will save a lot of heartache
sidefig :: Block -> Block
sidefig (Div (ident, classes, kvs) [Para [Image caps (src, src2)]]) 
    | "sideways" `elem` classes =
        (Div (ident, classes, kvs) [
            RawBlock (Format "latex") "%% @replace-next-environment figure sidewaysfigure",
            Para [Image caps (src, src2)] ])
sidefig (Div (ident, classes, kvs) [Table caps aligns widths headers rows])
    | "sideways" `elem` classes =
        (Div (ident, classes, kvs) [
            RawBlock (Format "latex") "%% @replace-next-environment longtable sidewaystable",
            Table caps aligns widths headers rows ])
    | "tabular" `elem` classes =
        (Div (ident, classes, kvs) [
            RawBlock (Format "latex") "%% @replace-next-environment longtable tabular",
            Table caps aligns widths headers rows ])
    {-Para [-}
        {-RawInline (Format "latex") "\\begin{sidewaysfigure}\n\\centering\n",-}
        {-RawInline (Format "latex") ("\\includegraphics{" ++ src ++ "}\n"),-}
        {-RawInline (Format "latex") ("\\caption{" ++ (inlineListToLaTeX caps) ++ "}\n"),-}
        {-RawInline (Format "latex") "\\end{sidewaysfigure}"-}
        {-]-}
sidefig x = x


main = toJSONFilter sidefig
