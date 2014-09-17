# pandoc-filters

[Pandoc](http://johnmacfarlane.net/pandoc/) needs a bit of help to give the
[full academic writing experience](https://github.com/jgm/pandoc/issues/813). In
particular, we're missing a few important features from LaTeX:

 -  internal references to figures and tables
 -  ability to use certain formatting environments (e.g. sidewaysfigure
    / sidewaystable)

The filters here try to help.

## Usage

1.  git clone the directory to your favorite folder

2.  compile the filters you want with [ghc](http://www.haskell.org/platform/)
    (e.g. `ghc --make pandoc-internalref.hs`) -- at some point I might push these
    onto cabal

3.  add the filter when you run pandoc (`pandoc in.md -o out.pdf --filter
    PATH/TO/pandoc-internalref`)

## pandoc-internalref

`pandoc-internalref` tries to implement the internal reference format
[suggested by jgm](https://github.com/jgm/pandoc/issues/813#issuecomment-21417209):

```markdown
![Image Caption!](image-place.png){#fig:image-ref class1 class2}

What a great [figure!](#fig:image-ref)
```

The filter transforms this to the equivalent of:

```markdown
<div id="fig:image-ref" class="class1 class2">
![Image Caption!](image-place.png)

</div>
```

Currently, `pandoc-internalref` only works for figures. What happens:

 -  the filter looks for single-image paragraphs with an attributes specification
    `{#fig:* class1 class2 ...}`
 -  the filter replaces the image paragraph with an internal div element having
    identifier "fig:\*" and classes "class1 class2 ..."
 -  for latex/pdf (and native for debugging) output, it adds a `\label{fig:*}` to
    the image caption
 -  for latex/pdf (+native) output, it wipes out the text of links pointing to
    `#fig:*` or `#tab:*` in favor of `\ref*{fig:*}` or `\ref*{tab:*}`

This should work for most cases, though it does mean that at the present moment
you need to name/number your figures by hand for formats other than latex:
I can't think of a good way to write text that sounds good in latex "Figure
`\ref`" and html "The figure" writing styles. Suggestions welcome.

It might be possible to make the latex filter write something like `Figure
\ref*{}` if not for capitalization issues.

### TODO:

 - tables

## pandoc-dropinenv

Pandoc is opinionated about what latex it writes -- swapping out environments
(e.g. figure for sideways figure, or longtable for anything else) would take
substantial work as a pandoc filter.

Instead, `pandoc-dropinenv` tries to write hooks for a tex postprocessor
(`pandoc-postprocess.py`) to muck around with the tex directly. At present this
handles figure -> sidewaysfigure well, and tables not at all.

## pandoc-postprocess

`pandoc-postprocess.py` is NOT a pandoc filter but is a python post-processor
for tex code. Use pandoc to make tex files, then use `python
pandoc-postprocess.py` to make it better. Currently, this postprocessor can
only replace one environment name by another. This works well for figure ->
sideways figure. It does NOT work for converting pandoc's preferred longtable
into a different table format.

## pandoc-sideways

`pandoc-sideways` reads the classes of a div to apply special LaTeX
environments.

This is a pain, as the filter has to write all of the latex -- pandoc's standard
writer is no help.

At present, this filter DOES NOT WORK.

### TODO:

 - implement any of this

