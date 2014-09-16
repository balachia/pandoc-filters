# pandoc-filters

[Pandoc](http://johnmacfarlane.net/pandoc/) needs a bit of help to give the full
academic writing experience. In particular, we're missing a few important
features from LaTeX:

 -  internal references to figures and tables
 -  ability to use certain formatting environments (e.g. sidewaysfigure
    / sidewaystable)

The filters here try to help.

## pandoc-internalref

`pandoc-internalref` tries to implement the internal reference format (suggested
by jgm)[https://github.com/jgm/pandoc/issues/813#issuecomment-21417209]:

```markdown
![Image Caption!](image-place.png){#fig:image-ref class1 class2}

What a great (figure!)[#fig:image-ref]
```

The filter transforms this to the equivalent of:

```markdown
<div id="fig:image-ref" class="class1 class2">
![Image Caption!](image-place.png)

</div> ```

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
`\ref`" and html "The figure" modes.

It might be possible to make the latex filter write something like `Figure
\ref*{}`

### TODO:

 - tables

## pandoc-sideways

`pandoc-sideways` reads the classes of a div to apply special LaTeX
environments.

This is hard, as the filter has to write all of the latex -- pandoc's standard
writer is no help.

### TODO:

 - implement any of this

