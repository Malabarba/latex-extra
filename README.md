latex-extra
=======

Defines extra commands and keys for LaTeX-mode. The additions fall
into the following three categories:

# 1-Key Compilation #

Tired of hitting `C-c C-c` 4 times (latex, bibtex, latex, view) for
the document to compile? This defines a much needed command that does
**everything** at once, and even handles compilation errors!

- `C-c C-a` **=>** `latex/compile-commands-until-done`

# Navigating Sections #

Five new keybindings are defined for navigating between
sections/chapters. These are meant to be intuitive to people familiar
with `org-mode`.

- `C-c C-n` **=>** `latex/next-section`  
Goes forward to the next section-like command in the buffer (\part,
\chapter, \(sub)section, or \(sub)paragraph, whichever comes first).
- `C-c C-p` **=>** `latex/previous-section`  
Same, but goes backward.
- `C-c C-u` **=>** `latex/up-section`  
Goes backward to the previous section-like command containing this
one. For instance, if you're inside a subsection it goes up to the
section that contains it.
- `C-c C-f` **=>** `latex/next-section-same-level`  
Like next-section, except it skips anything that's "lower-level" then
the current one. For instance, if you're inside a subsection it finds
the next subsection (or higher), skipping any subsubsections or
paragraphs.
- `C-c C-b` **=>** `latex/previous-section-same-level`  
Same, but goes backward.

# Whitespace Handling #

`latex-extra.el` improves `auto-fill-mode` so that it only applies to
text, not equations. To use this improvement, just activate
`auto-fill-mode` as usual.

It also defines a new command:  

- `C-c C-q` **=>** `latex/clean-fill-indent-environment`  
Completely cleans up the entire current environment. This involves:

1. Removing extraneous spaces and blank lines.
2. Filling text (and only text, not equations).
3. Indenting everything.
