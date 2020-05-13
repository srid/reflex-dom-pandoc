# reflex-dom-pandoc

Render Pandoc documents in reflex-dom.

Not all parts of the Pandoc AST are handled; the following in particular needs to be done:

- [ ] Table is rendered only in basic fashion; but its attributes are not handled.
- [ ] `Citation` (Pandoc's `Cite` node)
- [ ] Raw blocks and inlines
