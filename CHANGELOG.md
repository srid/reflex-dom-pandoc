# Change Log for reflex-dom-pandoc

## 1.0.0.0

- Drop `URILink` and simplify link render configuration (#11)
- Get rid of `PandocRaw` complexity (pass raw node render as a function) (#13)
  - But leave it available as an exposed module for explicit user use.
- Pass attributes to link renderer (#13)
- Remove hardcoded semantic UI CSS classes on `<table>` and `<input>` (checkbox) elements
- Remove the wrapper "div"
- Allow configuring how to render code blocks (eg: without syntax highlighting)
- Render img element alt attribute (#12)
- Apply attributes for `<table>`

## 0.6.0.0

- Hide footnote references from search engine results (#10)

## 0.4.0.0

- Fix task list checkbox styling margins by using Semantic UI checkbox.
- Add a wrapping div with class "pandoc" for user styling of elements
- Make unicode quote characters less heavy
- Require pure Haskell skylighting
- Remove empty HTML attributes (#6)
- Fancy lists (#7)
- Make `LineBreak` generate `<br>`
- Footnote misalignment on Firefox (#9)

## 0.2.0.0

Initial public release
