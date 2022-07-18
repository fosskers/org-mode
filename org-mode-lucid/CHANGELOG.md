# Changelog

## 1.6.3 (2022-07-17)

#### Fixed

- Account for `\n\n` gaps in Quote blocks.

## 1.6.2 (2022-04-26)

#### Added

- GHC 9 support.

## 1.6.1 (2021-06-04)

#### Fixed

- Match the newest `org-mode`.

## 1.6.0 (2021-04-05)

#### Added

- `OrgStyle` now contains the field `bulma :: Bool` for adding
  [Bulma](https://bulma.io/) classes to certain elements.

#### Changelog

- Images are now wrapped in a `<figure>` element as well.

## 1.5.0 (2020-09-04)

#### Added

- `toc` function is now exposed for manually generating an article's associated
  Table of Contents.

#### Changed

- Tables of Contents are no longer automatically injected into the `Html`
  produced by the `html` and `body` functions.
- The `tableOfContents` field of `OrgStyle` is no longer a `Maybe`. It must be
  provided, but can be given any value if you don't intend to call `toc`
  yourself.

#### Removed

- The `TOC` type had its `tocTitle :: Text` field removed.

## 1.4.0 (2020-07-13)

#### Added

- Headers and their contents can now be tweaked on the caller's end with the new
  `OrgStyle` field `sectionStyling`.

#### Removed

- The `hrBetweenSections` function has been removed. The same effect can be
  achieved with a custom `SectionStyling`:

```haskell
section :: O.SectionStyling
section depth h b = do
  h
  b
  when (depth == 1) $ hr_ []
```

## 1.3.0 (2020-03-24)

#### Changed

- `OrgStyle` has been given an extra field, `hrBetweenSections :: Bool`. If
  `True`, rendering will add a `<hr>` between top-level sections.
- `defaultStyle` has been updated to reflect the above.

## 1.2.0 (2020-03-19)

#### Changed

- `OrgStyle` has been given an extra field, `separator` for defining what should
  appear between adjacent elements (e.g. spaces between words).
- `defaultStyle` has been updated to reflect the above.

## 1.1.1 (2020-03-13)

#### Fixed

- Support for `org-mode-1.1`.

## 1.1.0 (2020-03-07)

#### Changed

- `OrgStyle` given a new field `highlighting` which allows one to inject
  customized rendering behaviour for Org source blocks. Combined with
  `skylighting-lucid`, this can be used for server-side syntax highlighting.
- Inlined `<code>` tags resulting from org highlighting with `~~` now also have
  an `org-highlighting` class.

## 1.0.1 (2020-03-06)

#### Fixed

- A minor rendering bug involving parentheses in lists, tables, and headings.

## 1.0.0 (2020-03-05)

#### Added

- Initial version of all `Html` conversion functions.
