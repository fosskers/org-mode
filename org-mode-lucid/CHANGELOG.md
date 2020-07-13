# Changelog

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
