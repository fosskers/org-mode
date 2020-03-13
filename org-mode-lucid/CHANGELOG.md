# Changelog

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
