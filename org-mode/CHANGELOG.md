# Changelog

## 2.1.0 (2023-04-19)

#### Changed

- Support numbered lists.

## 2.0.2 (2022-07-31)

#### Changed

- GHC 9.2 support.

## 2.0.1 (2022-04-26)

#### Added

- GHC 9 support.

## 2.0.0 (2021-06-04)

This release sees the extension of this library to understand heading
timestamps, TODOs, priorities, and `PROPERTIES` drawers.

#### Added

- A number of new timestamp types.
- `Ord` instances for every type.
- Functions `allDocTags` and `allSectionTags` for extracting unique `Set`s of
  recursive heading tags.

#### Changed

- **Breaking:** `Section` has several new fields involving timestamps and properties.
- **Breaking:** The `Tags` variant has been removed from the `Words` type.

#### Fixed

- Heading tags are properly parsed.

## 1.1.1 (2021-04-04)

#### Fixed

- Widened `megaparsec` bounds.

## 1.1.0 (2020-03-13)

If you're reading this from the future, everyone is being silly about the Corona
Virus right now.

#### Added

- Support for heading tags, like `:foo:`.

#### Changed

- `OrgFile` now contains a raw `Map Text Text` for all metadata key-value pairs.

#### Removed

- The `Meta` type has been removed.

## 1.0.1 (2020-03-06)

#### Fixed

- Fixed broken test suite due to missing files.

## 1.0.0 (2020-03-05)

#### Added

- Initial version of all types, parsers, and pretty-printers.
