# org-mode

`org-mode` is a library for parsing text in the [Emacs
org-mode](https://orgmode.org/) format.

## Assumptions

- File metadata like [In-buffer
  Settings](https://orgmode.org/manual/In_002dbuffer-Settings.html#In_002dbuffer-Settings)
  appears at the top of the file.
- Special timestamps like `DEADLINE` always appear in the order `CLOSED`,
  `DEADLINE`, then `SCHEDULED` if present together.
- If there are both a special timestamp and a regular timestamp present, the
  regular timestamp must appear on the next line (this is how Emacs itself
  inserts them).

## Specification Coverage

Basic styling:

| Feature               | Parses? |
|-----------------------|---------|
| Bold / Italics / etc. | Yes     |
| URLs / Images         | Yes     |
| Lists                 | Yes     |

Headings:

| Feature      | Parses? |
|--------------|---------|
| Nesting      | Yes     |
| `TODO`       | Yes     |
| Priorities   | Yes     |
| Tags         | Yes     |
| Timestamps   | Yes     |
| `PROPERTIES` | Yes     |

Tables:

| Feature    | Parses? |
|------------|---------|
| Basic      | Yes     |
| Formulas   | **No**  |
| Properties | **No**  |

Blocks:

| Feature     | Parses? |
|-------------|---------|
| Quotes      | Yes     |
| Examples    | Yes     |
| Source Code | Yes     |
| Center      | **No**  |
| Comment     | **No**  |
| Export      | **No**  |
| Verse       | **No**  |
