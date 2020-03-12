# org-mode

`org-mode` is a library for parsing text in the [Emacs
org-mode](https://orgmode.org/) format.

## Assumptions

- File metadata appears at the top of the file in a specific order.

## Unimplemented

The following don't parse as of yet:

- Table Formulas
- Heading tags
- Certain top-level document metadata
- Anything involving the TODO system or clocking-in
- `:PROPERTIES:`
- [In-buffer Settings](https://orgmode.org/manual/In_002dbuffer-Settings.html#In_002dbuffer-Settings)
