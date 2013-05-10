## Slice code skeleton generator from Slice files

This module generates simple slice skeletons based on parsing a slice file.
The idea is to use it to rename the variables in a slice file which might
be used for testing the slice parser (in a circular logic kind of way).

It relies on [language-slice](https://github.com/paulkoerbitz/language-slice).