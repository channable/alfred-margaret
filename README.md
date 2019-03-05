# Alfred-Margaret

Alfred-Margaret is a fast implementation of the Aho-Corasick string
searching algorithm in Haskell. It powers many string-related operations
in [Channable][channable].

The library is designed to work with the [`text`][text] package. It matches
directly on the internal UTF-16 representation of `Text` for efficiency. See the
[announcement blog post][blog-post] for a deeper dive into Aho-Corasick, and the
optimizations that make this library fast.

Alfred-Margaret is named after Alfred Aho and Margaret Corasick.

## Example

TODO: Add example.

## License

Alfred-Margaret is licensed under the 3-clause BSD license.

[channable]: https://www.channable.com/
[blog-post]: https://tech.channable.com/TODO
[text]:      https://github.com/haskell/text
