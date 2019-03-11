# Alfred-Margaret

Alfred-Margaret is a fast implementation of the Aho-Corasick string
searching algorithm in Haskell. It powers many string-related operations
in [Channable][channable].

The library is designed to work with the [`text`][text] package. It matches
directly on the internal UTF-16 representation of `Text` for efficiency. See the
[announcement blog post][blog-post] for a deeper dive into Aho-Corasick, and the
optimizations that make this library fast.

Alfred-Margaret is named after Alfred Aho and Margaret Corasick.

## Performance

Running time to count all matches, in a real-world data set,
comparing [a Java implementation][hankcs] and [a Rust implementation][burntsushi]
against Alfred-Margaret, and against memcopy to establish a lower bound:

<p align="center">
<img
  title="Graph that shows that Alfred-Margaret is fast."
  src="performance.png"
  width="80%"
>
</p>

For the full details of this benchmark, see
[our announcement blog post][blog-post], which includes more details about the
data set, the benchmark setup, and a few things to keep in mind when
interpreting this graph.

## Example

Check if a string contains one of the needles:

```haskell
import qualified Data.Text.AhoCorasick.Searcher as Searcher

searcher = Searcher.build ["tshirt", "shirts", "shorts"]

Searcher.containsAny searcher "short tshirts"
> True

Searcher.containsAny searcher "long shirt"
> False

Searcher.containsAny searcher "Short TSHIRTS"
> False

Searcher.containsAnyIgnoreCase searcher "Short TSHIRTS"
> True
```

Sequentially replace many needles:

```haskell
import Data.Text.AhoCorasick.Automaton (CaseSensitivity (..))
import qualified Data.Text.AhoCorasick.Replacer as Replacer

replacer = Replacer.build CaseSensitive [("tshirt", "banana"), ("shirt", "pear")]

Replacer.run replacer "tshirts for sale"
> "bananas for sale"

Replacer.run replacer "tshirts and shirts for sale"
> "bananas and pears for sale"

Replacer.run replacer "sweatshirts and shirtshirts"
> "sweabananas and shirbananas"

Replacer.run replacer "sweatshirts and shirttshirts"
> "sweabananas and pearbananas"
```

## License

Alfred-Margaret is licensed under the 3-clause BSD license.

[channable]:  https://www.channable.com/
[blog-post]:  https://tech.channable.com/TODO
[text]:       https://github.com/haskell/text
[hankcs]:     https://github.com/hankcs/AhoCorasickDoubleArrayTrie/tree/v1.2.0
[burntsushi]: https://github.com/BurntSushi/aho-corasick/tree/0.6.8
