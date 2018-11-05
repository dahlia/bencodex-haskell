Bencodex reader/writer for Haskell
==================================

[![Build Status][ci-badge]][ci]

This package implements [Bencodex] serialization format which extends
[Bencoding].

~~~~ haskell
> :set -XOverloadedStrings
> import Data.Bencodex
> let Right bVal = decodeStrict "lntfi123eu7:Unicodeu4:blobe"
> bVal
BList [BNull,BBool True,BBool False,BInteger 123,BText "Unicode",BText "blob"]
> encodeStrict bVal
"lntfi123eu7:Unicodeu4:blobe"
~~~~

[ci-badge]: https://travis-ci.com/dahlia/bencodex-haskell.svg?branch=master
[ci]: https://travis-ci.com/dahlia/bencodex-haskell
[Bencodex]: https://github.com/planetarium/bencodex
[Bencoding]: http://www.bittorrent.org/beps/bep_0003.html#bencoding


Author and license
------------------

Written by [Hong Minhee], and distributed under [GPLv3] or later.

[Hong Minhee]: https://hongminhee.org/
[GPLv3]: https://www.gnu.org/licenses/gpl-3.0.html
