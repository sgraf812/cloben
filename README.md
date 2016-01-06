# `cloben`

`cloben` is a Haskell shell script to clone git repositories and transform `cabal bench` results into a CSV file readable by `gipeda` for visualization.

It parses build warnings and timing data output in the standard `criterion` format.

# Usage

`cloben <repo> <commit>` will output the CSV data on `stdout`. This takes 10 minutes even for the simplest possible `criterion` setup on my MacBook, so stay calm :).

For usage with `gipeda`, `stdout` should be redirected into a csv file.

See also `cloben --help`.

# How to build

Simplest way? Don't! Use [`stack`s excellent support for `runghc`](http://docs.haskellstack.org/en/stable/GUIDE.html#script-interpreter):
```
$ stack cloben.hs <repo> <commit>
```
Or on unixoid systems:
```
$ chmod +x cloben.hs
$ ./cloben.hs <repo> <commit>
```

Of course, `cloben` can be built both in a `cabal` and in a `stack` environment.
```
$ stack build && stack exec cloben <repo> <commit>
...
$ cabal install -j && cabal run <repo> <commit>
...
```

# Example

What running `cloben` on the `Pipes` library yielded:

```
$ ./cloben.hs https://github.com/Gabriel439/Haskell-Pipes-Library 930c834aacfa7bf8ec65d072e0d0a982aa7a2bc1 > logs/930c834aacfa7bf8ec65d072e0d0a982aa7a2bc1.csv
# Some irrevelant out-of-band git output
$ cat logs/930c834aacfa7bf8ec65d072e0d0a982aa7a2bc1.csv
enchmark/prelude-benchmarks/Folds/all;166800.0
benchmark/prelude-benchmarks/Folds/any;163700.0
benchmark/prelude-benchmarks/Folds/find;170800.0
benchmark/prelude-benchmarks/Folds/findIndex;164500.0
benchmark/prelude-benchmarks/Folds/fold;68150.0
benchmark/prelude-benchmarks/Folds/foldM;67690.0
benchmark/prelude-benchmarks/Folds/head;10.84
benchmark/prelude-benchmarks/Folds/index;117000.0
benchmark/prelude-benchmarks/Folds/last;119600.0
benchmark/prelude-benchmarks/Folds/length;53960.0
benchmark/prelude-benchmarks/Folds/null;11.2
benchmark/prelude-benchmarks/Folds/toList;138100.0
benchmark/prelude-benchmarks/Pipes/chain;1351000.0
benchmark/prelude-benchmarks/Pipes/drop;110400.0
benchmark/prelude-benchmarks/Pipes/dropWhile;159000.0
benchmark/prelude-benchmarks/Pipes/filter;585100.0
benchmark/prelude-benchmarks/Pipes/findIndices;397900.0
benchmark/prelude-benchmarks/Pipes/map;324600.0
benchmark/prelude-benchmarks/Pipes/mapM;1276000.0
benchmark/prelude-benchmarks/Pipes/take;346500.0
benchmark/prelude-benchmarks/Pipes/takeWhile;332400.0
benchmark/prelude-benchmarks/Pipes/scan;370900.0
benchmark/prelude-benchmarks/Pipes/scanM;1177000.0
benchmark/prelude-benchmarks/Pipes/concat;159600.0
benchmark/prelude-benchmarks/Zips/zip;1218000.0
benchmark/prelude-benchmarks/Zips/zipWith;1318000.0
benchmark/prelude-benchmarks/enumFromTo.vs.each/enumFromTo;205700.0
benchmark/prelude-benchmarks/enumFromTo.vs.each/each;209000.0
benchmark/lift-benchmarks/ReaderT/runReaderP_B;4791000.0
benchmark/lift-benchmarks/ReaderT/runReaderP_A;266600.0
benchmark/lift-benchmarks/StateT/runStateP_B;4912000.0
benchmark/lift-benchmarks/StateT/runStateP_A;344800.0
benchmark/lift-benchmarks/StateT/evalStateP_B;5534000.0
benchmark/lift-benchmarks/StateT/evalStateP_A;349400.0
benchmark/lift-benchmarks/StateT/execStateP_B;5451000.0
benchmark/lift-benchmarks/StateT/execStateP_A;324300.0
```
