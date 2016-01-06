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
