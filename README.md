<img src="./logo.svg" width="150px" height="150px" alt="Logo" align="right"/>

# Haskell Spaceflight Workshop

[![Build Status](https://travis-ci.org/lancelet/space-workshop.svg?branch=master)](https://travis-ci.org/lancelet/space-workshop)

This workshop is still WIP.

The current notes for the workshop (VERY WIP) are available from
[GitHub pages](https://lancelet.github.io/space-workshop), built using Travis CI.

## Developer Notes

See the `.travis.yml` file for detailed CI build instructions.

A manual build can be performed as follows:

```
$ export IDDQD=1
$ stack test
$ stack exec tex-plots   # generates PGF-format plots for the LaTeX notes
$ cd notes
$ make                   # generates the LaTeX notes.pdf
```

Be careful: the LaTeX file has `\nonstopmode` set so that it doesn't hang the CI build. It may be best to remove this when making local changes so that errors are more obvious.
