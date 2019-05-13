<img src="./logo.svg" width="150px" height="150px" alt="Logo" align="right"/>

# Haskell Spaceflight Workshop

[![Build Status](https://travis-ci.org/lancelet/space-workshop.svg?branch=master)](https://travis-ci.org/lancelet/space-workshop)

The workshop contents are now complete. Final modifications and updates may still be made prior to LambdaJam 2019, but the current version is representative.

The current notes for the workshop are available from
[GitHub pages](https://lancelet.github.io/space-workshop), built using Travis CI.

## Instructions for Participants

Please make sure that you download the required dependencies for the workshop and [the notes](https://lancelet.github.io/space-workshop) in advance. We support the [`stack`](https://www.haskellstack.org) and `cabal` build tools. Dependencies can be fetched by doing:

```
$ stack --install-ghc test --only-dependencies
```

or

```
$ cabal new-build --only-dependencies test:tests
```

in the checked-out repository.

## Docker Option

If you are unable to obtain the required dependencies prior to the workshop, we will be distributing (on temporary loan only) USB flash drives containing a Docker image with the dependencies pre-installed. If you use this option, please copy everything off the USB drive before passing it on. Please then follow the instructions in the README from the flash drive.

For reference, the Docker image itself was built using the [CI pipeline here](https://gitlab.com/jmerritt/haskell-space-workshop-docker-image), but the USB Flash drive also contains a snapshot of this GitHub repository and the notes.

## Solutions

Solutions are provided in the `Solutions` sub-modules. To use them, either call them directly from the problem code, OR set the environment variable `IDDQD=1`, which will cause the `todo` function to use the fallback solutions:

```
$ export IDDQD=1  # causes the `todo` function to use the provided solutions
```

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

Or with `cabal`:

```
$ export IDDQD=1
$ cabal new-test
$ cabal new-run tex-plots   # generates PGF-format plots for the LaTeX notes
$ cd notes
$ make                   # generates the LaTeX notes.pdf
```

Be careful: the LaTeX file has `\nonstopmode` set so that it doesn't hang the CI build. It may be best to remove this when making local changes so that errors are more obvious.

The Makefile for the notes supports a `watch` phony target to continuously watch the source files and re-run LaTeX as required:

```
$ make watch
```
