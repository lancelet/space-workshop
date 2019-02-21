# launch-workshop

Still WIP!

## Examples

```
$ stack build
$ stack exec ode-examples   # creates ode_examples_shm.svg
$ stack exec lunar-ascent   # creates lunar-ascent.svg
```

## Theory Manual

```
$ cd notes
$ make      # creates notes.pdf
```

TODO: 
- Rename the theory manual to `theory-manual`.
- Create separate workshop notes (instructions for the workshop).

## Special Requirements

- Take a look at `extra-deps` in [`stack.yaml`](stack.yaml) for handling of various packages like `pragmatic-show` and `free-vector-spaces`.
