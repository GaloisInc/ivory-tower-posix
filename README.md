
# [ivory-tower-posix][]

## About

This repository contains both a support library and a [Tower][] backend for
using the [Ivory][]/[Tower][] languages on a POSIX based system.

## Dependencies

Code generated using this library depends on
[libev](http://software.schmorp.de/pkg/libev.html).

The code itself depends on the Ivory/Tower ecosystem. See
[smaccmpilot-build](https://github.com/galoisinc/smaccmpilot-build) for more
info.

## Contributing

This project adheres to the
[Contributor Covenant code of conduct](CODE_OF_CONDUCT.md).
By participating, you are expected to uphold this code. Please report unaccpetable
behavior to [smaccm@galois.com](mailto:smaccm@galois.com).

## Running on Linux/OSX (Lee's notes)

What's on master isn't doesn't have a "REPL loop" so doesn't do anything. You'll
want the version on branch `jal-master`. You'll need to check out the
corresponding `jal-master` branches for `ivory` and `tower`, too.

After doing a `stack build`, run something like

```
stack exec test-tick-gen -- --src-dir="foo" --const-fold
```

to generate the C code for one of the tests.

For a particular example, in the generated makefile, you need to add a include
directive to point to `ev.h`; I installed with brew, so I have
`-I/usr/local/Cellar/libev/4.22/include`.

You'll also have to set the `LIBRARY_PATH` to locate `libev.a`. For example, I run

```
LIBRARY_PATH=/usr/local/Cellar/libev/4.22/lib make
```

Also, there are a couple of asserts in `tower_init.c` that'll fail due to
assumptions about priorities on the Raspberry PI. Delete those.
