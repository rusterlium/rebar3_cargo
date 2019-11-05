# `rebar3_cargo`
[![Build Status](https://travis-ci.org/goertzenator/rebar3_cargo.svg?branch=master)](https://travis-ci.org/goertzenator/rebar3_cargo)

This plugin for [`rebar3`](https://www.rebar3.org/) enables the automatic building of Rust crates in an Erlang application.
The plugin will build all crates in the `crates` directory and copy all binary outputs to `priv/crates/<cratename>/<binary>`.
See the test application in this repository for an example of a port program and NIF module implemented in Rust.

## Todo list

As of this writing `rebar3_cargo` will build crates on
linux and passes tests, however it is still under construction.

Todo:
- allow cargo/rust compile flags
- `--target` flag handling
- Appveyor CI
- maybe external crate support using cargo clone


# Using the plugin
Use the plugin by adding the following to `rebar.config`:

``` erlang
{plugins, [
    { rebar3_cargo, ".*", {git, "https://github.com/goertzenator/rebar3_cargo", {branch, "master"}}}
]}.

{provider_hooks, [
    {post, [
        {compile, {rust, build}},
        {clean, {rust, clean}},
        {eunit, {rust, test}}
    ]}
]}.
```

This will automatically download and use `rebar3_cargo`.  Crates will be compiled whenever the containing app is compiled.
The cargo `--release` switch will be used when the `prod` profile is active.  For example:

``` sh
rebar3 as prod compile
```

# Application structure with Rust crates
To add crates to an Erlang application, place them in a `crates/` folder.  All crates found within will be built and resulting artifacts will be placed in the `priv/crates/` folder.

The library application [`find_crate`](https://github.com/goertzenator/find_crate) may be used to reliably find artifacts in `priv/crates` in a cross-platform manner.


Project structure:
```
myapp/
    rebar.config
    ebin/
        ...
    src/
         ...
    crates/
        foo_nif/
            Cargo.toml
            ...
        bar_port/
            Cargo.toml
             ...
    priv/
        crates/
            foo_nif/
                libfoo_nif.so
            bar_port/
                bar_port

```

