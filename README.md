# `rebar3_cargo`
![CI](https://github.com/rusterlium/rebar3_cargo/workflows/CI/badge.svg)

This plugin for [`rebar3`](https://www.rebar3.org/) enables the automatic
building of Rust crates in an Erlang application.  The plugin will build all
crates in the `crates` directory and copy all binary outputs to
`priv/crates/<cratename>/<binary>`.  See the test application in this repository
for an example of a port program and NIF module implemented in Rust.

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
{plugins, [rebar3_cargo]}.

{provider_hooks, [
    {pre, [
        {compile, {cargo, build}}
    ]},
    {post, [
        {clean, {cargo, clean}},
        {eunit, {cargo, test}}
    ]}
]}.
```

This will automatically download and use `rebar3_cargo`.  Crates will be
compiled whenever the containing app is compiled.  The cargo `--release` switch
will be used when the `prod` profile is active.  For example:

``` sh
rebar3 as prod compile
```

# Application structure with Rust crates
To add crates to an Erlang application, place them in a `crates/` folder, and
reference them in a Cargo Workspace.  All crates found within will be built and
resulting artifacts will be placed in the `priv/crates/` folder.

# Acknowledgment
This plugin is based on earlier work by
[goertzenator](https://github.com/goertzenator):
[rebar3_rust](https://github.com/goertzenator/rebar3_rust)
