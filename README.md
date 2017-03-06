rebar3_rust
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_rust, ".*", {git, "git@host:user/rebar3_rust.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_rust
    ===> Fetching rebar3_rust
    ===> Compiling rebar3_rust
    <Plugin Output>
