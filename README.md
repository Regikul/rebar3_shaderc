rebar3_shaderc
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

`glslangValidator` should be in `$PATH`

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_shaderc, {git, "https://github.com/regikul/rebar3_shaderc", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 shaderc compile
    ===> Fetching rebar3_shaderc
    ===> Compiling rebar3_shaderc
    <Plugin Output>

Or add precompile hook:

    {provider_hooks, [
      {pre, [
        {compile, {shaderc, compile}}
      ]}
    ]}.


Configure
---------

Configuration is optional

    {shaderc, [
        {src_dir, "shaders_src"},
        {spirv_dir, "priv/spirv"},
        {target_env, "vulkan1.0"}
    ]}.
