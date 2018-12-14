-module(rebar3_shaderc_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER,  compile).
-define(NAMESPACE, shaderc).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace,  ?NAMESPACE},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 shaderc"},  % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Opts = rebar_state:get(State, shaderc, []),
    Apps = case rebar_state:current_app(State) of
               undefined -> rebar_state:project_apps(State);
               AppInfo -> [AppInfo]
           end,
    lists:foreach(compile_shaders(Opts), Apps),
    {ok, State}.

compile_shaders(Opts) ->
  Shaders = proplists:get_value(src_dir, Opts, "shaders_src"),
  SPIRV = proplists:get_value(spirv_dir, Opts, "priv/spirv"),
  Env = proplists:get_value(target_env, Opts, "vulkan1.0"),
  fun (AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    SrcDir = filename:join(AppDir, Shaders),
    OutDir = filename:join(AppDir, SPIRV),
    ShaderFiles = rebar_utils:find_files(SrcDir, ".*", true),
    ShaderOpts = [{src, SrcDir},
                  {out, OutDir},
                  {env, Env}
                 ],
    lists:foreach(compile_shader(ShaderOpts), ShaderFiles)
  end.

compile_shader(Opts) ->
  In = proplists:get_value(src, Opts),
  Out = proplists:get_value(out, Opts),
  Env = proplists:get_value(env, Opts),
  fun (Shader) ->
    RelPath = remove_prefix(In, Shader),
    FilenameOut = Out ++ RelPath ++ ".spv",
    MakeDirCmd = "mkdir -p " ++ filename:dirname(FilenameOut),
    rebar_utils:sh(MakeDirCmd, []),
    CompileCmd = lists:flatten(["glslangValidator ", Shader, " -o ", FilenameOut, " --target-env ", Env]),
    {ok, _} = rebar_utils:sh(CompileCmd, [use_stdout])
  end.

remove_prefix([H | T1], [H | T2]) ->
  remove_prefix(T1, T2);
remove_prefix(_, Rest) ->
  Rest.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
