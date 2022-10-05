%%% @doc
%%% jex: jex
%%%
%%% This module is currently named `jex', but you may want to change that.
%%% Remember that changing the name in `-module()' below requires renaming
%%% this file, and it is recommended to run `zx update .app` in the main
%%% project directory to make sure the ebin/jex.app file stays in
%%% sync with the project whenever you add, remove or rename a module.
%%% @end

-module(jex).
-vsn("0.1.0").
-license("MIT").
-export([start/1]).

-include("$zx_include/zx_logger.hrl").


-spec start(ArgV) -> ok
    when ArgV :: [string()].

start(ArgV) ->
    ok = log(info, "ArgV: ~tp", [ArgV]),
    ok = dispatch(ArgV),
    zx:silent_stop().

help() ->
    io:format("~ts~n", [help_screen()]).


% TODO: be smart about creating jex_include
% TODO: make mindist tarballs
% TODO: make fulldist tarballs
% TODO: make tsdocs
% TODO: jex pull
% TODO: hints about where to find packages
% TODO: tarball shas
% TODO: tarball signatures
% TODO: tarball signatures

help_screen() ->
    ["welcome to hell\n"
     "\n"
     "COMMANDS:\n"
     "  cfgbarf         barf out the jex.eterms file (mostly to make sure it parses correctly)\n"
     "  echo home       echo $HOME\n"
     "  echo jexdir     echo $HOME/.jex\n"
     "  echo devdir     echo $HOME/.jex/dev\n"
     "  echo pkgname    name of current package\n"
     "  echo pkgdir     echo $HOME/.jex/dev/realm-name-X.Y.Z\n"
     "  echo deps       list dependencies of current package\n"
     "  echo pathof PKG list the path to PKG or \n"
     "  init            mkdir -p $HOME/.jex/dev\n"
     "  build           tsc && cp -r ./src/jex_include ./dist/\n"
     "      -w, --weak      continue building even if tsc fails\n"
     "      -f, --force     use cp -rf instead of cp -r\n"
     "  mindist         mkdir jex_mindist && cp -r src jex_mindist && cp -r dist jex_mindist && rm -r jex_mindist/src/jex_include\n"
     "      -f, --force     use cp -rf instead of cp -r\n"
     "  push            rsync -a jex_mindist/ PKGDIR\n"
     "  ls              ls $HOME/.jex/dev\n"
     "  jextree         tree $HOME/.jex/\n"
     "  rmpkg PKG       rm -r $HOME/.jex/dev/PKG\n"
     "  pull            pull each dependency into src/jx_include\n"
    ].


dispatch(["cfgbarf"])         -> cfgbarf();
dispatch(["echo", "home"])    -> echo(home);
dispatch(["echo", "jexdir"])  -> echo(jexdir);
dispatch(["echo", "devdir"])  -> echo(devdir);
dispatch(["echo", "pkgname"]) -> echo(pkgname);
dispatch(["echo", "pkgdir"])  -> echo(pkgdir);
dispatch(["echo", "deps"])    -> echo(deps);
dispatch(["init"])            -> init();
dispatch(["build" | Opts])    -> build(Opts);
dispatch(["mindist" | Opts])  -> mindist(Opts);
dispatch(["push"])            -> push();
dispatch(["ls"])              -> ls();
dispatch(["tree"])            -> tree();
dispatch(["rmpkg", Pkg])      -> rmpkg(Pkg);
dispatch(["pull"])            -> pull();
dispatch(_)                   -> help().






%%-----------------------------------------------------------------------------
%% jex cfgbarf
%%-----------------------------------------------------------------------------

cfgbarf() ->
    io:format("~tp~n", [file:consult("jex.eterms")]).


cfg() ->
    file:consult("jex.eterms").

%%-----------------------------------------------------------------------------
%% jex echo
%%-----------------------------------------------------------------------------

echo(home) ->
    tell(info, "~ts", [home()]);
echo(jexdir) ->
    tell(info, "~ts", [jexdir()]);
echo(devdir) ->
    tell(info, "~ts", [devdir()]);
echo(pkgname) ->
    tell(info, "~ts", [pkgname()]);
echo(pkgdir) ->
    tell(info, "~ts", [pkgdir()]);
echo(deps) ->
    PrintDep =
        fun(Dep) ->
            tell(info, "~ts", [Dep])
        end,
    lists:foreach(PrintDep, deps());
echo({pathof, Pkg}) ->
    tell(info, "~ts", [pathof(Pkg)]).


home() ->
    case os:getenv("HOME") of
        false -> error("You are running some retard system that doesn't have $HOME defined. Fuck off");
        HomeD -> HomeD
    end.


jexdir() ->
    filename:join(home(), ".jex").

devdir() ->
    filename:join(jexdir(), "dev").


pkgname() ->
    {ok, Cfg} = cfg(),
    Realm = proplists:get_value(realm, Cfg),
    Name  = proplists:get_value(name, Cfg),
    Vsn   = proplists:get_value(version, Cfg),
    io_lib:format("~tp-~tp-~ts", [Realm, Name, Vsn]).

pkgdir() ->
    filename:join(devdir(), pkgname()).


deps() ->
    {ok, Cfg} = cfg(),
    case proplists:get_value(deps, Cfg) of
        undefined ->
            error("jex.eterms is missing key `deps`");
        Deps ->
            Deps
    end.


pathof(Pkg) ->
    Filename = filename:join(devdir(), Pkg),
    case file_exists(Filename) of
        true    -> Filename;
        false   -> error({unknown_package, Pkg})
    end.

file_exists(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _}    -> true;
        {error, _} -> false
    end.



%%-----------------------------------------------------------------------------
%% jex init
%%-----------------------------------------------------------------------------

init() ->
    DevDir = devdir(),
    Cmd    = io_lib:format("mkdir -p ~ts", [DevDir]),
    _      = cmd(Cmd),
    ok.



%%-----------------------------------------------------------------------------
%% jex build
%%-----------------------------------------------------------------------------

build(Opts) ->
                 % flags              if flag         default
    OptsConfig = [{["-w", "--weak"],  {weak, weak},   {weak, strict}},
                  {["-f", "--force"], {force, force}, {force, dont_force}}],
    #{force := Force, weak := Weak} = parseopts(OptsConfig, Opts),
    _  = cmd("mkdir -p src/jex_include"),
    ok = tsc(Weak),
    ok = cp_jex_include(Force),
    ok.
    %ok.

tsc(strict) ->
    "" = cmd("tsc"),
    ok;
tsc(weak) ->
    _ = cmd("tsc"),
    ok.

cp_jex_include(dont_force) ->
    _ = cmd("cp -rv src/jex_include dist"),
    ok;
cp_jex_include(force) ->
    _ = cmd("cp -rvf src/jex_include dist"),
    ok.



%%-----------------------------------------------------------------------------
%% jex mindist
%%-----------------------------------------------------------------------------

mindist(Opts) ->
                 % flags              if flag         default
    OptsConfig = [{["-f", "--force"], {force, force}, {force, dont_force}}],
    #{force := Force} = parseopts(OptsConfig, Opts),
    _ = cmd("mkdir -p jex_mindist"),
    _ = mindist_cp(Force),
    _ = cmd("rm -r jex_mindist/src/jex_include"),
    ok.

mindist_cp(dont_force) ->
    _ = cmd("cp -rv src  jex_mindist"),
    _ = cmd("cp -rv dist jex_mindist"),
    ok;
mindist_cp(force) ->
    _ = cmd("cp -rvf src  jex_mindist"),
    _ = cmd("cp -rvf dist jex_mindist"),
    ok.



%%-----------------------------------------------------------------------------
%% jex push
%%-----------------------------------------------------------------------------

push() ->
    _ = cmd(io_lib:format("rsync -avv jex_mindist/ ~ts", [pkgdir()])),
    ok.


%%-----------------------------------------------------------------------------
%% jex ls
%%-----------------------------------------------------------------------------

ls() ->
    _ = cmd(io_lib:format("ls ~ts", [devdir()])),
    ok.

%%-----------------------------------------------------------------------------
%% jex tree
%%-----------------------------------------------------------------------------

tree() ->
    _ = cmd(io_lib:format("tree ~ts", [jexdir()])),
    ok.

%%-----------------------------------------------------------------------------
%% jex rmpkg Pkg
%%-----------------------------------------------------------------------------

rmpkg(Pkg) ->
    Filename = pathof(Pkg),
    _ = cmd(io_lib:format("rm -r ~ts", [Filename])),
    ok.

%%-----------------------------------------------------------------------------
%% jex pull
%%-----------------------------------------------------------------------------

pull() ->
    pull(deps()).

pull([Dep | Deps]) ->
    pull_dep(Dep),
    pull(Deps);
pull([]) ->
    tell(info, "no more dependencies to pull", []),
    ok.

pull_dep(Dep) ->
    Src = pathof(Dep),
    Dst = filename:join("./src/jex_include", Dep),
    _ = cmd(io_lib:format("mkdir -p ~ts", [Dst])),
    %tell(info, "path of ~s: ~s", [Dep, Src]),
    _ = cmd(io_lib:format("rsync -avv ~ts/ ~ts", [Src, Dst])),
    ok.

%%-----------------------------------------------------------------------------
%% INTERNALS
%%-----------------------------------------------------------------------------
cmd(Command) ->
    ok = tell("$ ~ts", [Command]),
    S  = os:cmd(Command),
    ok = tell("~ts", [S]),
    S.

parseopts(OptsConfig, Opts) ->
    log(info, "OptsConfig: ~tp", [OptsConfig]),
    log(info, "Opts: ~tp", [Opts]),
    DefaultOpts = default_opts(OptsConfig, #{}),
    log(info, "DefaultOpts: ~tp", [DefaultOpts]),
    Updater = updater(OptsConfig, #{}),
    log(info, "Updater: ~tp", [Updater]),
    Flags = getflags(Opts, []),
    UpdatedFlags = update_flags(Flags, Updater, DefaultOpts),
    log(info, "UpdatedFlags: ~tp", [UpdatedFlags]),
    UpdatedFlags.

update_flags([Flag | Rest], Updater, AccFlags) ->
    NewAcc =
        case maps:find(Flag, Updater) of
            {ok, {NewFlag, NewValue}} ->
                AccFlags#{NewFlag => NewValue};
            error ->
                error(["invalid flag", Flag])
        end,
    update_flags(Rest, Updater, NewAcc);
update_flags([], _Updater, FinalAcc) ->
    FinalAcc.

updater([{DashedFlags, IfFlag, _Def} | Rest], Acc) ->
    UndashedFlags = getflags(DashedFlags, []),
    Flagger =
        fun (UndashedFlag, UndashedFlagToOptionMap) ->
            UndashedFlagToOptionMap#{UndashedFlag => IfFlag}
        end,
    NewAcc = lists:foldl(Flagger, Acc, UndashedFlags),
    updater(Rest, NewAcc);
updater([], Acc) ->
    Acc.

default_opts([{_X, _Y, {Z, W}} | Rest], Acc) ->
    default_opts(Rest, Acc#{Z => W});
default_opts([], FinalAcc) ->
    FinalAcc.

% --flag (long flag)
getflags(["--"++LongFlag | Flags], Acc) ->
    NewAcc = [LongFlag | Acc],
    getflags(Flags, NewAcc);
% -xyz (many short flags)
getflags(["-"++ShortFlags | Flags], Acc) ->
    NewAcc = lists:map(fun(Char) -> [Char] end, ShortFlags) ++ Acc,
    getflags(Flags, NewAcc);
% no more options
getflags([], FinalAcc) ->
    FinalAcc.
