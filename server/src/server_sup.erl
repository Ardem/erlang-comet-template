%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Supervisor for the server application.

-module(server_sup).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    {_Ok, ServerConfig} = init:get_argument(comet_config),
    {_Result, ConfigData} = file:consult(hd(ServerConfig)),  
    Web = web_specs(server_web, config(server_port, ConfigData)),
    Router = router_specs(router),
    Webservice = webservice_specs(webservice),
    Processes = [Web, Router, Webservice],
    Strategy = {one_for_all, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port},
                 {docroot, server_deps:local_path(["priv", "www"])}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.

router_specs(Mod) ->
    {Mod,
     {Mod, start_link, []},
     permanent, 5000, worker, dynamic}.

webservice_specs(Mod) ->
    {Mod,
     {Mod, start, []},
     permanent, 5000, worker, dynamic}.

config(Param, ConfigData) ->
    case lists:keyfind(Param, 1, ConfigData) of
        {Param,Value}->
           Value;
        false->
           undefined
   end.
