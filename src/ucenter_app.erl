-module(ucenter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok,Ip} = application:get_env(ucenter,ip),
    {ok,Port} = application:get_env(ucenter,port),
    {ok,_}=ranch:start_listener(ucenter_server,2,ranch_tcp,[{port,Port},{ip,Ip},{backlog,1024},{nodelay, true}],ucenter_server,[]),
    user_manager:start_link(),
    share_manager:start_link(),
    ucenter_sup:start_link().

stop(_State) ->
    ok.
