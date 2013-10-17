-module(ucenter_server_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("ucenter_main.hrl"). 
-compile([{parse_transform, lager_transform}]). 
suite() ->
    [{timetrap,{seconds,30}}].
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.
init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, _Config) ->
    ok.
groups() ->
    [].

all() -> 
    [test_server].
test_server(_C)->
    start_lager(),
    application:start(ranch),
    Ip={127,0,0,1},
    Port=8088,
    application:set_env(ucenter,ip,Ip),
    application:set_env(ucenter,port,Port),
    application:start(ucenter),
    %%application:stop(ranch),
    %%application:stop(rtsp_stack),
    ok.
get_log()->
    get_node_name()++".log".
start_lager()->
    Log=get_log(),
    os:cmd("rm -f "++ Log),
    application:load(lager),
    application:set_env(lager, handlers, [{lager_console_backend,debug}
					  ,{lager_file_backend,[{Log,debug,10485760,"$D0",5}]}
					 ]),
    application:set_env(lager, error_logger_redirect, true),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    ok.

get_node_name()->
    N=atom_to_list(node()),
    {match,[_,{S1,E1}]}= re:run(N,"[^@](.*)@.*",[]),
    "/tmp/"++string:substr(N,S1,E1+1).
