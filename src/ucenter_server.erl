%%%-------------------------------------------------------------------
%%% @author  <xinming.yao@gmail.com>
%%% @copyright (C) 2012, 
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2012 by  <>
%%%-------------------------------------------------------------------
-module(ucenter_server).
-behaviour(gen_server).
%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-include("ucenter_main.hrl").
-record(state, {socket,keepalive=0}).
%%%===================================================================
%%% API
%%%===================================================================
-compile([{parse_transform, lager_transform}]). 
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link (_ListenPid,Socket,Transport,TransOps) ->
    gen_server:start_link( ?MODULE, [Socket,Transport,TransOps], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Socket,_,_]) ->
    lager:debug("start connection"),
    inet:setopts(Socket,[binary,{packet,4},{active,true}]),
    %erlang:send_after(1,self(),keepalive),
    {ok, #state{socket=Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp,Socket,<<?LOGIN_REQ:16,Bin/binary>>},State)->
    {ok,[Name,Pwd]}=msgpack:unpack(Bin),
   lager:debug("~p  ~p",[Name,Pwd]),
    case user_manager:login(Name,Pwd) of
	ok ->
	    Rep = msgpack:pack([0,<<"ok">>]),
	    gen_tcp:send(Socket,<<?LOGIN_REP:16,Rep/binary>>);
	{error,Reason}->
	    Rep = msgpack:pack([1,Reason]),
	    gen_tcp:send(Socket,<<?LOGIN_REP:16,Rep/binary>>)
    end,
    {noreply, State}
;
handle_info({tcp,Socket,<<?CREATE_SHARE_REQ:16,Bin/binary>>},State)->
    {ok,[Name,Ip,Port,Type]}=msgpack:unpack(Bin),
    Share = #share{name=Name,ip=Ip,port=Port,type=Type},
    case share_manager:create_share(Share) of
	ok ->
	    Rep = msgpack:pack([0,"ok"]),
	    gen_tcp:send(Socket,<<?CREATE_SHARE_REQ:16,Rep/binary>>);
	{error,Reason}->
	    Rep = msgpack:pack([1,Reason]),
	    gen_tcp:send(Socket,<<?CREATE_SHARE_REQ:16,Rep/binary>>)
    end,
    {noreply, State}
;

handle_info({tcp,Socket,<<?GET_SHARES_REQ:16,Bin/binary>>},State)->
    {ok,[_Name]}=msgpack:unpack(Bin),
    case share_manager:get_shares() of
	{ok,Shares} ->
	    S = lists:map(fun(#share{name=Name,ip=Ip,port=Port,type=Type})->
				  [Name,Ip,Port,Type] end,Shares),
	    Rep = msgpack:pack([0,S]),
	    gen_tcp:send(Socket,<<?CREATE_SHARE_REQ:16,Rep/binary>>);
	{error,Reason}->
	    Rep = msgpack:pack([1,Reason]),
	    gen_tcp:send(Socket,<<?CREATE_SHARE_REQ:16,Rep/binary>>)
    end,
    {noreply, State}
;
handle_info({tcp,_Socket,Bin},State)->
    lager:error("receive data:~p",[Bin]),
    {noreply, State};
handle_info({tcp_closed,_Socket}, State) ->
    {stop,normal,State};
handle_info(keepalive,State=#state{socket=Socket,keepalive=Count})->
    if Count > 10 ->
	    {stop,keepalive_timeout,State};
       true->
	    gen_tcp:send(Socket,term_to_binary(ping)),
	    erlang:send_after(5000,self(),keepalive),
	    {noreply,State#state{keepalive=Count+1}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



