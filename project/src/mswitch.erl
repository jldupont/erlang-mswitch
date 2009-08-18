%% Author: Jean-Lou Dupont
%% Created: 2009-08-15
%% Description: TODO: Add description to mswitch
-module(mswitch).

%%
%% Defines
%%
-define(SERVER,  mswitch).
-define(TIMEOUT, 1000).

%%
%% Exported Functions
%%
-export([
		 start_link/0, start_link/1,
		 stop/0,
		 rpc/2, rpc/3
		 ]).

-export([
		 publish/3,
		 subscribe/2,
		 unsubscribe/2,
		 getsubs/1
		 ]).

%%
%% Local Functions
%%
-export([
		 loop/0,
		 call/2,
		 handle/3,
		 send/3,
		 reply/2
		 ]).
%%
%% API Functions
%%
start_link() ->
	start_link([]).

start_link(Params) ->
	Pid = spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! {params, Params},
	{ok, Pid}.


stop() ->
	?SERVER ! stop.



%% @spec publish(Bus, Message) -> {ServerPid, ok} | {error, Reason}
%% Reason = rpcerror
%%
publish(FromNode, Bus, Message) ->
	rpc(FromNode, {publish, Bus, Message}).

%% @spec subscribe(Bus) -> {ServerPid, ok} | {error, Reason}
%% Reason = rpcerror
%%
subscribe(FromNode, Bus) ->
	rpc(FromNode, {subscribe, Bus}).

%% @spec unsubscribe(Bus) -> {ServerPid, ok} | {error, Reason}
%% Reason = rpcerror
%%
unsubscribe(FromNode, Bus) ->
	rpc(FromNode, {unsubscribe, Bus}).

%% @spec getsubs() -> {ServerPid, {busses, Busses}} | {error, Reason}
%% Reason = rpcerror
%% Busses = list()
%%
getsubs(FromNode) ->
	rpc(FromNode, getsubs).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%
%% Local Functions
%%

call(FromNode, Q) ->
	mng:msg("rpc: From[~p] Message[~p]", [FromNode, Q]),
	?SERVER ! {self(), {FromNode, Q}},
	receive
		{reply, ServerPid, Reply} ->
			{ServerPid, Reply};
	
		Other ->
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			{error, rpcerror}
	
	after ?TIMEOUT ->

			mng:msg("rpc: timeout"),
			{error, rpcerror}
	end.


%% @private
%% SERVER message loop
loop() ->
	receive
			
		{params, Params} ->
			put(params, Params);
		
		stop ->
			exit(ok);
		
		{From, {FromNode, Message}} -> handle(From, FromNode, Message);
		
		Other ->
			mng:msg("loop: unknown rx: ~p", [Other])

	end,
	loop().


%% API - SUBSCRIBE
%% ===============
%%
%% @private
handle(From, _FromNode, getsubs) ->
	Busses=mng:getbusses(),
	reply(From, {busses, Busses});


handle(From, FromNode, {subscribe, Bus}) ->
	mng:add_sub(Bus, FromNode),
	reply(From, ok);

%% API - UN-SUBSCRIBE
%% ==================
%%
%% @private
handle(From, FromNode, {unsubscribe, Bus}) ->
	mng:rem_sub(Bus, FromNode),
	reply(From, ok);


%% API - PUBLISH
%% =============
%%
%% @private
handle(From, _FromNode, {publish, Bus, Message}) ->
	Subscribers=mng:getsubs(Bus),
	send(From, Subscribers, Message).







send(_From, [], _Message) -> no_subs;
send(From, Subs, Message) -> dosend(From, Subs, Message).


dosend(_From, [], _Message) ->
	no_more_subs;

dosend(From, [Current|Rest], Message) ->
	_Ret=sendto(From, Current, Message),
	dosend(From, Rest, Message).


sendto(From, To, Message) ->
	try To ! {From, Message} of
		{From, Message} ->
			mng:msg("sendto: To[~p] Message[~p]", [To, Message]),
			ok;
		_ ->
			mng:delete_sub(To),
			{removed_sub, To}
	catch 
		_:_ ->
			mng:delete_sub(To),
			{removed_sub, To}
	end.




%% @private
reply(To, Message) ->
	To ! {reply, self(), Message}.


%% ----------------------               ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% REMOTE ACCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------               ------------------------------



%% @private
rpc(From, Message) ->
	Rnode=mng:getvar({mswitch, rnode}, undefined),
	rpc(From, Message, Rnode).

rpc(From, Message, undefined) ->
	Rnode=mng:make_node(?SERVER),
	put({mswitch, rnode}, Rnode),
	dorpc(Rnode, From, Message);


rpc(FromNode, Message, RemoteNode) ->
	dorpc(FromNode, RemoteNode, Message).


%% @private
dorpc(FromNode, RemoteNode, Message) ->
	
	case rpc:call(RemoteNode, mswitch, call, [FromNode, Message], 2000) of
		{badrpc, _Reason} ->
			rpcerror;
		
		Other ->
			Other
	end.

