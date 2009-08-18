%% Author: Jean-Lou Dupont
%% Created: 2009-08-15
%% Description: Distributed Message Switch
%%
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
		 rpc/1, rpc/3
		 ]).

-export([
		 status/0,
		 publish/2,
		 subscribe/2,
		 unsubscribe/2,
		 getsubs/0
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

start_link(debug) ->
	start_link([debug]);

start_link(Params) ->
	Pid = spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! {params, Params},
	{ok, Pid}.


stop() ->
	?SERVER ! stop.


%% @spec status() -> {ServerPid, ok} | {error, Reason}
%% Reason = rpcerror
%%
status() ->
	rpc(status).

%% @spec publish(Bus, Message) -> {ServerPid, ok} | {error, Reason}
%% Reason = rpcerror
%%
publish(Bus, Message) ->
	rpc({publish, Bus, Message}).

%% @spec subscribe(MailBox, Bus) -> {ServerPid, ok} | {error, Reason}
%%
%% MailBox = {Module, Function, Server}
%% Module = atom()
%% Function = atom()
%% Server = atom()
%% Reason = rpcerror
%%
subscribe(MailBox, Bus) ->
	rpc({subscribe, MailBox, Bus}).

%% @spec unsubscribe(Bus) -> {ServerPid, ok} | {error, Reason}
%% @see subscribe/2
%%
unsubscribe(MailBox, Bus) ->
	rpc({unsubscribe, MailBox, Bus}).

%% @spec getsubs() -> {ServerPid, {busses, Busses}} | {error, Reason}
%% Reason = rpcerror
%% Busses = list()
%%
getsubs() ->
	rpc(getsubs).



%% ----------------------               ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% REMOTE ACCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------               ------------------------------


%% @private
rpc(Message) ->
	%% grab the source node for convenience to the caller
	From=node(),
	Rnode=mng:getvar({mswitch, rnode}, undefined),
	rpc(From, Message, Rnode).

rpc(From, Message, undefined) ->
	Rnode=mng:make_node(?SERVER),
	put({mswitch, rnode}, Rnode),
	dorpc(From, Rnode, Message);

rpc(FromNode, Message, RemoteNode) ->
	dorpc(FromNode, RemoteNode, Message).


%% @private
dorpc(FromNode, RemoteNode, Message) ->
	%%io:format("dorpc: Fnode[~p] Rnode[~p] Message[~p]~n", [FromNode, RemoteNode, Message]),
	case rpc:call(RemoteNode, mswitch, call, [FromNode, Message], ?TIMEOUT) of
		{badrpc, Reason} ->
			io:format("dorpc: badrpc: ~p~n", [Reason]),
			{error, Reason};
		
		{ServerPid, ok} ->
			{ServerPid, ok};
		
		Other ->
			io:format("dorpc: unknown message[~p]~n", [Other]),
			mng:msg("dorpc: unknown message[~p]", [Other]),
			Other
	end.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%
%% Local Functions
%%

call(FromNode, Q) ->
	%%io:format("call: from[~p] Q[~p]~n", [FromNode, Q]),
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
		
		{From, {FromNode, Message}} -> 
			handle(From, FromNode, Message);
		
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


handle(From, FromNode, {subscribe, MailBox, Bus}) ->
	mng:msg("subscribe: node[~p] bus[~p]", [FromNode, Bus]),
	mng:add_sub(Bus, {FromNode, MailBox}),
	reply(From, ok);

%% API - UN-SUBSCRIBE
%% ==================
%%
%% @private
handle(From, FromNode, {unsubscribe, MailBox, Bus}) ->
	mng:msg("unsubscribe: node[~p] bus[~p]", [FromNode, Bus]),	
	mng:rem_sub(Bus, {FromNode, MailBox}),
	reply(From, ok);


%% API - PUBLISH
%% =============
%%
%% @private
handle(From, FromNode, {publish, Bus, Message}) ->
	Subscribers=mng:getsubs(Bus),
	send(FromNode, Subscribers, Message),
	reply(From, ok).







send(_FromNode, [], _Message) -> no_subs;
send(FromNode, Subs, Message) -> dosend(FromNode, Subs, Message).


dosend(_FromNode, [], _Message) ->
	no_more_subs;

dosend(FromNode, [Current|Rest], Message) ->
	sendto(FromNode, Current, Message),
	dosend(FromNode, Rest, Message).


%% @private
%% don't send to self!
sendto(X, {X, {_,_,_}}, _Message) ->
	%%mng:msg("skip self!"),
	skip_self;

sendto(FromNode, To, Message) ->
	%% extract mailbox parameters
	{DestNode, {Module, Function, Server}} = To,
	mng:msg("sendto: Dest[~p] Module[~p] Function[~p] Message[~p]", [DestNode, Module, Function, Message]),
	
	try rpc:call(DestNode, Module, Function, [{FromNode, Server, Message}]) of
		{FromNode, Message} ->
			ok;
		Other ->
			mng:msg("rpc:call error: [~p]",[Other]),
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


