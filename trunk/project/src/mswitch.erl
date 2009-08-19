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
%% Management API Functions
%%
-export([
		 start_link/0, start_link/1,
		 stop/0
		 ]).

%%
%% Core API functions
%%
-export([
		 publish/2,
		 subscribe/2, unsubscribe/2,
		 getsubs/0
		 ]).

%%
%% Local Functions
%%
-export([
		 loop/0, call/2,
		 handle/3, send/3, reply/2
		 ]).
%% ----------------------                 ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% MANAGEMENT API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                 ------------------------------

%% Starts the mswitch
%%
start_link() ->
	do_start_link([]).

%% Starts the mswitch in debug mode (verbose)
%%
%% @spec start_link(debug) -> {ok, Pid}
start_link(debug) ->
	do_start_link([debug]).

%% @private
do_start_link(Params) ->
	Pid = spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! {params, Params},
	{ok, Pid}.

%% Stops the mswitch
%%
%% @spec stop() -> void()
stop() ->
	?SERVER ! stop.


%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% CORE API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------

%% Subscribe to a Bus with message delivery to MailBox
%%
%% The 'Server' parameter corresponds to a registered Pid used
%% for receiving messages. The 'Module' and 'Function' parameters
%% correspond to a valid Module:Function for handling the
%% rpc:call from mswitch.
%%
%% @spec subscribe(MailBox, Bus) -> {ServerPid, ok} | {error, Reason}
%%
%% MailBox = {Module, Function, Server}
%% Module = atom()
%% Function = atom()
%% Server = atom()
%%
%% ServerPid = pid()
%% Reason = rpcerror | mswitch_node_down
%%
subscribe(MailBox, Bus) ->
	rpc({subscribe, MailBox, Bus}).

%% Unsubscribe from Bus
%%
%% @spec unsubscribe(MailBox, Bus) -> {ServerPid, ok} | {error, Reason}
%% @see subscribe/2
%%
unsubscribe(MailBox, Bus) ->
	rpc({unsubscribe, MailBox, Bus}).


%% Publish Message on Bus
%%
%% @spec publish(Bus, Message) -> {ServerPid, ok} | {error, Reason}
%%
%% Reason = rpcerror | mswitch_node_down
%%
publish(Bus, Message) ->
	rpc({publish, Bus, Message}).


%% @spec getsubs() -> {ServerPid, {busses, Busses}} | {error, Reason}
%%
%% Reason = rpcerror | mswitch_node_down
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

%% @private
rpc(From, Message, undefined) ->
	Rnode=mng:make_node(?SERVER),
	put({mswitch, rnode}, Rnode),
	dorpc(From, Rnode, Message);

rpc(FromNode, Message, RemoteNode) ->
	dorpc(FromNode, RemoteNode, Message).


%% @private
dorpc(FromNode, RemoteNode, Message) ->
	%%io:format("dorpc: Fnode[~p] Rnode[~p] Message[~p]~n", [FromNode, RemoteNode, Message]),
	
	%% If the mswitch daemon is down, this call will fail first and thus
	%% {error, mswitch_node_down} will be received by the caller
	case rpc:call(RemoteNode, mswitch, call, [FromNode, Message], ?TIMEOUT) of
		{badrpc, _Reason} ->
			%%io:format("dorpc: badrpc: ~p~n", [Reason]),
			{error, mswitch_node_down};
		
		Other ->
			Other
	end.





%% ----------------------                 ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                 ------------------------------


call(FromNode, Q) ->
	%%io:format("call: from[~p] Q[~p]~n", [FromNode, Q]),
	%%mng:msg("rpc: From[~p] Message[~p]", [FromNode, Q]),
	?SERVER ! {self(), {FromNode, Q}},
	receive
		{reply, ServerPid, Reply} ->
			{ServerPid, Reply};
	
		Other ->
			%% This should only occur when the mswitch daemon is down
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			{error, rpcerror}
	
	after ?TIMEOUT ->
			error_logger:error_msg("~p: rpc: timeout~n", [?MODULE]),
			{error, rpc_timeout}
	end.


%% @private
%% SERVER message loop
loop() ->
	receive

		stop ->
			exit(ok);
	
		{params, Params} ->
			put(params, Params);
		
		{From, {FromNode, Message}} -> 
			handle(From, FromNode, Message);

		%% catch-all: shouldn't occur in normal case
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



%% rpc reply mechanism
%% @private
reply(To, Message) ->
	To ! {reply, self(), Message}.



%% @private
send(_FromNode, [], _Message) -> 
	no_subs;

send(FromNode, Subs, Message) -> 
	dosend(FromNode, Subs, Message).


%% @private
dosend(_FromNode, [], _Message) ->
	no_more_subs;

dosend(FromNode, [Current|Rest], Message) ->
	sendto(FromNode, Current, Message),
	dosend(FromNode, Rest, Message).


%% @private
%% don't send to self!
sendto(X, {X, {_,_,_}}, _Message) ->
	skip_self;

sendto(FromNode, To, Message) ->
	%% extract mailbox parameters
	{DestNode, {Module, Function, Server}} = To,
	mng:msg("sendto: Dst[~p] Mod[~p] Func[~p] Msg[~p]", [DestNode, Module, Function, Message]),
	
	try rpc:call(DestNode, Module, Function, [{FromNode, Server, Message}]) of
		
		%% Subscriber probably disappeared...
		{badrpc, _Reason} ->
			mng:delete_sub(To),
			{removed_sub, To};
	
		Other ->
			Other			
				 
	catch 
		_:_ ->
			mng:delete_sub(To),
			{removed_sub, To}
	end.

