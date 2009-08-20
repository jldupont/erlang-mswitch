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
		 stop/0,
		 rpc/1,
		 daemon_api/1
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
		 handle/3, send/3, reply/2,
		 set_code_path/0
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
	set_code_path(),
	Pid = spawn_link(?MODULE, loop, []),
	register(mswitch_server, Pid),
	global:register_name(mswitch, Pid),
	Pid ! {params, Params},
	{ok, Pid}.

%% Stops the mswitch
%%
%% @spec stop() -> void()
stop() ->
	mswitch_server ! stop.


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
%% Bus = atom() | [atom()]
%% MailBox = {Module, Function, Server}
%% Module = atom()
%% Function = atom()
%% Server = atom()
%%
%% ServerPid = pid()
%% Reason = rpcerror | mswitch_node_down | invalid_bus | invalid_mailbox
%%
subscribe(_, {_}) ->
	{error, invalid_bus};

subscribe(_, Bus) when length(Bus)==0 ->
	{error, invalid_bus};

subscribe({}, _) ->
	{error, invalid_mailbox};

subscribe({Module, Function, Server}, Bus) ->
	MailBox={Module, Function, Server},
	Ret=rpc({subscribe, MailBox, Bus}),
	sync({subscribe, MailBox, Bus, Ret});

subscribe(_, _) ->
	{error, invalid_mailbox}.

%% Unsubscribe from Bus
%%
%% @spec unsubscribe(MailBox, Bus) -> {ServerPid, ok} | {error, Reason}
%% @see subscribe/2
%%
unsubscribe(_, {_}) ->
	{error, invalid_bus};

unsubscribe(_, Bus) when length(Bus)==0 ->
	{error, invalid_bus};

unsubscribe({}, _) ->
	{error, invalid_mailbox};
	
unsubscribe({Module, Function, Server}, Bus) ->
	MailBox={Module, Function, Server},
	Ret=rpc({unsubscribe, MailBox, Bus}),
	sync({unsubscribe, MailBox, Bus, Ret});

unsubscribe(_, _) ->
	{error, invalid_mailbox}.

%% Publish Message on Bus
%%
%% @spec publish(Bus, Message) -> {ServerPid, ok} | {error, Reason}
%%
%% Reason = rpcerror | mswitch_node_down | invalid_bus
%%
publish({_}, _) ->
	{error, invalid_bus};

publish(Bus, _) when is_list(Bus) ->
	{error, invalid_bus};

publish(Bus, Message) ->
	check_sync(),
	Ret=rpc({publish, Bus, Message}),
	sync({publish, Ret}).


%% @spec getsubs() -> {ServerPid, {busses, Busses}} | {error, Reason}
%%
%% Reason = rpcerror | mswitch_node_down
%% Busses = list()
%%
getsubs() ->
	rpc(getsubs).


%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SYNC SUPPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------

check_sync() ->
	SyncState=get({mswitch, out_of_sync}),
	dosync(SyncState).

dosync(true) ->
	Busses=mng:find_node_subscriptions(local, node()),
	io:format("dosync: busses: ~p~n",[Busses]),
	MB=mng:getsubmailbox(local, node()),
	rpc({subscribe, MB, Busses}),
	ok;

dosync(_) ->
	ok.


sync({subscribe, MailBox, Bus, {error, Reason}}) ->
	tools:msg("sync: MB[~p] Bus[~p]~n", [MailBox, Bus]),
	put({mswitch, out_of_sync}, true),
	mng:add_sub(local, Bus, {node(),MailBox}),
	{error, Reason};

sync({subscribe, MailBox, Bus, Ret}) ->
	put({mswitch, out_of_sync}, false),
	mng:add_sub(local, Bus, {node(),MailBox}),
	Ret;

sync({unsubscribe, MailBox, Bus, {error, Reason}}) ->
	put({mswitch, out_of_sync}, true),
	mng:rem_sub(local, Bus, MailBox),
	{error, Reason};

sync({unsubscribe, MailBox, Bus, Ret}) ->
	put({mswitch, out_of_sync}, false),
	mng:rem_sub(local, Bus, MailBox),
	Ret;

sync({publish, {error, Reason}}) ->
	put({mswitch, out_of_sync}, true),
	{error, Reason};

sync({publish, Ret}) ->
	put({mswitch, out_of_sync}, false),
	Ret.





%% ----------------------               ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% REMOTE ACCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------               ------------------------------


%% @private
rpc(Message) ->
	%% grab the source node for convenience to the caller
	From=node(),
	Rnode=tools:getvar({mswitch, rnode}, undefined),
	rpc(From, Message, Rnode).

%% @private
rpc(From, Message, undefined) ->
	Rnode=tools:make_node(?SERVER),
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
		{badrpc, Reason} ->
			io:format("dorpc: badrpc: ~p~n", [Reason]),
			{error, mswitch_node_down};
		
		Other ->
			Other
	end.





%% ----------------------                 ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                 ------------------------------


%% @private
call(FromNode, Q) ->
	%%io:format("call: from[~p] Q[~p]~n", [FromNode, Q]),
	%%tools:msg("rpc: From[~p] Message[~p]", [FromNode, Q]),
	mswitch_server ! {self(), {FromNode, Q}},
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
			error_logger:error_msg("?: loop: unknown [~p]~n", [?MODULE, Other]),
			tools:msg("loop: unknown rx: ~p", [Other])

	end,
	loop().


%% ----------------------                              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% API IMPLEMENTATION FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                              ------------------------------


%% API - SUBSCRIBE
%% ===============
%%
%% @private
handle(From, _FromNode, {subscribe, _MailBox, []}) ->
	reply(From, ok);

handle(From, FromNode, {subscribe, MailBox, Bus}) ->
	tools:msg("subscribe: node[~p] bus[~p]", [FromNode, Bus]),
	mng:add_sub(Bus, {FromNode, MailBox}),
	
	%% Notification on "system bus"
	send(mswitch, {system, {subscribe, From, Bus, MailBox}}),
	
	%% rpc reply
	reply(From, ok);

%% API - UN-SUBSCRIBE
%% ==================
%%
%% @private
handle(From, _FromNode, {unsubscribe, _MailBox, []}) ->
	reply(From, ok);

handle(From, FromNode, {unsubscribe, MailBox, Bus}) ->
	tools:msg("unsubscribe: node[~p] bus[~p]", [FromNode, Bus]),	
	mng:rem_sub(Bus, {FromNode, MailBox}),

	%% Notification on "system bus"
	send(mswitch, {system, {unsubscribe, From, Bus, MailBox}}),
	
	reply(From, ok);


%% API - PUBLISH
%% =============
%%
%% @private
handle(From, FromNode, {publish, Bus, Message}) ->
	send(FromNode, {Bus, Message}),
	reply(From, ok);


%% API - GETBUSSES
%% ===============
%%
%% @private
handle(From, _FromNode, getbusses) ->
	Busses=mng:getbusses(),
	reply(From, {busses, Busses});


%% API - GETNODES
%% ===============
%%
%% @private
handle(From, _FromNode, getnodes) ->
	Nodes=mng:getnodes(),
	reply(From, {nodes, Nodes});


%% API - GETSUBS
%% ===============
%%
%% @private
handle(From, _FromNode, {getsubs, Bus}) ->
	Subs=mng:getsubs(Bus),
	reply(From, {subs, Subs});


handle(From, FromNode, Message) ->
	tools:msg("handle: unsupported api[~p] Caller[~p]", [Message, FromNode]),
	send(mswitch, {system, {invalid_api, From, FromNode, Message}}),
	reply(From, {error, {invalid_api, Message}}).


%% rpc reply mechanism
%% @private
reply(To, Message) ->
	To ! {reply, self(), Message}.


%% Sends a Message on a Bus
%%
%% @private
send(FromNode, {Bus, Message}) ->
	Subscribers=mng:getsubs(Bus),
	send(FromNode, Subscribers, Message).


%% @private
send(_FromNode, [], _Message) -> 
	no_subs;

send(FromNode, Subs, Message) -> 
	dosend(FromNode, Subs, Message).


%% @private
dosend(_FromNode, [], _Message) ->
	no_more_subs;

dosend(FromNode, [Sub|Rest], Message) ->
	[MB]=mng:getsubmailbox(Sub),
	sendto(FromNode, {Sub, MB}, Message),
	dosend(FromNode, Rest, Message).


%% @private
%% don't send to self!
sendto(X, {X, {_,_,_}}, _Message) ->
	skip_self;

sendto(FromNode, To, Message) ->
	%% extract mailbox parameters
	{DestNode, {Module, Function, Server}} = To,
	tools:msg("sendto: Dst[~p] Mod[~p] Func[~p] Msg[~p]", [DestNode, Module, Function, Message]),
	
	try rpc:call(DestNode, Module, Function, [{FromNode, Server, Message}]) of
		
		%% Subscriber probably disappeared...
		{badrpc, _Reason} ->
			mng:delete_node(DestNode),
			
			%% system bus notification
			send(mswitch, {system, {delete_node, DestNode}}),
			{removed_sub, DestNode};
	
		Other ->
			Other			
				 
	catch 
		_:_ ->
			mng:delete_node(DestNode),
			
			%% system bus notification
			send(mswitch, {system, {delete_node, DestNode}}),
			{removed_sub, DestNode}
	end.



%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% DAEMON API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


%% @private
daemon_api(status) ->
	{pid, os:getpid()};

daemon_api(getnodes) ->
	{_, Message} = call(node(), getnodes),
	Message;

daemon_api(getbusses) ->
	{_, Message} = call(node(), getbusses),
	Message;

daemon_api(_) ->
	{error, invalid_command}.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------


%% Prioritize mswitch's code path
%%
%% @private
set_code_path() ->
	AbsPath=code:which(mswitch),
	Dir=filename:dirname(AbsPath),
	code:add_patha(Dir).

