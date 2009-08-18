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
		 stop/0
		 ]).

-export([
		 publish/2,
		 subscribe/1,
		 unsubscribe/1,
		 getsubs/0
		 ]).

%%
%% Local Functions
%%
-export([
		 loop/0,
		 rpc/1,
		 handle/3, handle/4,
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
publish(Bus, Message) ->
	Reply=rpc({publish, Bus, Message}),
	handleReply(Reply).

%% @spec subscribe(Bus) -> {ServerPid, ok} | {error, Reason}
%%
subscribe(Bus) ->
	%% keep a local context
	mng:add_sub(Bus, self()), 
	rpc({subscribe, Bus}).

%% @spec unsubscribe(Bus) -> {ServerPid, ok} | {error, Reason}
%%
unsubscribe(Bus) ->
	%% keep a local context
	mng:rem_sub(Bus, self()),
	rpc({unsubscribe, Bus}).


getsubs() ->
	rpc(getsubs).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




handleReply({Pid, ok}) ->
	PreviousPid=mng:getvar({mswitch, pid},undefined),
	sync(PreviousPid, Pid);

%% Any other reply, we can't do much...
handleReply(_) ->
	ok.

%% @private
%% Start-up: the client should have subscribed anyways
sync(undefined, _) ->
	ok;

sync(PreviousPid, CurrentPid) when PreviousPid == CurrentPid ->
	ok;

sync(PreviousPid, CurrentPid) when PreviousPid =/= CurrentPid ->
	resubscribe().


resubscribe() ->
	Busses=mng:getbusses(),
	subscribe_to_list(Busses).

subscribe_to_list([]) ->
	ok;

subscribe_to_list([Bus|Rest]) ->
	subscribe(Bus),
	subscribe_to_list(Rest);

subscribe_to_list(Bus) ->
	subscribe(Bus).





%%
%% Local Functions
%%

rpc(Q) ->
	io:format("~p: rpc(~p)~n", [?MODULE, Q]),
	?SERVER ! {self(), Q},
	receive
		{reply, ServerPid, Reply} ->
			{ServerPid, Reply};
	
		Other ->
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			{error, rpcerror}
	
	after ?TIMEOUT ->
			
			io:format("~p: rpc timeout~n",[?MODULE]),
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
		{From, getsubs} -> 					handle(From, getsubs);
		{From, {publish, Bus, Message}} ->	handle(From, publish, Bus, Message);
		{From, {subscribe, Bus}} ->			handle(From, subscribe, Bus);
		{From, {unsubscribe, Bus}} ->		handle(From, unsubscribe, Bus)

	end,
	loop().


%% API - SUBSCRIBE
%%
%% @private
handle(From, getsubs) ->
	Busses=mng:getbusses(),
	reply(From, {busses, Busses}).

handle(From, subscribe, Bus) ->
	mng:add_sub(Bus, From),
	reply(From, ok);

%% API - UN-SUBSCRIBE
%%
%% @private
handle(From, unsubscribe, Bus) ->
	mng:rem_sub(Bus, From),
	reply(From, ok).

%% API - PUBLISH
%%
%% @private
handle(From, publish, Bus, Message) ->
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


