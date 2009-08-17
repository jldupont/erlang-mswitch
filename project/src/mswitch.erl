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
		 start/0,
		 start/1,
		 stop/0
		 ]).

-export([
		 publish/2,
		 subscribe/1,
		 unsubscribe/1
		 ]).

%%
%% Local Functions
%%
-export([
		 loop/0,
		 rpc/1,
		 handle/3,
		 handle/4,
		 send/3,
		 isdebug/0
		 ]).
%%
%% API Functions
%%
start() ->
	start([]).

start(Params) ->
	Pid = spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! {params, Params},
	{ok, Pid}.


stop() ->
	?SERVER ! stop.



%% @spec publish(Bus, Message) -> ok | {error, Reason}
%%
publish(Bus, Message) ->
	rpc({publish, Bus, Message}).

%% @spec subscribe(Bus) -> ok | {error, Reason}
%%
subscribe(Bus) ->
	rpc({subscribe, Bus}).

%% @spec unsubscribe(Bus) -> ok | {error, Reason}
%%
unsubscribe(Bus) ->
	rpc({unsubscribe, Bus}).



%%
%% Local Functions
%%

rpc(Q) ->
	io:format("~p: rpc(~p)~n", [?MODULE, Q]),
	?SERVER ! {self(), Q},
	receive
		{?SERVER, Reply} ->
			Reply;
	
		Other ->
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			{error, rpcerror}
	
	after ?TIMEOUT ->
			
			io:format("~p: rpc timeout~n",[?MODULE]),
			{error, rpcerror}
	end.


loop() ->
	receive
		{params, Params} ->
			put(params, Params);
		
		stop ->
			exit(ok);
		
		{From, {publish, Bus, Message}} ->
			handle(From, publish, Bus, Message);
		
		{From, {subscribe, Bus}} ->
			handle(From, subscribe, Bus);
		
		{From, {unsubscribe, Bus}} ->
			handle(From, unsubscribe, Bus)

	end,
	loop().



handle(From, subscribe, Bus) ->
	Subscribers=getvar({subscribers, Bus}, []),
	Filtered=Subscribers -- From,
	NewList=Filtered ++ From,
	put({subscribers, Bus}, NewList),
	ok;


handle(From, unsubscribe, Bus) ->
	Subscribers=getsubs(Bus),
	Filtered=Subscribers -- From,
	put({subscribers, Bus}, Filtered),
	ok.
	

handle(From, publish, Bus, Message) ->
	Subscribers=getsubs(Bus),
	send(From, Subscribers, Message).



send(_From, [], _Message) ->
	ok;

send(From, Subscribers, Message) ->
	[Current|Rest] = Subscribers,
	sendto(From, Current, Message),
	send(From, Rest, Message).


sendto(From, To, Message) ->
	Result = To ! {From, Message},
	case Result of
		{From, Message} ->
			ok;
		_ ->
			error
	end.



%% @private
tern(Var, Value, True, False) ->
	case Var of
		Value -> True;
		_     -> False
	end.


ltern(List, Var, Value, True, False) ->
	ok.	


%% @private
isdebug() ->
	Params=getvar(params, []),
	lists:member("debug", Params)
	or
	lists:member(debug, Params).



%% @private
getsubs(Bus) ->
	getvar({subscribers, Bus}, []).



%% @spec getvar(VarName, Default) -> Value | Default
%% Value = atom() | string() | integer() | float()
getvar(VarName, Default) ->
	VarValue=get(VarName),
	getvar(VarName, VarValue, Default).

getvar(VarName, undefined, Default) ->
	put(VarName, Default),
	Default;

getvar(_VarName, VarValue, _Default) ->
	VarValue.

