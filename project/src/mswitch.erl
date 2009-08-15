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
		 loop/1,
		 rpc/1,
		 handle/3,
		 handle/4,
		 send/3
		 ]).
%%
%% API Functions
%%
start() ->
	start([]).

start(Params) ->
	Pid = spawn_link(?MODULE, loop, [Params]),
	register(?SERVER, Pid),
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


loop(Params) ->
	receive
		stop ->
			exit(ok);
		
		{From, {publish, Bus, Message}} ->
			handle(From, publish, Bus, Message);
		
		{From, {subscribe, Bus}} ->
			handle(From, subscribe, Bus);
		
		{From, {unsubscribe, Bus}} ->
			handle(From, unsubscribe, Bus)

	end,
	loop(Params).



handle(From, subscribe, Bus) ->
	Subscribers=getvar({subscribers, Bus}, []),
	Filtered=Subscribers -- From,
	NewList=Filtered ++ From,
	put({subscribers, Bus}, NewList),
	ok;


handle(From, unsubscribe, Bus) ->
	Subscribers=getvar({subscribers, Bus}, []),
	Filtered=Subscribers -- From,
	put({subscribers, Bus}, Filtered),
	ok.
	

handle(From, publish, Bus, Message) ->
	Subscribers=getvar({subscribers, Bus}, []),
	send(From, Subscribers, Message).



send(_From, [], _Message) ->
	ok;

send(From, Subscribers, Message) ->
	[Current|Rest] = Subscribers,
	Result = sendto(From, Current, Message),
	send(From, Rest, Message).


sendto(From, To, Message) ->
	Result = To ! {From, Message},
	case Result of
		{From, Message} ->
			ok;
		_ ->
			error
	end.


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
