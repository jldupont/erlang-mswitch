%% Author: Jean-Lou Dupont
%% Created: 2009-08-18
%%
%% NOTE: start with a unique short-name for
%%       each instance e.g. erl -sname test1
-module(test).

-compile(export_all).

-define(TIMEOUT, 2000).


%%
%% Exported Functions
%%
-export([
		 go/0,
		 start/2,
		 inbox/1,
		 loop/0
		 ]).

%%
%% API Functions
%%
go() ->
	start(test, [notif]).

start(Server, Busses) ->
	Pid=spawn_link(?MODULE, loop, []),
	register(Server, Pid),
	Pid ! {params, Server, Busses},
	{ok, Pid}.


inbox({FromNode, Server, Message}) ->
	%%io:format("inbox, message[~p]~n",[Message]),
	Server ! {FromNode, Message}.




%%
%% Local Functions
%%

loop() ->
	receive
		
		{params, Server, Busses} ->
			io:format("params: server[~p] busses[~p]~n", [Server, Busses]),
			subscribe(Server, Busses);
		
		stop ->
			exit(ok);
		
		{_From, {count, Count}} ->
			io:format("Rx: ~p~n", [Count]);
		
		Other ->
			io:format("~p: unhandled message [~p]~n", [?MODULE, Other])
		
	after ?TIMEOUT ->

		Count=pvadd(count,1),
		Ret=mswitch:publish(notif, {count, "Count: "++erlang:integer_to_list(Count)}),
		io:format("Ret: ~p~n", [Ret])
		
	end,
	loop().


subscribe(_Server, []) ->
	done;

subscribe(Server, [Bus|Rest]) ->
	subscribe(Server, Rest),
	io:format("test: subscribe[~p]~n",[Bus]),
	mswitch:subscribe({test, inbox, Server}, Bus).


add(undefined, Value) ->
	Value;

add(Var, Value) ->
	Var + Value.



pvadd(Var, Value) ->
	Count=get(Var),
	NewCount = test:add(Count, Value),
	put(Var, NewCount),
	NewCount.
	

	