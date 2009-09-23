%% Author: Jean-Lou Dupont
%% Created: 2009-08-18
%%
%% NOTE: start with a unique short-name for
%%       each instance e.g. erl -sname test1
%%                          erl -sname test2
-module(mswitch_test).

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
	start(test, [system, notif, event]).

start(Server, Busses) ->
	Pid=spawn_link(?MODULE, loop, []),
	register(Server, Pid),
	Pid ! {params, Server, Busses},
	{ok, Pid}.


inbox({FromNode, Server, Bus, Message}) ->
	%%io:format("inbox, message[~p]~n",[Message]),
	Server ! {FromNode, Bus, Message}.




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
		
		{_From, Bus, {count, Count}} ->
			io:format("Bus[~p] Rx: ~p~n", [Bus, Count]);
		
		Other ->
			io:format("~p: unhandled message [~p]~n", [?MODULE, Other])
		
	after ?TIMEOUT ->

		Count=pvadd(count,1),
		Ret=mswitch:publish(notif, {count, "Count: "++erlang:integer_to_list(Count)})
		%io:format("Ret: ~p~n", [Ret])
		
	end,
	loop().


subscribe(Server, Busses) ->
	io:format("test: subscribe [~p]~n",[Busses]),
	mswitch:subscribe({?MODULE, inbox, Server}, Busses).


add(undefined, Value) ->
	Value;

add(Var, Value) ->
	Var + Value.



pvadd(Var, Value) ->
	Count=get(Var),
	NewCount = add(Count, Value),
	put(Var, NewCount),
	NewCount.
	

	