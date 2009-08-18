%% Author: Jean-Lou Dupont
%% Created: 2009-08-18
%% Description: 
-module(test).

-compile(export_all).

-define(TIMEOUT, 2000).


%%
%% Exported Functions
%%
-export([
		 start/2,
		 inbox/1,
		 loop/0
		 ]).

%%
%% API Functions
%%
start(Server, Busses) ->
	Pid=spawn_link(?MODULE, loop, []),
	register(Server, Pid),
	Pid ! {params, Server, Busses},
	{ok, Pid}.


inbox({FromNode, Server, Message}) ->
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
		
		Other ->
			io:format("~p: unhandled message [~p]~n", [?MODULE, Other])
		
	after ?TIMEOUT ->

		Count=get(count),
		mswitch:publish(notif, "Count: "++Count)
		
	end,
	loop().


subscribe(Server, []) ->
	done;

subscribe(Server, [Bus|Rest]) ->
	subscribe(Server, Rest),
	mswitch:subscribe({test, inbox, Server}, Bus);

subscribe(Server, Bus) ->
	mswitch:subscribe(Bus).




	