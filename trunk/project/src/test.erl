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
		 start/1,
		 loop/0
		 ]).

%%
%% API Functions
%%
start(Busses) ->
	Pid=spawn_link(?MODULE, loop, []),
	Pid ! {busses, Busses},
	{ok, Pid}.




%%
%% Local Functions
%%

loop() ->
	receive
		
		{busses, Busses} ->
			subscribe(Busses);
		
		stop ->
			exit(ok);
		
		Other ->
			io:format("~p: unhandled message [~p]~n", [?MODULE, Other])
		
	after ?TIMEOUT ->

		Count=get(count),
		mswitch:publish(Bus, "Count: "++Count)
		
	end,
	loop().


subscribe([]) ->
	done;

subscribe([Bus|Rest]) ->
	subscribe(Rest),
	mswitch:subscribe(Bus);

subscribe(Bus) ->
	mswitch:subscribe(Bus).




	