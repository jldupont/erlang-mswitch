%% Author: Jean-Lou Dupont
%% Created: 2009-08-18
%% Description: 
-module(test).

%%
%% Macros
%%
-define(CLIENTS, [{client_a, []},{client_b, [bus1]}, {client_c, [bus1, bus2]} ]).

%%
%% Exported Functions
%%
-export([
		 start/0,
		 stop/0,
		 loop/1
		 ]).

%%
%% API Functions
%%
start() ->
	start_clients(?CLIENTS).

start_clients([]) ->
	done;

start_clients([Client|Rest]) ->
	start_client(Client),
	start_clients(Rest).

start_client(Client) ->
	{Name, Busses} = Client,
	Pid=spawn_link(?MODULE, loop, [Name]),
	register(Name, Pid),
	Pid ! {busses, Busses},
	{ok, Pid}.

	
stop() ->
	stop_clients(?CLIENTS).

stop_clients([]) ->
	done;

stop_clients([Client|Rest]) ->
	stop_client(Client),
	stop_clients(Rest).

stop_client(Client) ->
	{Name, _} = Client,
	Name ! stop.



%%
%% Local Functions
%%

loop(Name) ->
	receive
		
		{busses, Busses} ->
			subscribe(Busses);
		
		stop ->
			exit(ok);
		
		Other ->
			io:format("~p: unhandled message [~p]~n", [?MODULE, Other])
		
	end,
	loop(Name).


subscribe([]) ->
	done;

subscribe([Bus|Rest]) ->
	mswitch:subscribe(Bus),
	subscribe(Rest);

subscribe(Bus) ->
	mswitch:subscribe(Bus),
	done.


	
	