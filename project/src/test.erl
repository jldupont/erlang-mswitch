%% Author: Jean-Lou Dupont
%% Created: 2009-08-18
%% Description: 
-module(test).

-compile(export_all).


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
			subscribe(Name, Busses);
		
		stop ->
			exit(ok);
		
		Other ->
			io:format("~p: unhandled message [~p]~n", [?MODULE, Other])
		
	end,
	loop(Name).


subscribe(_, []) ->
	done;

subscribe(Name, [Bus|Rest]) ->
	subscribe(Name, Rest),
	mswitch:subscribe(Name, {subscribe, Bus});

subscribe(Name, Bus) ->
	mswitch:subscribe(Name, {subscribe, Bus}).




	