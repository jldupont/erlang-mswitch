%% Author: Jean-Lou Dupont
%% Created: 2009-08-20
%% Description: Bus Sniffer
%%
%% NOTE: start with a short-name "mswitch_sniffer"
%%
-module(mswitch_sniffer).

-define(TIMEOUT, 10000).
-define(SERVER, mswitch_sniffer).

-compile(export_all).


start() ->
	start([system]).	

start(Bus) when is_atom(Bus) ->
	start([Bus]);

start(Busses) ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! {params, ?SERVER, Busses},
	{ok, Pid}.

stop() ->
	?SERVER ! stop.


inbox({FromNode, Server, Message}) ->
	Server ! {FromNode, Message}.


%%
%% Local Functions
%%

loop() ->
	receive
		
		{params, Server, Busses} ->
			subscribe(Server, Busses);
		
		stop ->
			exit(ok);
		
		Message ->
			io:format("~p: >~p<~n", [?MODULE, Message])
	
	after ?TIMEOUT ->
			
		%% serves as "syncing" procedure mainly
		mswitch:publish(system, {mswitch_sniffer, now()})
	
	end,
	loop().


subscribe(Server, Busses) ->
	mswitch:subscribe({?SERVER, inbox, Server}, Busses).
