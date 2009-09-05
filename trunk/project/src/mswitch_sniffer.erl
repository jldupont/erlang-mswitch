%% Author: Jean-Lou Dupont
%% Created: 2009-08-20
%% Description: Bus Sniffer
%%
%% NOTE: start with a short-name "mswitch_sniffer"
%%
-module(mswitch_sniffer).

-define(TIMEOUT, 10000).
-define(SERVER, sniffer).
-define(TOOLS,  mswitch_tools).
-define(CLT,    mswitch_clt).

-compile(export_all).


start() ->
	run(undefined, [system]).

start(OptionsList) when is_list(OptionsList) ->
	Parsed=?CLT:parse($^, OptionsList),
	Nodes =?TOOLS:kfind('^n', Parsed),
	Busses=?TOOLS:kfind('^b', Parsed),
	%io:format("Parsed: ~p~n", Parsed),
	run(Nodes, Busses);

start(Bus) when is_atom(Bus) ->
	run(undefined, [Bus]).


run({}, {})                  -> do_run([], [system]);
run({}, {_, Busses})         -> do_run([], Busses);
run({_, Nodes}, {})          -> do_run(Nodes, [system]);
run({_, Nodes}, {_, Busses}) -> do_run(Nodes, Busses).


do_run(Nodes,Busses) ->
	io:format("n<~p> b<~p>~n", [Nodes, Busses]),	
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! {params, ?SERVER, Nodes, Busses},
	{ok, Pid}.


stop() ->
	?SERVER ! stop.


inbox({FromNode, Server, Bus, Message}) ->
	%io:format("inbox: <~p>~n", [Message]),
	Server ! {FromNode, Bus, Message}.



%%
%% Local Functions
%%

loop() ->
	receive
		
		{params, Server, Nodes, Busses} ->
			Ns=format_nodes(Nodes),
			io:format("Nodes <~p>~n", [Ns]),
			put(nodes, Ns),
			subscribe(Server, Busses);
		
		stop ->
			exit(ok);

		{FromNode, Bus, Message} ->
			handle(FromNode, Bus, Message);
		
		Message ->
			io:format("~p: unknown >~p<~n", [?MODULE, Message])
	
	after ?TIMEOUT ->
			
		%% serves as "syncing" procedure mainly
		mswitch:publish(system, {mswitch_sniffer, now()})
	
	end,
	loop().


subscribe(Server, Busses) ->
	mswitch:subscribe({?MODULE, inbox, Server}, Busses).

handle(FromNode, Bus, Message) ->
	Nodes=get(nodes),
	do_handle(Nodes, FromNode, Bus, Message).

do_handle([], FromNode, Bus, Message) ->
	io:format("from<~p> bus<~p> msg<~p>~n", [FromNode, Bus, Message]);


do_handle(Nodes, FromNode, Bus, Message) ->
	%io:format("FromNode<~p>~n", [FromNode]),
	case lists:member(FromNode, Nodes) of
		true ->
			io:format("from<~p> bus<~p> msg<~p>~n", [FromNode, Bus, Message]);
		false ->
			filtered
	end.





format_nodes(Nodes) ->
	do_format_nodes(Nodes, []).

do_format_nodes([], Acc) ->
	Acc;

do_format_nodes([Node|Nodes], Acc) ->
	NN=?TOOLS:make_node(Node),
	do_format_nodes(Nodes, Acc++[NN]).


