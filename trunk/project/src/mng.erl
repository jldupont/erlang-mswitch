%% Author: Jean-Lou Dupont
%% Created: 2009-08-17
%% Description: Management functions
-module(mng).

%%
%% MACROS
%%


%%
%% Exported Functions
%%
-export([
		 getlocalsubs/0,
		 getsubs/1,
		 getbusses/0,
		 add_sub_local/2, rem_sub_local/2,
		 add_sub/2, rem_sub/2,
		 delete_sub/1,
		 remove_sub_from_busses/2,
		 clean_bus_table/0,
		 clean_busses/1
		 ]).

%%
%% Local Functions
%%

getlocalsubs() ->
	tools:getvar({mswitch, local, busses}, {[],{}}).
	

%% @private
getsubs(Bus) ->
	tools:getvar({mswitch, subscribers, Bus}, []).

getbusses() ->
	tools:getvar({mswitch, subs, self()}, []).



%% Add a Subscriber to a Bus
%% Local state used for sync procedure
%%
%% @private
add_sub_local(Bus, MailBox) ->
	tools:add_to_var({mswitch, local, busses}, {Bus, MailBox}),
	ok.

%% Remove a Subscriber from a Bus
%% Local state used for sync procedure
%%
%% @private
rem_sub_local(Bus, MailBox) ->
	tools:rem_from_var({mswitch, local, busses}, {Bus, MailBox}),
	ok.



%% Add a Subscriber to a Bus
%%
%% Sub= {Node, MailBox}
%%
%% @private
add_sub(Bus, {Node, MailBox}) ->
	tools:add_to_var({mswitch, busses}, Bus),
	tools:add_to_var({mswitch, node, Node}, MailBox),
	tools:add_to_var({mswitch, subs, Bus}, Node),
	ok.
	
%% Remove a Subscriber from a Bus
%%
%% @private
rem_sub(Bus, Node) ->
	tools:rem_from_var({mswitch, subs, Bus}, Node),
	clean_node_table(),
	clean_bus_table(),
	ok.
	


%% Remove a subscriber from the tables
%% @private
delete_sub(Node) ->
	mng:msg("Deleting sub node[~p]", [Node]),
	
	%% delete mailbox
	erase({mswitch, node, Node}),
	
	%% delete from busses
	Busses=tools:getvar({mswitch, busses}, []),
	remove_sub_from_busses(Busses, Node),
	clean_bus_table().



%% @private
remove_sub_from_busses([], _) ->
	ok;

remove_sub_from_busses([H|T], Node) ->
	rem_sub(H, Node),
	remove_sub_from_busses(T, Node);

remove_sub_from_busses(Bus, Node) ->
	rem_sub(Bus, Node),
	ok.


%% Go through all busses
%%  and remove any invalid entry i.e. non-existing node
%%
%% @private
clean_node_table() ->
	Busses=tools:getvar({mswitch, busses}, []),
	clean_node_table(Busses).

clean_node_table([]) ->
	ok;

clean_node_table([Bus|Rest]) ->
	clean_node_from_bus(Bus),
	clean_node_table(Rest).

clean_node_from_bus(Bus) ->
	Nodes=tools:getvar({mswitch, subs, Bus}, []),
	clean_node_from_bus(Bus, Nodes).

clean_node_from_bus(_, []) ->
	ok;

clean_node_from_bus(Bus, [Node|Rest]) ->
	clean_node(Bus, Node),
	clean_node_from_bus(Bus, Rest).

%% Removes one Node from Bus
clean_node(Bus, Node) ->
	tools:rem_from_var({mswitch, subs, Bus}, Node).

	
	

%% @private
clean_bus_table() ->
	Busses=tools:getvar({mswitch, busses}, []),
	clean_busses(Busses).

%% @private
clean_busses([]) ->
	no_busses;

clean_busses([Bus|T]) ->
	Nodes=tools:getvar({mswitch, subs, Bus}, []),
	clean_bus(Bus, Nodes),
	clean_busses(T).

%% If empty, remove bus from bus table
%% @private
clean_bus(Bus, []) ->
	mng:msg("deleting bus: ~p", [Bus]),
	tools:rem_from_var({mswitch, busses}, Bus),
	{deleted_bus, Bus};

clean_bus(_Bus, _) ->
	bus_has_subscribers.




