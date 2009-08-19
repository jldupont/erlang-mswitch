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
		 getsubs/1, getsubs/2,
		 getsubmailbox/1, getsubmailbox/2,
		 getbusses/0, getbusses/1,
		 getnodes/0,  getnodes/1,
		 
		 erase_bus/1, erase_bus/2,
		 erase_sub/2, erase_sub/3,
		 erase_node_mailbox/1, erase_node_mailbox/2,
		 erase_node/1, erase_node/2, 
		 
		 add_sub/2,  add_sub/3,
		 rem_sub/2,  rem_sub/3,
		 
		 delete_node/1, delete_node/2
		 ]).

-export([
		 test/0
		 ]).
%%
%% Local Functions
%%

%% @private
getsubs(Bus) -> getsubs(daemon, Bus).
getsubs(Context, Bus) -> tools:getvar({mswitch, Context, subs, Bus}, []).

getsubmailbox(Sub) -> getsubmailbox(daemon, Sub).
getsubmailbox(Context, Sub) -> tools:getvar({mswitch, Context, node, Sub}).

getbusses() -> getbusses(daemon).
getbusses(Context) -> tools:getvar({mswitch, Context, busses}, []).


getnodes() -> getnodes(daemon).
getnodes(Context) -> tools:getvar({mswitch, Context, nodes}, []).


erase_bus(Bus) -> erase_bus(daemon, Bus).
erase_bus(Context, Bus) -> tools:rem_from_var({mswitch, Context, busses}, Bus).


erase_sub(Bus, Sub) -> erase_sub(daemon, Bus, Sub).
erase_sub(Context, Bus, Sub) ->	tools:rem_from_var({mswitch, Context, subs, Bus}, Sub).


erase_node_mailbox(Node) ->	erase_node_mailbox(daemon, Node).
erase_node_mailbox(Context, Node) -> erase({mswitch, Context, node, Node}).


erase_node(Node) ->	erase_node(daemon, Node).
erase_node(Context, Node) -> tools:rem_from_var({mswitch, Context, nodes}, Node).



%% Add a Subscriber to a Bus
%%
%% Sub= {Node, MailBox}
%%
%% @private
add_sub(Bus, {Node, MailBox}) ->
	add_sub(daemon, Bus, {Node, MailBox}).

add_sub(Context, Bus, {Node, MailBox}) ->
	tools:add_to_var({mswitch, Context, nodes}, Node),
	tools:add_to_var({mswitch, Context, busses}, Bus),
	tools:add_to_var({mswitch, Context, node, Node}, MailBox),
	tools:add_to_var({mswitch, Context, subs, Bus}, Node),
	ok.
	


%% Remove a Subscriber from a Bus
%%
%% @private
rem_sub(Bus, Node) ->
	rem_sub(daemon, Bus, Node).

rem_sub(Context, Bus, Node) ->
	tools:rem_from_var({mswitch, Context, subs, Bus}, Node),
	clean_node_table(Context),
	clean_bus_table(Context),
	ok.


%% Remove a subscriber Node from the all tables
%% @private
delete_node(Node) ->
	delete_node(daemon, Node).
	
delete_node(Context, Node) ->
	mng:msg("Deleting context[~p] node[~p]", [Context, Node]),
	
	%% delete associated mailbox
	erase_node_mailbox(Context, Node),
	
	%% delete from all busses
	remove_node_from_busses(Context, Node),
	clean_bus_table(Context),
	
	%% Finally, remove from the 'all nodes' list
	erase_node(Context, Node).



%% Remove a subscriber Node from all busses
%% @private
remove_node_from_busses(Context, Node) ->
	Busses=getbusses(Context),
	remove_node_from_busses(Context, Busses, Node).

remove_node_from_busses(_Context, [], _) ->
	no_more_bus;

remove_node_from_busses(Context, [Bus|T], Node) ->
	rem_sub(Context, Bus, Node),
	remove_node_from_busses(Context, T, Node);

remove_node_from_busses(Context, Bus, Node) ->
	rem_sub(Context, Bus, Node).


	
%% Go through all busses
%%  and delete any bus without at least 1 subscriber
%%
%% @private
clean_bus_table(Context) ->
	Busses=getbusses(Context),
	clean_busses(Context, Busses).

%% @private
clean_busses(_, []) ->
	no_more_bus;

clean_busses(Context, [Bus|T]) ->
	Nodes=getsubs(Context, Bus),
	clean_bus(Context, Bus, Nodes),
	clean_busses(Context, T).

%% If empty, remove bus from bus table
%% @private
clean_bus(Context, Bus, []) ->
	mng:msg("deleting bus: ~p context[~p]", [Bus, Context]),
	erase_bus(Context, Bus),
	{deleted_bus, Context, Bus};

clean_bus(_, _, _) ->
	bus_has_subscriber.




%% Go through all busses
%%  and remove any invalid entry i.e. non-existing node
%%
%% @private
clean_node_table(Context) ->
	Busses=getbusses(Context),
	clean_node_table(Context, Busses).

clean_node_table(_Context, []) ->
	no_busses;

clean_node_table(Context, [Bus|Rest]) ->
	clean_node_from_bus(Context, Bus),
	clean_node_table(Context, Rest).

%% --

clean_node_from_bus(Context, Bus) ->
	Subs=getsubs(Context, Bus),
	clean_node_from_bus(Context, Bus, Subs).

clean_node_from_bus(_, _Bus, []) ->
	no_sub_on_bus;

clean_node_from_bus(Context, Bus, [Node|Rest]) ->
	clean_node(Context, Bus, Node),
	clean_node_from_bus(Context,Bus, Rest).

%% --

%% Removes one Node from Bus
clean_node(Context, Bus, Node) ->
	MB=getsubmailbox(Context, Node),
	clean_node(Context, Bus, Node, MB).

%% a node without a mailbox... clean!
clean_node(Context, Bus, Node, undefined) ->
	erase_sub(Context, Bus, Node);

clean_node(_Context, _Bus, _Node, _) ->
	node_is_ok.
	



%% ----------------------       ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------       ------------------------------


test() ->
	ok.
	

