%% Author: Jean-Lou Dupont
%% Created: 2009-09-17
%% Description:
%%
-module(mswitch_mod_tools).
-compile(export_all).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(MSWITCH,        mswitch).
-define(MSWITCH_TOOLS,  mswitch_tools).
-define(LOG,            log).

-record(mod_mswitch_userlists, {user, lists}).
-record(mod_mswitch_userlist,  {user, list, busses}).
-record(mod_mswitch_selection, {user, selection}).
-record(mod_mswitch_consumers, {user, pid}).



send_presence(From, To, "") ->
    ?LOG(send_presence, "Sending sub reply of type ((available))~n~p -> ~p", [From, To]),
    ejabberd_router:route(From, To, {xmlelement, "presence", [], []});
send_presence(From, To, TypeStr) ->
    ?LOG(send_presence, "Sending sub reply of type ~p~n~p -> ~p", [TypeStr, From, To]),
    ejabberd_router:route(From, To, {xmlelement, "presence", [{"type", TypeStr}], []}).



%% @doc Create the necessary tables
%%
create_tables() ->
	
	try
	Ret1=mnesia:create_table(mod_mswitch_userlists,
			[{attributes, record_info(fields, mod_mswitch_userlists)},
			{disc_copies, [node()]}]),

	Ret2=mnesia:create_table(mod_mswitch_userlist,
			[{attributes, record_info(fields, mod_mswitch_userlist)},
			{disc_copies, [node()]}]),
	
	Ret3=mnesia:create_table(mod_mswitch_selection,
			[{attributes, record_info(fields, mod_mswitch_selection)},
			{disc_copies, [node()]}]),

	Ret4=mnesia:create_table(mod_mswitch_consumers,
			[{attributes, record_info(fields, mod_mswitch_consumers)}
			]),
	
	process_results([Ret1, Ret2, Ret3, Ret4])
	
	catch X:Y -> 
		{error, {X, Y}}
	end.




process_results([]) -> ok;
process_results([{atomic, ok}|T]) -> process_results(T);
process_results([{aborted, {already_exists,_}}|T]) -> process_results(T);
process_results(Error) -> {error, Error}.


%% ----------------------     ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% GET %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------     ------------------------------


%% @doc Retrieve current selection for user
%%
%% @spec get_selection(UserJid) -> List | undefined
%% where
%%	UserJid=JID()
%%	List=atom()
get_selection(UserJid) ->
	SJID=short_jid(UserJid),
	
	Ret=mnesia:transaction(
      fun () ->
	      case mnesia:read({mod_mswitch_selection, SJID}) of
			     [Selection] -> Selection;
			     [] -> default
			 end
      end),
	case Ret of
		{atomic, {mod_mswitch_selection, _,Result}} -> Result;
		_ -> default
	end.

%% @doc Retrieve the lists associated with User
%%
%% @spec get_lists(UserJid) -> Lists | undefined
%%
get_lists(UserJid) ->
	SJID=short_jid(UserJid),
	
	Ret=mnesia:transaction(
      fun () ->
	      case mnesia:read({mod_mswitch_userlists, SJID}) of
			     [Lists] -> Lists;
			     [] -> undefined
			 end
      end),
	case Ret of
		{atomic, {mod_mswitch_userlists, _, Result}} -> Result;
		_ -> undefined
	end.
	

get_busses(UserJid, List) ->
	SJID=short_jid(UserJid),
	
	Ret=mnesia:transaction(
      fun () ->
	      case mnesia:read({mod_mswitch_userlist, SJID, List}) of
			     [List] -> List;
			     [] -> undefined
			 end
      end),
	case Ret of
		{atomic, {mod_mswitch_userlist, _, _, Result}} -> Result;
		_ -> undefined
	end.


get_pid(UserJid) ->
	SJID=short_jid(UserJid),
	
	Ret=mnesia:transaction(
      fun () ->
	      case mnesia:read({mod_mswitch_consumers, SJID}) of
			     [Pid] -> Pid;
			     [] -> undefined
			 end
      end),
	case Ret of
		{atomic, {mod_mswitch_consumers, _, Result}} -> Result;
		_ -> undefined
	end.


%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% CACHED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% GET    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------

cget(selection, UserJID) ->
	do_cget({selection, UserJID}, get_selection, [UserJID]);

cget(lists,  UserJID) ->
	do_cget({lists, UserJID}, get_lists, [UserJID]);	

cget(busses, {UserJID, List}) ->
	do_cget({busses, UserJID, List}, get_busses, [UserJID, List]);

cget(U, _) ->
	?CRITICAL_MSG("MOD_MSWITCH: cget: unknown ~p", [U]).

do_cget(Var, Func, Params) ->
	Value=get(Var),
	case Value of 
		undefined ->
			Ret=apply(?MODULE, Func, Params),
			put(Var, Ret),
			Ret;
		Value ->
			Value
	end.





%% ----------------------     ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SET %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------     ------------------------------


set_selection(UserJid, Selection) ->
	SJID=short_jid(UserJid),
	
	?INFO_MSG("set_selection: user: ~p  selection: ~p", [SJID, Selection]),
	mnesia:transaction(
		fun() ->
		  	mnesia:write(#mod_mswitch_selection{user= SJID, selection= Selection})
		end
	),
	put({selection, SJID}, Selection).


set_lists(UserJid, Lists) ->
	SJID=short_jid(UserJid),
	
	?INFO_MSG("set_lists: user: ~p  lists: ~p", [SJID, Lists]),
	mnesia:transaction(
		fun() ->
		  	mnesia:write(#mod_mswitch_userlists{user= SJID, lists= Lists})
		end
	),
	put({lists, SJID}, Lists).

set_list(UserJid, List, Busses) ->
	SJID=short_jid(UserJid),
	?INFO_MSG("set_list: user: ~p  list: ~p  busses: ~p", [SJID, List, Busses]),
	mnesia:transaction(
		fun() ->
		  	mnesia:write(#mod_mswitch_userlist{user= SJID, list= List, busses=Busses})
		end
	),
	put({busses, SJID, List}, Busses).

set_pid(UserJid, Pid) ->
	%SJID=short_jid(UserJid),
	?INFO_MSG("set_pid: user: ~p  pid: ~p", [UserJid, Pid]),
	mnesia:transaction(
		fun() ->
		  	mnesia:write(#mod_mswitch_consumers{user= UserJid, pid=Pid})
		end
	),
	put({consumer.pid, UserJid}, Pid).

%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% CONSUMER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------


start_consumer(UserJid, Server, Priority) ->
		
	case mnesia:transaction(
		fun () ->
			case mnesia:read({mod_switch_consumers, user=UserJid}) of
				[#mod_mswitch_consumers{user=UserJid, pid = Pid}] ->
					?INFO_MSG("Existing consumer, Pid: ~p", [Pid]),
					{existing, Pid};
				[] ->
					do_start_consumer(UserJid, Server, Priority);
				_ ->
					do_start_consumer(UserJid, Server, Priority)					
			
			end
		end) of
			{atomic, {new, _Pid}} ->
				ok;
			{atomic, {existing, Pid}} ->
				Pid ! {presence, UserJid, Priority},
				ok;
		
			{aborted, {no_exists, _}} ->
				% should only occur the first time around
				do_start_consumer(UserJid, Server, Priority);

			Other ->
				?ERROR_MSG("start_consumer, Other<~p>", [Other])
	end.
 
do_start_consumer(UserJid, Server, Priority) ->
	Pid = spawn(get_consumer_server(UserJid, Server, Priority)),
	Pid ! start,
	?INFO_MSG("Starting consumer, Pid: ~p", [Pid]),
	set_pid(UserJid, Pid),
	set_consumer_pid(UserJid, Pid),
	{new, Pid}.

	

stop_consumer(UserJid) ->
	
	mnesia:transaction(
		fun () ->
			case mnesia:read({mod_mswitch_consumers, UserJid}) of
				[#mod_mswitch_consumers{user=UserJid, pid = Pid}] ->
					Pid ! unavailable,
					ok;
				[] ->
					ok;
				_ ->
					ok
			end
		end),
	ok.




 


set_consumer_pid(UserJid, Pid) ->
	put({consumer.pid, UserJid}, Pid).



get_consumer_pid(UserJid) ->
	Pid=get({consumer.pid, UserJid}),
	get_consumer_pid(UserJid, Pid).

get_consumer_pid(UserJid, undefined) ->
	Pid=get_pid(UserJid),
	put({consumer.pid, UserJid}, Pid),
	Pid;

get_consumer_pid(_UserJid, Pid) ->
	Pid.



%% @hidden
consumer_init(UserJID, Server, Priority) ->
	?INFO_MSG("**** starting consumer for jid: ~p", [UserJID]),
    SelfPid = self(),
    spawn_link(fun () ->
				erlang:monitor(process, SelfPid),
				wait_for_death()
				end),
    
	consumer_server(UserJID, Server, Priority).



wait_for_death() ->
	receive
		{'DOWN', _Ref, process, _Pid, _Reason} ->
			done;
		Other ->
			exit({wait_for_death, unexpected, Other})
	end.



get_consumer_server(UserJID, Server, Priority) ->
	fun() ->
		consumer_server(UserJID, Server, Priority)
	end.


%% NOTE: UserJID must be a 'real' JID  i.e. not a "string" version
%%
consumer_server(UserJID, Server, Priority) ->
	receive
		start ->
			?INFO_MSG("Consumer started, Pid: ~p", [self()]),
			?MSWITCH:publish(debug, {mod_mswitch, consumer.started, UserJID}),
			consumer_init(UserJID, Server, Priority);
		
		reload ->
			do_reload(self(), UserJID),
			?INFO_MSG("got reload!", []);
		
		{presence, UserJID, Priority} ->
			noop;
		
		{busses, Busses} ->
			?MSWITCH:subscribe(Busses);
		
		{mswitch, Bus, Msg} ->
			handle_mswitch(UserJID, Bus, Msg);
		
		unavailable ->
			set_pid(UserJID, undefined),
			exit(normal);
	
		Other ->
			?INFO_MSG("Unsupported message: ~p", [Other])
			
	end,
	consumer_server(UserJID, Server, Priority).


do_reload(ServerPid, User) ->
	ListSel=get_selection(User),
	Busses=get_busses(User, ListSel),
	maybe_subscribe(User, ServerPid, ListSel, Busses).
	

maybe_subscribe(User, _ServerPid, List, undefined) ->
	?INFO_MSG("No busses for user: ~p selection: ~p", [User, List]);
	
maybe_subscribe(User, ServerPid, _List, Busses) when is_list(Busses) ->
	?INFO_MSG("Subscribing user: ~p to busses: ~p", [User, Busses]),
	?MSWITCH:subscribe(ServerPid, Busses);

maybe_subscribe(User, _ServerPid, List, Busses) ->
	?ERROR_MSG("maybe_subscribe: exception: User: ~p  List: ~p  Busses: ~p", [User, List, Busses]).


handle_mswitch(UserJID, Bus, Msg) ->
	?INFO_MSG("mswitch rx: Bus: ~p  Msg: ~p", [Bus, Msg]),
	ok.




%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SUB/UNSUB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------



sub(_ThisBot, _UserJID) ->
	ok.

unsub(_ThisBot, UserJID) ->
	stop_consumer(UserJID).



%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% MISC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------



short_jid(Jid) ->
	RFjid=jlib:jid_remove_resource(Jid),
	jlib:jid_to_string(RFjid).



extract_priority(Packet) ->
    case xml:get_subtag_cdata(Packet, "priority") of
	"" ->
	    0;
	S ->
	    list_to_integer(S)
    end.



%% ----------------------     ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------     ------------------------------

log(Context, Msg, Params) when is_atom(Context), is_list(Msg) ->
	List=?MSWITCH_TOOLS:make_list(Params),
	MessageFormat=erlang:atom_to_list(Context) ++ ": "++Msg,
	Message= try
		lists:flatten( io_lib:format(MessageFormat, List) )
	catch
		_:_ ->
		io_lib:format(MessageFormat, List)
	end,
	Stripped=string:strip(Message, both, $"),
	%?INFO_MSG("mod_mswitch MESSAGE: ~p", [Message]),	
	?MSWITCH:publish(debug, {Context, Stripped}).
	%?INFO_MSG("from mswitch:publish: ~p", [Ret]).
	%?INFO_MSG("mod_mswitch: node info: ~p", [node()]).

