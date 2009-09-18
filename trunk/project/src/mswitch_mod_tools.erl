%% Author: Jean-Lou Dupont
%% Created: 2009-09-17
%% Description: 
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
	
	process_results([Ret1, Ret2, Ret3, Ret4]).



process_results([]) -> ok;
process_results([{atomic, ok}|T]) -> process_results(T);
process_results(_) -> error.


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
	Ret=mnesia:transaction(
      fun () ->
	      case mnesia:read({mod_mswitch_selection, UserJid}) of
			     [Selection] -> Selection;
			     [] -> undefined
			 end
      end),
	case Ret of
		{atomic, Result} -> Result;
		_ -> undefined
	end.

%% @doc Retrieve the lists associated with User
%%
%% @spec get_lists(UserJid) -> Lists | undefined
%%
get_lists(UserJid) ->
	Ret=mnesia:transaction(
      fun () ->
	      case mnesia:read({mod_mswitch_userlists, UserJid}) of
			     [Lists] -> Lists;
			     [] -> undefined
			 end
      end),
	case Ret of
		{atomic, Result} -> Result;
		_ -> undefined
	end.
	

get_busses(UserJid, List) ->
	Ret=mnesia:transaction(
      fun () ->
	      case mnesia:read({mod_mswitch_userlist, UserJid, List}) of
			     [List] -> List;
			     [] -> undefined
			 end
      end),
	case Ret of
		{atomic, Result} -> Result;
		_ -> undefined
	end.


get_pid(UserJid) ->
	Ret=mnesia:transaction(
      fun () ->
	      case mnesia:read({mod_mswitch_consumers, UserJid}) of
			     [Pid] -> Pid;
			     [] -> undefined
			 end
      end),
	case Ret of
		{atomic, Result} -> Result;
		_ -> undefined
	end.


%% ----------------------     ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SET %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------     ------------------------------


set_selection(UserJid, Selection) ->
	mnesia:transaction(
		fun() ->
		  	mnesia:write(#mod_mswitch_selection{user= UserJid, selection= Selection})
		end
	).


set_lists(UserJid, Lists) ->
	mnesia:transaction(
		fun() ->
		  	mnesia:write(#mod_mswitch_userlists{user= UserJid, lists= Lists})
		end
	).

set_list(UserJid, List, Busses) ->
	mnesia:transaction(
		fun() ->
		  	mnesia:write(#mod_mswitch_userlist{user= UserJid, list= List, busses=Busses})
		end
	).

set_pid(UserJid, Pid) ->
	mnesia:transaction(
		fun() ->
		  	mnesia:write(#mod_mswitch_consumers{user= UserJid, pid=Pid})
		end
	).

%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% CONSUMER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------


start_consumer(UserJID, Server, Priority) ->
	case mnesia:transaction(
		fun () ->
			case mnesia:read({mod_switch_consumers, UserJID}) of
				[#mod_mswitch_consumers{pid = Pid}] ->
					{existing, Pid};
				[] ->
				%% TODO: Link into supervisor or something
					Pid = spawn(?MODULE, consumer_init, [UserJID, Server, Priority]),
					mnesia:write(#mod_mswitch_consumers{pid = Pid}),
					set_consumer_pid(UserJID, Pid),
					{new, Pid}
			end
		end) of
			{atomic, {new, _Pid}} ->
				ok;
			{atomic, {existing, Pid}} ->
				Pid ! {presence, UserJID, Priority},
				ok
	end.
 
stop_consumer(UserJID) ->
	mnesia:transaction(
		fun () ->
			case mnesia:read({mod_mswitch_consumers, UserJID}) of
				[#mod_mswitch_consumers{pid = Pid}] ->
					Pid ! unavailable,
					ok;
				[] ->
					ok
			end
		end),
	ok.




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


set_consumer_pid(UserJID, Pid) ->
	put({consumer.pid, UserJID}, Pid).


get_consumer_pid(UserJID) ->
	Pid=get({consumer.pid, UserJID}),
	get_consumer_pid(UserJID, Pid).

get_consumer_pid(UserJID, undefined) ->
	Pid=get_pid(UserJID),
	put({consumer.pid}, Pid),
	Pid;

get_consumer_pid(_UserJID, Pid) ->
	Pid.


	


consumer_server(UserJID, Server, Priority) ->
	receive
		unavailable ->
			set_pid(UserJID, undefined),
			exit(normal);
	
		ok -> ok
	end,
	consumer_server(UserJID, Server, Priority).





%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SUB/UNSUB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------



sub(_ThisBot, UserJID) ->
	ok.

unsub(_ThisBot, UserJID) ->
	stop_consumer(UserJID),
	ok.



%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% MISC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------

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
	MessageFormat=erlang:atom_to_list(Context) ++ Msg,
	Message=io_lib:format(MessageFormat, List),
	%?INFO_MSG("mod_mswitch MESSAGE: ~p", [Message]),	
	?MSWITCH:publish(debug, {Context, Message}).
	%?INFO_MSG("from mswitch:publish: ~p", [Ret]).
	%?INFO_MSG("mod_mswitch: node info: ~p", [node()]).

