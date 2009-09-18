%% Author: Jean-Lou Dupont
%% Created: 2009-09-17
%% Description: 
-module(mswitch_mod_tools).
-compile(export_all).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(mod_mswitch_userlists, {user, lists}).
-record(mod_mswitch_userlist,  {user, list, busses}).
-record(mod_mswitch_selection, {user, selection}).



send_presence(From, To, "") ->
    ?DEBUG("Sending sub reply of type ((available))~n~p -> ~p", [From, To]),
    ejabberd_router:route(From, To, {xmlelement, "presence", [], []});
send_presence(From, To, TypeStr) ->
    ?DEBUG("Sending sub reply of type ~p~n~p -> ~p", [TypeStr, From, To]),
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

	process_results([Ret1, Ret2, Ret3]).



process_results([]) -> ok;
process_results([{atomic, ok}|T]) -> process_results(T);
process_results(_) -> error.


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
	


update_selection(UserJid, Selection) ->
	mnesia:transaction(
		fun() ->
		  	mnesia:write(#mod_mswitch_selection{user= UserJid, selection= Selection})
		end
	).



