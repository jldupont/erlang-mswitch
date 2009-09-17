%% Author: Jean-Lou Dupont
%% Created: 2009-09-17
%% Description: 
-module(mswitch_mod_tools).
-compile(export_all).

-include("ejabberd.hrl").
-include("jlib.hrl").



send_presence(From, To, "") ->
    ?DEBUG("Sending sub reply of type ((available))~n~p -> ~p", [From, To]),
    ejabberd_router:route(From, To, {xmlelement, "presence", [], []});
send_presence(From, To, TypeStr) ->
    ?DEBUG("Sending sub reply of type ~p~n~p -> ~p", [TypeStr, From, To]),
    ejabberd_router:route(From, To, {xmlelement, "presence", [{"type", TypeStr}], []}).
