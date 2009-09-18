%% Author: Jean-Lou Dupont
%% Created: 2009-09-18
%% Description: 
-module(mswitch_mod_cmds).
-compile(export_all).

-include("ejabberd.hrl").
-include("jlib.hrl").


handle_message(To, From, Body) ->
	ok.



send_command_reply(From, To, {Status, Fmt, Args}) ->
    send_command_reply(From, To, {Status, io_lib:format(Fmt, Args)});
send_command_reply(From, To, {ok, ResponseIoList}) ->
    send_chat(From, To, ResponseIoList);
send_command_reply(From, To, {error, ResponseIoList}) ->
    send_chat(From, To, ResponseIoList);
send_command_reply(_From, _To, noreply) ->
    ok.


send_chat(From, To, {Fmt, Args}) ->
    send_chat(From, To, io_lib:format(Fmt, Args));
send_chat(From, To, IoList) ->
    send_message(From, To, "chat", lists:flatten(IoList)).

send_message(From, To, TypeStr, BodyStr) ->
    XmlBody = {xmlelement, "message",
	       [{"type", TypeStr},
		{"from", jlib:jid_to_string(From)},
		{"to", jlib:jid_to_string(To)}],
	       [{xmlelement, "body", [],
		 [{xmlcdata, BodyStr}]}]},
    ?DEBUG("Delivering ~p -> ~p~n~p", [From, To, XmlBody]),
    ejabberd_router:route(From, To, XmlBody).

