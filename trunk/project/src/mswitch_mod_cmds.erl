%% Author: Jean-Lou Dupont
%% Created: 2009-09-18
%% Description: 
-module(mswitch_mod_cmds).
-compile(export_all).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(TOOLS, mswitch_mod_tools).
-define(LOG,   ?TOOLS:log).

-define(CMDS, ["/", "/sub", "/add", "/del", "/sel" ]).


get_cmds() ->
	?CMDS.


handle_message(ThisBot, From, Body) ->
	Stripped=string:strip(Body),
	Tokens=string:tokens(Stripped, " "),
	dispatch_cmd(ThisBot, From, Tokens).


dispatch_cmd(_, _, []) ->
	ok;

dispatch_cmd(ThisBot, User, [$/|_T]) ->
	send_command_reply(ThisBot, User, {ok, "Commands: ~p", get_cmds()});

dispatch_cmd(ThisBot, User, ["/sub"|Rest]) ->
	{Status, Msg, Params}=do_sub(Rest),
	send_command_reply(ThisBot, User, {Status, Msg, Params});

dispatch_cmd(ThisBot, User, ["/add"|Rest]) ->
	{Status, Msg, Params}=do_add(Rest),
	send_command_reply(ThisBot, User, {Status, Msg, Params});

dispatch_cmd(ThisBot, User, ["/del"|Rest]) ->
	{Status, Msg, Params}=do_del(Rest),
	send_command_reply(ThisBot, User, {Status, Msg, Params});

dispatch_cmd(ThisBot, User, ["/sel"|Rest]) ->
	{Status, Msg, Params}=do_sel(Rest),
	send_command_reply(ThisBot, User, {Status, Msg, Params});

dispatch_cmd(ThisBot, User, _) ->
	send_command_reply(ThisBot, User, {error, "Eh?"}).



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
    ?LOG(msg, "Delivering ~p -> ~p~n~p", [From, To, XmlBody]),
    ejabberd_router:route(From, To, XmlBody).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HANDLERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

do_sub(Params) ->
	ok.

do_add(Params) ->
	ok.

do_del(Params) ->
	ok.

do_sel(Params) ->
	ok.
