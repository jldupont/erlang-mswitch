%% Author: Jean-Lou Dupont
%% Created: 2009-09-17
%% Description: MSWITCH bridge
%%
%% @doc
%%
%%  = Commands =
%%  /sub                 Display current list subscriptions  
%%  /sub X: b1 b2 ...    Subscribe to busses [b1, b2, ...] and save as list X
%%  /del X               Unsubscribe & delete list X 
%%	/sel                 Display current active list
%%	/sel X               Select list X as current
%%
%%  = Data Model =
%%
%%	{userlists, User}       -> [Lists]
%%	{userlist,  User, List}	-> [Busses]
%%	{selection, User}       -> List
%%
-module(mod_mswitch).

-behaviour(gen_server).

%% NOTE: a warning might appear here as erlIDE isn't fully featured yet.
%%       It is not possible to configure librairy paths currently.
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").


%% API
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {host}).

-define(PROCNAME,       ejabberd_mod_mswitch).
-define(MSWITCH,        mswitch).
-define(MSWITCH_TOOLS,  mswitch_tools).
-define(LOG,            log).
-define(TOOLS,          mswitch_mod_tools).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
	%?LOG(host, "Host: ~p", [Host]),
	?INFO_MSG("mod_mswitch Host: ~p", [Host]),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).


start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [Host, Opts]},
	 temporary,
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    MyHost = gen_mod:get_opt_host(Host, Opts, "mswitch.@HOST@"),
    ejabberd_router:register_route(MyHost),
	%?LOG(init, "MyHost: ~p", [MyHost]),
    {ok, #state{host = MyHost}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet}, State) ->
	route(From, To, Packet),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% ROUTING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------



%safe_route(From, To, Packet) ->
	%?LOG(safe_route, "From: ~p To: ~p", [From, To]).
%	?MSWITCH:publish(debug, {mod_mswitch, From, To, Packet}).




%% ----------------------     ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------     ------------------------------

log(Context, Msg, Params) when is_atom(Context), is_list(Msg) ->
	List=?MSWITCH_TOOLS:make_list(Params),
	MessageFormat=erlang:atom_to_list(Context) ++ Msg,
	Message=io_lib:format(MessageFormat, List),
	?INFO_MSG("mod_mswitch MESSAGE: ~p", [Message]),	
	Ret=?MSWITCH:publish(debug, {Context, Message}),
	?INFO_MSG("from mswitch:publish: ~p", [Ret]).
	%?INFO_MSG("mod_mswitch: node info: ~p", [node()]).


	
%% @TODO filter/block
%%
route(From, To, Packet) ->
    safe_route(From, To, Packet).



safe_route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end.
 
do_route(#jid{lserver = FromServer} = From,
	 #jid{lserver = ToServer} = To,
	 {xmlelement, "presence", _, _})
  when FromServer == ToServer ->
    %% Break tight loops by ignoring these presence packets.
    ?WARNING_MSG("Tight presence loop between~n~p and~n~p~nbroken.",[From, To]),
    ok;

do_route(From, To, {xmlelement, "presence", _, _} = Packet) ->

    case xml:get_tag_attr_s("type", Packet) of
	"subscribe" ->
	    ?TOOLS:send_presence(To, From, "subscribe");
	"subscribed" ->
	    sub(To, From),
	    ?TOOLS:send_presence(To, From, "subscribed"),
	    ?TOOLS:send_presence(To, From, "");
	"unsubscribe" ->
	    unsub(To, From),
	    ?TOOLS:send_presence(To, From, "unsubscribed"),
	    ?TOOLS:send_presence(To, From, "unsubscribe");
	"unsubscribed" ->
	    unsub(To, From),
	    ?TOOLS:send_presence(To, From, "unsubscribed");
 
	"" ->
	    ?TOOLS:send_presence(To, From, ""),
	    ?TOOLS:start_consumer(From, To#jid.lserver, ?TOOLS:extract_priority(Packet));
	"unavailable" ->
	    ?TOOLS:stop_consumer(From);
 
	"probe" ->
	    ?TOOLS:send_presence(To, From, "");
 
	_Other ->
	    ?INFO_MSG("Other kind of presence~n~p", [Packet])
    end,
    ok;

do_route(From, To, {xmlelement, "message", _, _} = Packet) ->
    case xml:get_subtag_cdata(Packet, "body") of
	"" ->
	    empty;
	Body0 ->
	    Body = strip_bom(Body0),
	    case xml:get_tag_attr_s("type", Packet) of
		"error" ->
		    ?ERROR_MSG("Received error message~n~p -> ~p~n~p", [From, To, Packet]),
			error;
		_ ->
		    send_command_reply(To, From, do_command(To, From, Body, parse_command(Body)))
	    end
    end,
    ok;
do_route(_From, _To, _Packet) ->
    ?INFO_MSG("**** DROPPED~n~p~n~p~n~p", [_From, _To, _Packet]),
    ok.

 

strip_bom([239,187,191|C]) -> C;
strip_bom(C) -> C.


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
 






sub(To, From) ->
	ok.

unsub(To, From) ->
	ok.

