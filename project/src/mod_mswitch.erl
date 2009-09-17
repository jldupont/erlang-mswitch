%% Author: Jean-Lou Dupont
%% Created: 2009-09-17
%% Description: TODO: Add description to mswitch_mod_mswitch
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

-define(PROCNAME, ejabberd_mod_mswitch).
-define(MSWITCH,        mswitch).
-define(MSWITCH_TOOLS,  mswitch_tools).
-define(LOG,      log).


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
	safe_route(From, To, Packet),
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



safe_route(From, To, _Packet) ->
	?LOG(safe_route, "From: ~p To: ~p", [From, To]).




%% ----------------------     ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------     ------------------------------

log(Context, Msg, _Params) when is_atom(Context), is_list(Msg) ->
	%List=?MSWITCH_TOOLS:make_list(Params),
	%MessageFormat=erlang:atom_to_list(Context) ++ Msg,
	%Message=io_lib:format(MessageFormat, List),
	%?INFO_MSG("mod_mswitch MESSAGE: ~p", [Message]),	
	%Ret=?MSWITCH:publish(debug, from_mod_mswitch),
	%?INFO_MSG("from mswitch:publish: ~p", Ret).
	?INFO_MSG("mod_mswitch: node info: ~p", [node()]).


	
	
