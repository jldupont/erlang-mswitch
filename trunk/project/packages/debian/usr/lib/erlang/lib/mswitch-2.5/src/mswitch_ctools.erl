%% Author: Jean-Lou Dupont
%% Created: 2009-09-05
%% Description: Configuration helpers
%%
%% @doc Helper functions for configuring modules from configuration files
%%
%% The main function of this module is 'config' @see config/3.
%%
%%
-module(mswitch_ctools).
-compile(export_all).

-include_lib("kernel/include/file.hrl").

-define(TOOLS,    mswitch_tools).


%% @doc Loads and validates system configuration from file
%%
%% @spec config({LogModule, LogFunction}, Filename, Modules) -> {error, Reason} | {ok, Mtime, Merged}
%% where
%%	LogModule=atom()    
%%	LogFunction=atom()  
%%	Filename=string()
%%	Modules=[atom()]    
%%	Mtime=tuple()       
%%	Merged=[tuple()]    
%%
config({LogModule, LogFunction}, Filename, Modules) when is_atom(LogModule), is_atom(LogFunction),
															is_list(Filename), is_list(Modules) ->
	LogFun={LogModule, LogFunction},
	
	Defaults=get_defaults(LogFun, Modules),
	%io:format("Defaults: ~p~n", [Defaults]),	
	Result=process_config(LogFun, Filename, Modules, Defaults),
	case Result of
		{ok, Mtime, Config} ->
			%io:format("Config: ~p~n", [Config]),
			Merged=merge(LogFun, Defaults, Config),
			{ok, Mtime, Merged};
		_Other ->
			%% Error ? just load the defaults
			Merged=merge(LogFun, Defaults, []),
			{ok, default, Merged}
	end.



%% @private
log({Mod, Fun}, Sev, Msg) ->
	apply(Mod, Fun, [Sev, Msg]).

%% @private	
log({Mod, Fun}, Sev, Msg, Params) ->
	apply(Mod, Fun, [Sev, Msg, Params]).




%% @doc Reads the configuration file
%%
%% @spec read_config(Filename) -> {error, Reason} | {ok, Terms}
%% where 
%%	Filename= string()
%% 	Terms = [tuple()]
%%
read_config(Filename) ->
	case file:consult(Filename) of
		{error, Reason} ->
			{error, Reason};
		{ok, Terms} ->
			{ok, Terms}
	end.




%% @doc Processes configuration from file
%%		The following function loads configuration information from file:
%%		- Read raw terms
%%		- 
%%
%% @spec process_config(LogFun, Filename, Modules, Defaults) -> {error, Reason} | {ok, Mtime, ModuleConfig}
%%
process_config(LogFun, Filename, Modules, Defaults) ->
	case load_config_file(LogFun, Filename) of
		{error, Reason} ->
			% already logged
			{error, Reason};
		
		{ok, Mtime, Config} ->
			
			%io:format("Config file content: ~p~n", [Config]),
			
			%% 1) check format
			List=  do_process_config(LogFun, Config, []),
			Blacklist=get_blacklist(Modules, []),
			
			%% 2) filter on blacklist
			List2= filter_on_blacklist(LogFun, List, Blacklist, []),
			
			%io:format("After blacklist: ~p~n", [List2]),
			
			List3= ?TOOLS:filter_on_patterns(['.min', '.max'], List2, []),
			
			%% 3) check presence of mandatory parameters
			check_mandatory(LogFun,List3, Defaults),
			
			%% 4) type check
			List4=check_type(LogFun, List2, Defaults, []),
			
			List5=filter_entries(List4, []),
			
			%% 5) check limit requirements
			List6=check_limits(LogFun, List5, Defaults, []),
	
			%% Remove unnecessary entries such as {}
			List7=filter_entries(List6, []),
			
			{ok, Mtime, List7}			
	end.

%% @private
do_process_config(_LogFun, [], Acc) ->
	Acc;

%% @private
do_process_config(LogFun, Config, Acc) when is_list(Config) ->
	[Entry|Rest] = Config,
	Fentry = filter_one(LogFun, Entry),
	do_process_config(LogFun, Rest, Acc++[Fentry]).


%% @private
filter_one(_LogFun, {}) -> {};
filter_one(_LogFun, {Key, Value}) when is_atom(Key) -> {Key, Value};
filter_one(LogFun, Inv) -> log(LogFun, error, "config: invalid entry in config file: ", [Inv]).


%% @private
filter_on_blacklist(_LogFun, [], _Blacklist, Acc) -> Acc;

filter_on_blacklist(LogFun, [Entry|Rest], Blacklist, Acc) ->
	Key=get_key(Entry),
	Blacklisted=lists:member(Key, Blacklist),
	case Blacklisted of
		true  -> 
			log(LogFun, warning, "config: blacklisted parameter: ", [Key]),
			filter_on_blacklist(LogFun, Rest, Blacklist, Acc);
		false -> filter_on_blacklist(LogFun, Rest, Blacklist, Acc++[Entry])
	end.
	



%% @private
check_mandatory(_LogFun, _, []) ->
	finished;

%% @doc Go through the Defaults list to verify
%%		the presence of mandatory parameters
%%
check_mandatory(LogFun, List, [Default|Defaults]) ->
	Key=get_key(Default),
	Level=get_level(Default),
	
	case Level of
		mandatory ->
			check_mandatory1(LogFun, List, Key);
		_ ->
			ok
	end,
	check_mandatory(LogFun, List, Defaults).
	

check_mandatory1(LogFun, List, Key) ->
	case find_key_in_config_list(List, Key) of
		{} -> log(LogFun, error, "config: missing mandatory parameter: ", [Key]);
		_  -> ok
	end.


%% @private
check_type(_LogFun, [], _Defaults, Acc) -> Acc;

%% @doc Go through the parameters list
%%		and check each entry type against
%%		the entries in defaults. Filter out
%%		each entry in type mismatch error.
%%
%% @spec check_type(Parameters, Defaults, Acc) -> list()
%%
check_type(LogFun, [ConfigEntry|Rest], Defaults, Acc) ->
	Entry=check_type_one(LogFun, ConfigEntry, Defaults),
	check_type(LogFun,Rest, Defaults, Acc++[Entry]).


check_type_one(LogFun, ConfigEntry, Defaults) ->
	{Key, _Value}=ConfigEntry,
	Dentry=get_entry_from_defaults(Defaults, Key),
	case check_type2(LogFun, Key, ConfigEntry, Dentry) of
		ok -> ConfigEntry;
		_ ->  
			case Dentry of
				no_default -> {};
				_          -> Dentry
			end
	end.


%% @doc Filters a configuration parameter
%%
check_type2(LogFun, Key, _ConfigEntry, {}) ->
	log(LogFun, debug, "config: missing default entry for key: ", [Key]),
	
	%% delete entry from list of valid entries
	no_default;

check_type2(LogFun, Key, {Ckey, Cvalue}, DefaultEntry) ->
	{type, TargetType}=get_type(DefaultEntry),
	{value, DefaultValue}=get_value(DefaultEntry),
	check_type3(LogFun, Key, Ckey, TargetType, Cvalue, DefaultValue).

check_type3(_LogFun, _Key, _Ckey, atom,   Cvalue, _Dvalue) when is_atom(Cvalue)    -> ok;
check_type3(_LogFun, _Key, _Ckey, string, Cvalue, _Dvalue) when is_list(Cvalue)    -> ok;
check_type3(_LogFun, _Key, _Ckey, int,    Cvalue, _Dvalue) when is_integer(Cvalue) -> ok;
check_type3(_LogFun, _Key, _Ckey, float,  Cvalue, _Dvalue) when is_float(Cvalue)   -> ok;
check_type3(LogFun, Key,  _Ckey, nstring,Cvalue, _Dvalue) when is_list(Cvalue) -> 
	case length(Cvalue) of
		0 -> log(LogFun, error, "config: expecting non-zero string for key: ", [Key]),	invalid;
		_ -> ok
	end;

check_type3(LogFun, Key, _Ckey, Type, _Cvalue, _Dvalue) -> 
	log(LogFun, error, "config: expecting Type for Key, {Type, Key}: ", [{Type, Key}]).
		






%% @doc Loads the config file and updates the local variables and config state. 
%%
%% The function loads all the Default configuration parameters first
%% and updates/complements them with the parameters on file.
%%
%% @spec load_config_file(LogFun, Filename) -> {error, Reason} | {ok, Mtime, Config}
%% where
%%	LogFun={LogModule, LogFunction}
%%	Filename=string()
%%
%% @private
load_config_file(LogFun, Filename) ->
	case read_config(Filename) of
		{error, Reason} ->
			log(LogFun, error, "config: error reading configuration file, reason: ", [Reason]),
			{error, Reason};
		
		{ok, Config} ->
			Mtime=get_config_file_mtime(LogFun, Filename),
			{ok, Mtime, Config}
	end.



get_config_file_mtime(LogFun, Filename) ->
	%Filename=config_filename(),
	case file:read_file_info(Filename) of
		{ok, FileInfo} ->
			FileInfo#file_info.mtime;
		{error, Reason} ->
			log(LogFun, error, "config: error getting info from file, {Filename, Reason}", [Filename, Reason]),
			unknown
	end.




%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  DEFAULTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

%% @doc Returns a validated list of defaults
%%		
%% @spec get_defaults(LogFun, Modules) -> [ModuleDefaults]
%% where
%%	Modules=list()
%%	ModuleDefaults = {ModuleName, Defaults}
%%	ModuleName = atom()
%%	Defaults = [Entry]
%%	Entry={Key, Level, Type, Value}
%%	Key = atom()
%%	Level = atom()
%%	Type = atom()
%%	Value = atom() | list() | integer() | float()
%%
get_defaults(LogFun, Modules) ->
	load_defaults(LogFun, Modules, []).
	


load_defaults(_LogFun, [], Acc) ->
	Acc;

%% @doc Load defaults
%%		Go through the modules list and
%%		queries the default values through the
%%		function 'defaults'.
%% @spec load_defaults(Modules) -> void()
%% where
%%	Modules = [atom()]
%%
load_defaults(LogFun, [Module|Modules], Acc) ->
	Server=get_module_server(Module),
	case Server of
		undefined ->
			log(LogFun, debug, "load_defaults: cannot access 'module server name' for module: ", [Module]),
			load_defaults(LogFun, Modules, Acc);
		ServerName ->
			Defaults=try_load_module_defaults(LogFun, Module),
			%io:format("load_defaults: Module[~p] Defaults[~p]~n",[Module, Defaults]),	
			List=try_check_defaults(LogFun, Defaults, []),
			FilteredList=filter_entries(List, []),
			load_defaults(LogFun, Modules, Acc++[{ServerName, FilteredList}])
	end.
	


try_load_module_defaults(LogFun, Module) ->
	try
		erlang:apply(Module, defaults, [])
	catch
		_:_ ->
			log(LogFun, debug, "config: no defaults for module: ", [Module]),
			[]
	end.


try_check_defaults(_, [], []) ->
	[];

try_check_defaults(_, [], List) ->
	List;

try_check_defaults(LogFun, Defaults, List) when is_list(Defaults) ->
	[Default|Rest]=Defaults,
	
	%% Entry format 'envelope' check
	Entry= check1_one_default(LogFun, Default),
	
	%% Entry 'elements' format check
	E1= check2_one_default(LogFun, Entry),
	
	%% type check
	E2= check3_one_default(LogFun, E1),
	try_check_defaults(LogFun, Rest, List++[E2]).


check1_one_default(_, {}) -> {};

check1_one_default(LogFun, Default) when is_tuple(Default) ->
	try
		{Key, Level, Type, Value} = Default,
		{Key, Level, Type, Value}
	catch
		_:_ ->
			log(LogFun, debug, "config: invalid entry format: ", [Default]),
			{}
	end;

check1_one_default(LogFun, Default) ->
	log(LogFun, debug, "config: invalid default entry: ", [Default]),
	{}.



check2_one_default(_, {Key, Level, Type, Value}) when is_atom(Key), is_atom(Level), is_atom(Type) ->
	{Key, Level, Type, Value};

check2_one_default(LogFun, Entry) ->
	log(LogFun, debug, "config: invalid default entry: ", [Entry]),
	{}.



check3_one_default(LogFun, {Key, Level, Type, Value}) when Type==string ->
	case is_list(Value) of
		true ->	{Key, Level, Type, Value};
		false -> log(LogFun, debug, "config: expecting 'string' for Key: ", [Key]), {}
	end;
check3_one_default(LogFun, {Key, Level, Type, Value}) when Type==nstring ->
	case is_list(Value) of
		true ->	{Key, Level, Type, Value};
		false -> log(LogFun, debug, "config: expecting 'nstring' for Key: ", [Key]), {}
	end;
check3_one_default(LogFun, {Key, Level, Type, Value}) when Type==int ->
	case is_integer(Value) of
		true ->	{Key, Level, Type, Value};
		false -> log(LogFun, debug, "config: expecting 'int' for Key: ", [Key]), {}
	end;
check3_one_default(LogFun, {Key, Level, Type, Value}) when Type==float ->
	case is_float(Value) of
		true ->	{Key, Level, Type, Value};
		false -> log(LogFun, debug, "config: expecting 'float' for Key: ", [Key]), {}
	end;
check3_one_default(LogFun, {Key, Level, Type, Value}) when Type==atom ->
	case is_atom(Value) of
		true ->	{Key, Level, Type, Value};
		false -> log(LogFun, debug, "config: expecting 'atom' for Key: ", [Key]), {}
	end.







get_entry_from_defaults([], _Key) -> {};

%% @doc Retrieves an Entry from Key in the Defaults list
%%
%% @spec get_entry_from_defaults(Defaults, Key) -> Entry
%%
get_entry_from_defaults([ModuleEntries|Modules], Key) ->
	{_ModuleName, Entries}=ModuleEntries,
	%io:format("get_entry_from_defaults: Mod[~p] Key[~p] Entries[~p]~n",[ModuleName, Key, Entries]),
	Entry=?TOOLS:kfind(Key, Entries),
	case Entry of
		{} ->
			get_entry_from_defaults(Modules, Key);
		Entry ->
			Entry
	end.

	


%% @doc Retrieves a module's defaults
%%
%% @spec get_module_defaults(Entries, Module) -> list()
%%
get_module_defaults(Entries, Module) ->
	?TOOLS:kfind(Module, Entries).



%% @doc Retrieves a module's blacklist entries
get_module_blacklist(Module) ->
	try
		erlang:apply(Module, blacklist, [])
	catch
		_:_ -> []
	end.


%% @doc Retrieves all the blacklisted parameters
%%		from all modules
%% @spec get_blacklist(Modules, Acc) -> list()
%%
get_blacklist([Module|Modules], Acc) ->
	List=get_module_blacklist(Module),
	get_blacklist(Modules, Acc++List);

get_blacklist([], Acc) ->
	Acc.
	


%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------



%% @doc Finds a specific Key from the tuple list
%%
find_key_in_config_list(List, Key) ->
	?TOOLS:kfind(Key, List).





%% @doc Retrieves the 'min' value for Key in the Defaults
%%
%% @spec get_min(List, Key) -> tuple() | {}
%% where
%%	Key=atom()
get_min(List, Key) ->
	?TOOLS:get_special(List, ".min", Key).
	

%% @doc Retrieves the 'max' value for Key in the Defaults
%%
%% @spec get_max(List, Key) -> tuple() | {}
%% where
%%	Key=atom()

%%
get_max(List, Key) ->
	?TOOLS:get_special(List, ".max", Key).







	


%% @doc Retrieves the Server associated with Module
%%
%% @spec get_module_server(Module) -> undefined | atom()
%%
get_module_server(Module) ->
	try
		erlang:apply(Module, get_server, [])
	catch
		_:_ -> undefined
	end.



%% @doc Merge the parameters from the Defaults
%%		with ones from the configuration file
%%
%%		It is assumed that the 'Config' list has
%%		been validated.
%%
%%		Config   =[{ModuleName, [{ParamName, Value}]} ]
%%		Defaults =[{ModuleName.ParamName, Level, Type, Value}]
%%
%%		Result= [{ModuleName.Param, Value}]
%%
%% @spec merge(LogFun, Defaults, Config) -> [tuple()]
%%
merge(LogFun, Defaults, Config) ->
	%io:format("merge: defaults<~p>~n", [Defaults]),
	%io:format("merge: config<~p>~n", [Config]),
	
	% start with all the Defaults in list
	do_merge(LogFun, Config, Defaults).


do_merge(_, [], Acc) ->
	Acc;

do_merge(LogFun, [ConfigEntry|ConfigEntries], Acc) ->
	%io:format("do_merge: ce[~p]~n", [ConfigEntry]),
	
	try
		{ParamName, Value}=ConfigEntry,
		%io:format("do_merge: param[~p] value[~p]~n",[ParamName, Value]),
		ModuleName=extract_module_name(ParamName),
		%io:format("do_merge: moduleName[~p]~n",[ModuleName]),
		NewList=insert_entry(ModuleName, ParamName, Value, Acc),
		do_merge(LogFun, ConfigEntries, NewList)
	catch
		_:_ ->
			log(LogFun, error, "config: error whilst merging defaults+config"),
			{error, merging}
	end;

do_merge(LogFun, Other, _) ->
	log(LogFun, critial, "config: exception whilst merging: ", [Other]).


insert_entry(ModuleName, ParamName, Value, List) ->
	NewList=?TOOLS:rem_from_tuple_list(List, ModuleName, ParamName),
	?TOOLS:add_to_tuple_list(NewList, ModuleName, {ParamName, Value}).





is_for_module(ModuleName, Key) ->
	HeadKey=?TOOLS:extract_head(Key),
	is_for_module2(ModuleName, HeadKey).

is_for_module2(X, X) -> true;
is_for_module2(_, _) -> false.


extract_module_name(Key) ->
	?TOOLS:extract_head(Key).



get_module_entries(ModuleName, Config) ->
	get_module_entries(ModuleName, Config, []).


get_module_entries(_ModuleName, [], Acc) -> Acc;
get_module_entries(ModuleName, [Entry|Rest], Acc) ->
	{ParamName, Value}=Entry,
	case is_for_module(ModuleName, ParamName) of
		true  -> get_module_entries(ModuleName, Rest, Acc++[{ParamName, Value}]);
		false -> get_module_entries(ModuleName, Rest, Acc)
	end.




put_config(Version, ConfigData) ->
	put(config.version, Version),
	put_config(ConfigData).

put_config([]) ->
	ok;
	
put_config(ConfigData) when is_list(ConfigData) ->
	[Entry|Entries] = ConfigData,
	put_config_one(Entry),
	put_config(Entries);
	
put_config(_) ->
	{error, invalid_param}.

%% For entries from Defaults
put_config_one({Param, _Level, _Type, Value}) ->
	put(Param, Value);

%% For entries from Configuration File
put_config_one({Param, Value}) ->
	put(Param, Value).





%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LIMITS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------



check_limits(_, [], _Defaults, Acc) ->
	Acc;

check_limits(LogFun, [ConfigEntry|Entries], Defaults, Acc) ->
	Entry=check_limit_one(LogFun, ConfigEntry, Defaults),
	check_limits(LogFun, Entries, Defaults, Acc++[Entry]).



check_limit_one(LogFun, {Key, Value}, Defaults) ->
	Default=get_var_in_defaults(Key, Defaults),
	case Default of 
		undefined ->
			log(LogFun, debug, "config: missing default for key: ", [Key]);
		{{_, Entries}, {_, _Level, int, _Default}} ->
			MinResult=get_min(Entries, Key),
			MaxResult=get_max(Entries, Key),
			%io:format("check_limit_one2: key[~p] val[~p] min[~p] max[~p]~n", [Key, Value, MinResult, MaxResult]),
			check_limit_one2(LogFun, {Key, Value}, MinResult, MaxResult);
		{{_, Entries}, {_, _Level, float, _Default}} ->
			MinResult=get_min(Entries, Key),
			MaxResult=get_max(Entries, Key),
			%io:format("check_limit_one2: key[~p] val[~p] min[~p] max[~p]~n", [Key, Value, MinResult, MaxResult]),
			check_limit_one2(LogFun, {Key, Value}, MinResult, MaxResult);
		_ ->
			type_without_limit_check,
			{Key, Value}
	end;			
	

%% shouldn't get here...
check_limit_one(_, _, _Defaults) ->
	ok.

check_limit_one2(LogFun, {Key, Value}, {}, {}) ->
	log(LogFun, debug, "config: missing 'min' default for key: ", [Key]), 
	log(LogFun, debug, "config: missing 'max' default for key: ", [Key]),
	{Key, Value};

check_limit_one2(LogFun, {Key, Value}, Result, {}) ->
	Limit=get_value(Result),
	log(LogFun, debug, "config: missing 'max' default for key: ", [Key]),
	check_limit(LogFun, min, {Key, Value}, Limit);

check_limit_one2(LogFun, {Key, Value}, {}, Result) ->
	Limit=get_value(Result),
	log(LogFun, debug, "config: missing 'min' default for key: ", [Key]),
	check_limit(LogFun, max, {Key, Value}, Limit);


check_limit_one2(LogFun, {Key, Value}, MinResult, MaxResult) ->
	MinV=get_value(MinResult),
	MaxV=get_value(MaxResult),
	%io:format("check_limit_one2: minv[~p] maxv[~p]~n", [MinV, MaxV]),
	{K1, V1}=check_limit(LogFun, min, {Key, Value}, MinV),
	check_limit(LogFun, max, {K1, V1}, MaxV).




check_limit(LogFun, max, {Key, Value}, {_, Limit}) when (is_integer(Value) or is_float(Value)) 
  											and (is_integer(Limit) or is_float(Limit)) ->
	case Value > Limit of
		true ->	log(LogFun, error, "config: value 'too high' {Key, Value, MaxValue} ", [Key, Value, Limit]), 
				{Key, Limit};
		false->	{Key, Value}
	end;


check_limit(LogFun, min, {Key, Value}, {_, Limit}) when (is_integer(Value) or is_float(Value)) 
  											and (is_integer(Limit) or is_float(Limit)) ->
	case Value < Limit of
		true ->	log(LogFun, error, "config: value 'too low' {Key, Value, MinValue} ", [Key, Value, Limit]), 
				{Key, Limit};
		false->	{Key, Value}
	end;

check_limit(LogFun, _, {Key, _Value}, Limit) ->
	log(LogFun, error, "config: invalid default value for {Key, DefaultValue}: ", [[Key, Limit]]),
	{Key, Limit}.



%% @doc Retrieves a specific entry in the Defaults list
%%
%% @spec get_var_in_defaults(VarName, Defaults) -> undefined | {[ModuleEntries], Entry}
%% where
%%	ModuleEntries=[tuple()]
%%	Entry=tuple()
%%
get_var_in_defaults(VarName, Defaults) ->
	ModuleName=?TOOLS:extract_head(VarName),
	ModuleEntries=?TOOLS:kfind(ModuleName, Defaults),
	case ModuleEntries of
		{} -> undefined;
		{_, Entries} ->
			{ModuleEntries, ?TOOLS:kfind(VarName, Entries)}
	end.



filter_entries([], Acc)           -> Acc;
filter_entries([[]|Rest], Acc)    -> filter_entries(Rest, Acc);
filter_entries([{}|Rest], Acc)    -> filter_entries(Rest, Acc);
filter_entries([Entry|Rest], Acc) -> filter_entries(Rest, Acc++[Entry]).


%% @doc Retrieves the Key field from an Entry (defaults format)
%% 
%% @spec get_key(Entry) -> Key
%% where 
%%	Entry = {Key, Level, Type, Value}
%%	Key = atom()
get_key({Key, _Level, _Type, _Value}) -> {key, Key};
get_key(_) -> error.

get_level({_Key, Level, _Type, _Value}) -> {level, Level};
get_level(_) ->	error.

get_type({_Key, _Level, Type, _Value}) -> {type, Type};
get_type(_) -> error.

get_value({_Key, _Level, _Type, Value}) -> {value, Value};
get_value(_) -> error.




%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  BOILER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  PLATE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  FOR     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  MODULES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------


%% NOTE the process dictionary variable 'config.version'
%%
do_publish_config_version(Switch, Server) ->
	Version=get(config.version),
	erlang:apply(Switch, publish, [sys, {mod.config, Server, Version}]).

do_config(_Switch, _Server, undefined) -> ok;
do_config(Switch, Server, VersionInForce) ->
	Version=get(config.version),
	maybe_ask_for_config(Switch, Server, VersionInForce, Version).	

maybe_ask_for_config(_,_, X, X) -> ok;
maybe_ask_for_config(Switch, Server, _, _) -> do_publish_config_version(Switch, Server).




