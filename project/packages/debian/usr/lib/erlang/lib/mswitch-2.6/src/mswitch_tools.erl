%% Author: Jean-Lou Dupont
%% Created: 2009-08-19
%% Description: Common function tools
-module(mswitch_tools).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 tern/4,
		 isdebug/0,
		 getvar/2, getvar/3,
		 add_to_var/2, rem_from_var/2,
		 msg/1, msg/2,
		 extract_host/0,
		 make_node/1
		,kfind/2, kfind/3
		
		,to_string/1
		,gen_auth_header/2
		,url_encode/1
		,code_to_phrase/1
		
		,encode_list/1
		,encode_tuple/2
		,force_list/1
		
		,http_extract/2
		
		,concat_atoms/2
		,to_atom/1
		
		,head/1
		,head_pair/2
		
		,make_atom_from_list/1
		,vsize/1

		,is_alive/1
			
		,extract_head/1	
		,extract_tail/1
		
		,add_to_tuple_list/3
		,rem_from_tuple_list/3
		
		,make_list/1
		
		,has_pattern/2
		,check_pattern/2
		,filter_on_patterns/3
		
		,get_special/3
		
		,integer_to_hex/1
		 ]).

%% @doc Ternary operator
%%		If 'Var==Value' then 'True' else 'False'
%%
%% @spec tern(Var, Value, True, False) -> True | False
%% where
%%	Var   = term()
%%	Value = term()
%%	True = term()
%%	False= term()
%%
tern(Var, Value, True, False) ->
	case Var of
		Value -> True;
		_     -> False
	end.


%% @doc Returns atom(true) if the process dictionary
%%		variable 'debug' is found.
%% 
%% @spec isdebug() -> true | false
%%
isdebug() ->
	Params=getvar(params, []),
	lists:member("debug", Params)
	or
	lists:member(debug, Params).


%% @doc Retrieves the Value of a VarName in the process dictionary
%% and returns Default is not found.
%%
%% @spec getvar(VarName, Default) -> Value | Default
%% where
%%	VarName = atom()
%%	Value = term()
%%	Default = term()
%%
getvar(VarName, Default) ->
	VarValue=get(VarName),
	getvar(VarName, VarValue, Default).

getvar(VarName, undefined, Default) ->
	put(VarName, Default),
	Default;

getvar(_VarName, VarValue, _Default) ->
	VarValue.


%% @doc Adds a term Value to a Value list
%%		in the process dictionary.
%%
%% @spec add_to_var(VarName, VarValue) -> void()
%% where
%%	VarName = atom()
%%	VarValue = term()
%%
add_to_var(VarName, VarValue) when is_list(VarValue)->
	List=getvar(VarName, []),
	FilteredList=List--VarValue,
	NewList=FilteredList++VarValue,
	put(VarName, NewList);
	
	
add_to_var(VarName, VarValue) ->
	List=getvar(VarName, []),
	FilteredList=List--[VarValue],
	NewList=FilteredList++[VarValue],
	put(VarName, NewList).


%%@doc Removes a term Value from a Value list
%%		in the proces dictionary.
%%
%% @spec rem_from_var(VarName, VarValue) -> void()
%% where
%%	VarName = atom()
%%	VarValue = term()
%%
rem_from_var(VarName, VarValue) when is_list(VarValue)->
	List=getvar(VarName, []),
	FilteredList=List--VarValue,
	put(VarName, FilteredList);

rem_from_var(VarName, VarValue) ->
	List=getvar(VarName, []),
	FilteredList=List--[VarValue],
	put(VarName, FilteredList).


%% @doc Outputs Message if 'debug' variable
%%		is defined in the process dictionary.
%%
%% @spec msg(Message) -> void()
%% where
%%	Message = term()
%%
msg(Message) ->
	msg(Message, []).

msg(Message, Params) ->
	Debug=isdebug(),
	domsg(Debug, Message, Params).

%% @private
domsg(false, _, _) ->
	ok;

domsg(true, Message, Params) ->
	Msg="~s:",
	io:format(Msg++Message++"~n", ["mswitch"]++Params).


%% @doc Extracts the "host" part of the running node
%% 		i.e.  Host@Node
%%
%% @spec extract_host() -> string()
%%
extract_host() ->
	extract_host(node()).

%% @doc Extracts the "host" part of a "short-name" node name
%%		e.g. Host@Node
%%
%% @spec extract_host(Node) -> string()
%% where
%%	Node = atom()
%%
extract_host(Node) when is_atom(Node) ->
	extract_host(atom_to_list(Node));

extract_host(Node) when is_list(Node) ->
	Tokens = string:tokens(Node, "@"),
	lists:last(Tokens).
	

%% @doc Makes a "short name" node from a Name
%%		e.g. Name@Node
%%
%% @spec make_node(Name) -> string()
%% where
%%	Name=atom()
%%
make_node(Name) ->
	make_node(Name, node()).

%% @private
make_node(Name, Node) when is_atom(Name) ->
	make_node(erlang:atom_to_list(Name), Node);

make_node(Name , Node) when is_list(Name) ->
	Host=extract_host(Node),
	PartialName=string:concat(Name, "@"),
	CompleteName=string:concat(PartialName, Host),
	erlang:list_to_atom(CompleteName).



kfind(_Key, []) ->	{};

%% @doc Searches through a tuple list for Key
%%
%% @spec kfind(Key, List) -> {} | tuple()
%% where
%%	Key = atom()
%%	List = [tuple()]
%%	Value = term()
kfind(Key, List) ->
	case is_list(List) of
		true  -> TheList=List;
		false -> TheList=[List]
	end,
	
	case erlang:is_builtin(lists, keyfind, 3) of
		true  ->
			case lists:keyfind(Key,1,TheList) of
				false -> {};
				Tuple -> Tuple
			end;

		false ->
			case lists:keysearch(Key,1,TheList) of
				{value, Value} -> Value;
				_              -> {}
			end
	end.

kfind(Key, [], Default) ->
	{Key, Default};

%% @doc Returns {Key, Default} if not found or {Key, Value} otherwise
%%
%% @spec kfind(Key, List, Default) -> tuple() | {Key, Default}
%% where
%%	Key = atom()
%%	List = [tuple()]
%%	Value = term()
%%	Default = term()
kfind(Key, List, Default) ->
	case kfind(Key, List) of
		false        -> {Key, Default};
		{Key, Value} ->	{Key, Value}
	end.


%% @doc Converts a term() to a string()
%%
%% @spec to_string(term()) -> list()
%%
to_string(StringTerm) ->
	Binary=erlang:iolist_to_binary(StringTerm),
	erlang:binary_to_list(Binary).
	

%% @doc Generate an HTTP compatible "basic authentication" header
%%
%% @spec gen_auth_header(Username, Password) -> string()
%% where
%%	Username = string()
%%	Password = string()
%%
gen_auth_header(Username, Password) ->
	"Basic "++gen_auth(Username, Password).
	
gen_auth(Username, Password) ->
	StringToEncode=Username++":"++Password,
	base64:encode_to_string(StringToEncode).



url_encode(I) when is_integer(I) ->
	url_encode(erlang:integer_to_list(I));

%% @doc URL Encode
%%
%% @spec url_encode(term()) -> string()
%%
url_encode(T) when is_tuple(T) ->
	S=io_lib:format("~p", [T]),
	url_encode(lists:flatten(S));

	
%% From YAWS - modified by jldupont
url_encode([H|T]) ->
    if
	
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
		
        H == $_; H == $.; H == $-; H == $/;	H == $:; H == ${; H == $} -> % FIXME: more..
            [H|url_encode(T)];

		%%jld
		is_tuple(H) ->
			FT=io_lib:format("~p", [H]),
			[url_encode(lists:flatten(FT))|url_encode(T)];


        true ->
			%io:format("integer_to_hex: ~p~n", [H]),
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)];
				X ->
					[$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].



%% From YAWS
%% @private
integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} ->
            old_integer_to_hex(I);
        Int ->
            Int
    end.

old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);

old_integer_to_hex(I) when I<16 ->
    [I-10+$A];

old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).





%% from yaws_api
%% @doc HTTP Code to human readable string
%%
%% @spec code_to_phrase(integer()) -> string()
%%
code_to_phrase(100) -> "Continue";
code_to_phrase(101) -> "Switching Protocols ";
code_to_phrase(200) -> "OK";
code_to_phrase(201) -> "Created";
code_to_phrase(202) -> "Accepted";
code_to_phrase(203) -> "Non-Authoritative Information";
code_to_phrase(204) -> "No Content";
code_to_phrase(205) -> "Reset Content";
code_to_phrase(206) -> "Partial Content";
code_to_phrase(207) -> "Multi Status";
code_to_phrase(300) -> "Multiple Choices";
code_to_phrase(301) -> "Moved Permanently";
code_to_phrase(302) -> "Found";
code_to_phrase(303) -> "See Other";
code_to_phrase(304) -> "Not Modified";
code_to_phrase(305) -> "Use Proxy";
code_to_phrase(306) -> "(Unused)";
code_to_phrase(307) -> "Temporary Redirect";
code_to_phrase(400) -> "Bad Request";
code_to_phrase(401) -> "Unauthorized";
code_to_phrase(402) -> "Payment Required";
code_to_phrase(403) -> "Forbidden";
code_to_phrase(404) -> "Not Found";
code_to_phrase(405) -> "Method Not Allowed";
code_to_phrase(406) -> "Not Acceptable";
code_to_phrase(407) -> "Proxy Authentication Required";
code_to_phrase(408) -> "Request Timeout";
code_to_phrase(409) -> "Conflict";
code_to_phrase(410) -> "Gone";
code_to_phrase(411) -> "Length Required";
code_to_phrase(412) -> "Precondition Failed";
code_to_phrase(413) -> "Request Entity Too Large";
code_to_phrase(414) -> "Request-URI Too Long";
code_to_phrase(415) -> "Unsupported Media Type";
code_to_phrase(416) -> "Requested Range Not Satisfiable";
code_to_phrase(417) -> "Expectation Failed";
code_to_phrase(500) -> "Internal Server Error";
code_to_phrase(501) -> "Not Implemented";
code_to_phrase(502) -> "Bad Gateway";
code_to_phrase(503) -> "Service Unavailable";
code_to_phrase(504) -> "Gateway Timeout";
code_to_phrase(505) -> "HTTP Version Not Supported".



encode_list([]) ->
	"";

%% @doc URL encoding a tuple list of {Key, Value}
%%		i.e. Key1=Value1&Key2=Value2 ...
%%
%% @spec encode_list(list()) -> string()
%%
encode_list([{Key, Value}]) ->
	LKey=force_list(Key),
	url_encode(LKey) ++ "=" ++ url_encode(Value);
	
encode_list([H]) ->
	url_encode(H);

encode_list([{Key, Value}|T]) ->
	LKey=force_list(Key),
	url_encode(LKey)++"="++url_encode(Value)++"&"++ encode_list(T);

encode_list([H|T]) ->
	url_encode(H) ++ "&" ++ encode_list(T);

encode_list(E) ->
	url_encode(E).


%% @doc URL encode a tuple {Key, Value} as Key=Value
%%
%% @spec encode_tuple(Key, Value) -> string()
%% where
%%	Key=atom()
%%	Value=atom()
%%
encode_tuple(Key, Value) ->
	LKey=force_list(Key),
	url_encode(LKey)++"="++url_encode(Value).


%% @doc Forces an atom() to a list()
%%
%% @spec force_list(atom()) -> list()
%%
force_list(Key) when is_atom(Key) ->
	erlang:atom_to_list(Key);

force_list(Key) ->
	Key.


%% @doc Extracts various fields from an HTTP Response issued
%%		by the 'inets' module 'httpc' service.
%%
%% Result = {{HttpVersion, HttpCode, HttpResponseCode}, [Headers], ResponseBody}
%% HttpVersion = string()         (eg. "HTTP/1.1")
%% HttpCode = integer()           (eg. "200")
%% HttpResponseCode = string()    (eg. "OK")
%% Headers = {key, value}, {key, value} ...
%% ResponseBody = string()
http_extract(Result, headers) ->
	{{_Version, _Code, _CodeText}, Headers, _Body} = Result,
	Headers;

http_extract(Result, body) ->
	{{_Version, _Code, _CodeText}, _Headers, Body} = Result,
	Body;

http_extract(Result, http.code) ->
	{{_Version, Code, _CodeText}, _Headers, _Body} = Result,
	Code;

http_extract(Result, http.code.text) ->
	{{_Version, _Code, CodeText}, _Headers, _Body} = Result,
	CodeText.



%% @doc Concatenates two atom() to form a single atom()
%%
%% @spec concat_atoms(A1, A2) -> atom()
%% where
%% A1=atom()
%% A2=atom()
%%
concat_atoms(A1, A2) when is_atom(A1), is_atom(A2) ->
	L1=erlang:atom_to_list(A1),
	L2=erlang:atom_to_list(A2),
	erlang:list_to_atom(L1++L2);

concat_atoms(V1, V2) ->
	A1=to_atom(V1),
	A2=to_atom(V2),
	concat_atoms(A1, A2).


%% @doc Coerces a term() to an atom() representation
%%
%% @spec to_atom(term()) -> atom()
%%
to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_list(V) -> erlang:list_to_atom(V);
to_atom(V) when is_integer(V) ->
	L=erlang:integer_to_list(V),
	erlang:list_to_atom(L).


%% @doc Returns the Head of a list even an empty one;
%%		erlang:hd barks at empty list.
%%
%% @spec head(list()) -> term()
%%
head([]) ->	[];
head(Liste) when is_list(Liste) -> erlang:hd(Liste);
head(_) -> [].



head_pair([], _DefaultSecond) ->
	{[],[]};

%% @doc Retrieves the first two elements of a list
%%		and fills with Default if there isn't a second element.
%%
%% @spec head_pair(List, DefaultSecond) -> {Pair, Rest}
%% where
%%	List=list()
%%  DefaultSecond=atom() | string() | integer()
%%	Pair=list()
%%	Rest=list()
%%
head_pair(Liste, DefaultSecond) when is_list(Liste) ->
	[First|Rest] = Liste,
	head_pair(First, Rest, DefaultSecond).

head_pair(First, [], DefaultSecond) ->
	{[First, DefaultSecond], []};

head_pair(First, [Second|Rest], _DefaultSecond) ->
	{[First, Second], Rest}.



%% @doc Concatenates elements from the list
%%		into one atom.
%%
%% @spec make_atom_from_list(List) -> Result
%% where
%%	List = [atom()] | [string()]
%%  Result = list()
%%
make_atom_from_list(List) when is_list(List) ->
	{HeadPair, Rest}=head_pair(List, ''),
	make_atom_from_list(HeadPair, Rest).

%% @private
make_atom_from_list([First,Second], []) ->
	concat_atoms(First, Second);


make_atom_from_list([First, Second], [Third|Rest]) ->
	Partial =concat_atoms(First, Second),
	Partial2=concat_atoms(Partial, Third),
	make_atom_from_list([Partial2|Rest]).



%% @doc Returns the 'size' of the following term() types: 
%%		atom(), list(), tuple().
%%
%% @spec vsize(Term) -> integer() | undefined
%% where
%%	Term=atom() | list() | tuple()
%%
vsize(Atom) when is_atom(Atom) ->
	erlang:length(erlang:atom_to_list(Atom));

vsize(List) when is_list(List) ->
	erlang:length(List);

vsize(Tuple) when is_tuple(Tuple) ->
	size(Tuple);

vsize(_) ->
	undefined.


%% @doc Returns atom(true) if the process with the registered Name is alive
%%
%% @spec is_alive(Name) -> true | false
%% where
%%	Name=atom()
%%
is_alive(Name) ->
	Pid=erlang:whereis(Name),
	is_alive(Name, Pid).

%% @private
is_alive(_Name, undefined) -> false;
is_alive(_Name, Pid) when is_pid(Pid) -> erlang:is_process_alive(Pid);
is_alive(_,_) -> false.



extract_head(Atom) ->
	extract_head(Atom, ".").

%% @doc Extracts the 'head' of the atom by tokenizing
%%		the Atom with the specified Separators list.
%%		The default SeparatorList is ".".
%%		
%% @spec extract_head(Atom, SeparatorList) -> atom()
%% where
%%	Atom=atom()
%%	SeparatorList=string()
%%
extract_head(Atom, SeparatorList) when is_atom(Atom) ->
	String=erlang:atom_to_list(Atom),
	Tokens=string:tokens(String, SeparatorList),
	extract_head2(Tokens).

extract_head2([]) ->
	'';

extract_head2([Head|_Rest]) ->
	erlang:list_to_atom(Head).
	

%% @doc Extracts the 'tail' of an atom by tokenizing
%%		the said atom using the SeparatorList. The
%%		default SeparatorList consists of ".".
%% 
%% @spec extract_tail(atom()) -> atom()
%%
extract_tail(Atom) ->
	extract_tail(Atom, ".").

extract_tail(Atom, SeparatorList) when is_atom(Atom) ->
	String=erlang:atom_to_list(Atom),
	Tokens=string:tokens(String, SeparatorList),
	extract_tail2(Tokens).

extract_tail2([]) ->
	'';

extract_tail2([_Head|Tail]) ->
	erlang:list_to_atom(Tail).


	
%% @doc Adds an Element	to a specific tuple in a List
%%		whilst respecting uniqueness. 
%%
%% @spec add_to_tuple_list(List, TupleName, Element) -> TupleList
%% where
%%	List = list()
%%	TupleName= atom()
%%	Element = term()
%%	TupleList = [{ItemName, ItemValue}]
%%	ItemName = atom()
%%	ItemValue = term()
%%
add_to_tuple_list(List, TupleName, Element) when is_atom(TupleName), is_list(List) ->
	do_add_to_tuple_list(List, TupleName, Element);

add_to_tuple_list(_,_,_) ->	{error, invalid_params}.


%% Trivial case... also makes an example for target return format
do_add_to_tuple_list([], TupleName, Element) when is_list(Element)->
	[{TupleName, Element}];

do_add_to_tuple_list([], TupleName, Element) when is_atom(Element)->
	[{TupleName, [Element]}];

do_add_to_tuple_list(List, TupleName, Element) ->
	case is_list(Element) of
		true  -> E=Element;
		false -> E=[Element]
	end,
	
	Tuple=kfind(TupleName, List),
	case Tuple of
		{} ->
			List++[{TupleName, E}];
		
		{_, Value} ->
			FNewList=List--[Tuple],
			FNewValue=Value--E,
			NewValue=FNewValue++E,
			FNewList++[{TupleName,NewValue}]
	end.


%% @doc Removes a tuple from list
%%
%% @spec rem_from_tuple_list(List, GroupName, TupleName) -> list()
%% where
%%	List=[tuple()]
%%	GroupName=atom()
%%	TupleName=atom()
%%
rem_from_tuple_list(List, GroupName, TupleName) ->
	Group=kfind(GroupName, List),
	case Group of
		{} -> List;
		{GroupName, GroupEntries} ->
			%io:format("GroupEntries: ~p~n", [GroupEntries]),
			Tuple=kfind(TupleName, GroupEntries),
			case Tuple of
				{}    -> List;
				Entry ->
					%io:format("Entry: ~p~n", [Entry]),
					PartialList=List--make_list({GroupName,GroupEntries}),
					%io:format("PartialList: ~p~n",[PartialList]),
					NewGroup=GroupEntries--make_list(Entry),
					%io:format("NewGroup: ~p~n",[NewGroup]),
					PartialList ++ [{GroupName, NewGroup}]
			end
	end.


%% @doc Forces a term() to a list() form IFF
%%		the said term isn't already a list.
%%
%% @spec make_list(Term) -> list()
%% where
%%	Term = term()
%%
make_list(List) when is_list(List) -> List;
make_list(SomeTerm) -> [SomeTerm].


%% @doc Verifies if Key contains any of Patterns
%%
%% @spec has_pattern(Patterns, Key) -> true | false
%% where
%%	Key = atom()
%%	Patterns=[atom()]
%%
has_pattern(Patterns, Key) when is_atom(Key) ->
	has_pattern(Patterns, erlang:atom_to_list(Key));

has_pattern(Patterns, Key) when is_list(Patterns) ->
	do_has_pattern(false, Patterns, Key).

do_has_pattern(true, _, _)      -> true;
do_has_pattern(false, [], _Key) -> false;
	
do_has_pattern(false, Patterns, Key) ->
	[Pattern|Rest] = Patterns,
	Result=check_pattern(Pattern, Key),
	do_has_pattern(Result, Rest, Key).

%% @doc Verifies if Key string contains atom Pattern 
%%
%% @spec check_pattern(Pattern, Key) -> true | false
%% where
%%	Pattern=atom()
%%	Key=atom()
%%
check_pattern(Pattern, Key) ->
	Str=erlang:atom_to_list(Pattern),
	case string:str(Key, Str) of
		0 -> false;
		_ -> true
	end.


%% @doc Retrieves the complete tuple matching Key
%%
%% @spec get_special(List, Pattern, Key) -> tuple() | {}
%% where
%%	List=[tuple()]
%%	Pattern=atom()
%%	Key=atom()
get_special(List, Pattern, Key) when is_atom(Pattern) ->
	Pat=erlang:atom_to_list(Pattern),
	get_special(List, Pat, Key);

get_special(List, Pattern, Key) when is_list(Pattern) ->
	%%io:format("get_special: Key[~p] List[~p]~n", [Key, List]),
	Var=erlang:atom_to_list(Key)++Pattern,
	Vara=erlang:list_to_atom(Var),
	kfind(Vara, List).




filter_on_patterns(_Patterns, [], Acc) -> Acc;

%% @doc Filters elements of List based on Patterns
%%
%% @spec filter_on_patterns(Patterns, List, Acc) -> list() 
%% where
%%	@type Patterns=[atom()]
%%	List=list()
%%	Acc=list()
%%
filter_on_patterns(Patterns, List, Acc) ->
	[{Key, Value}|Rest] = List,
	case has_pattern(Patterns, Key) of
		true  -> Item={};
		false -> Item={Key, Value}
	end,
	filter_on_patterns(Patterns, Rest, Acc++[Item]).

