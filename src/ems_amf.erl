-module(ems_amf).
-include("ems.hrl").

-compile(export_all).

decode(Bin) ->
	{string, Command, Rest} = parse(Bin),
	case parse(Rest) of
		{{mixed_array, Rest2}, _Rest3} -> %without id,  set type to notify
					Args = decode_args(Rest2, []),
					#amf{command = list_to_atom(Command), args = Args, type=notify};
		{number, Id, Rest2}  ->
	         case parse(Rest2) of
						{{mixed_array, Rest3}, _Rest4} ->
								Args = decode_args(Rest3, []),
								_AMF = #amf{command = list_to_atom(Command), id=Id, args = Args, type=invoke};
						{{object, Rest3}, _Rest4}  ->
						    %Args = decode_args(Rest3, []),
						    _AMF = #amf{command = list_to_atom(Command), args = {object, Rest3}, type=invoke}
					end;
				_ -> ?D("AMF Decode Error")
	end.

encode(AMF) when is_record(AMF,amf) ->
	Command = encode(AMF#amf.command),
	Args    = encode(AMF#amf.args, <<>>),
	case AMF#amf.type of
		invoke ->
		  Id = encode(AMF#amf.id),
			<<Command/binary,Id/binary,Args/binary>>;
		notify ->
		    <<Command/binary,Args/binary>>
	end;

encode(null) -> <<?AMF_NULL>>;
encode(false) -> <<?AMF_BOOLEAN,0>>;
encode(true) -> <<?AMF_BOOLEAN,1>>;
encode(Atom) when is_atom(Atom) -> encode(atom_to_list(Atom));
encode(Number) when is_number(Number) -> <<?AMF_NUMBER, Number:64/float>>;
encode({mixed_array,List} = Array) when is_list(List) -> encode_mixed_array(Array);
encode({object,List} = Object)     when is_list(List) -> encode_object(Object);
encode({string,List} = _String)    when is_list(List) -> encode(List);
encode([H|_] = List) when is_list(List), is_tuple(H) ->
	encode_object(List, <<>>);
encode(List) when is_list(List) ->
	Length = length(List),
	Bin = list_to_binary(List),
	<<?AMF_STRING, 0, Length:8,Bin/binary>>;
encode(_Value) ->
	?D(_Value),
	<<>>.
%%--------------------------------------------------------------------
%% @spec (List::list(),Bin::binary()) -> any()
%% @hidden
%% @doc Encode AMF call into a binary
%% @end
%%--------------------------------------------------------------------
encode([],Bin) -> Bin;
encode([H|T],Bin) ->
	Part = case H of
		{Key0,Value0} ->
			Key = encode(Key0),
			Value = encode(Value0),
			<<Key/binary,Value/binary>>;
		Value -> encode(Value)
	end,
	encode(T, <<Bin/binary,Part/binary>>).


encode_object({object,List} = _Object) -> encode_object(List, <<>>).
encode_object([], Bin) -> <<?AMF_OBJECT,Bin/binary,0,0,9>>;
encode_object([{Key,Value}|T], Bin) ->
	KeyBinary = list_to_binary(atom_to_list(Key)),
	KeySize = size(KeyBinary),
	ValueBinary = encode(Value),
	Part = <<0,KeySize:8,KeyBinary/binary,ValueBinary/binary>>,
	encode_object(T, <<Bin/binary,Part/binary>>).


encode_mixed_array({mixed_array,List} = _Array) when is_list(List) -> encode_mixed_array(List, 0, <<>>).
encode_mixed_array([], Max, Bin) -> <<8,Max:32,Bin/binary,0,0,9>>;
encode_mixed_array([{Key0,Value}|T], Max, Bin) when is_number(Key0) ->
	Key = number_to_string(Key0),
	NewMax = if
		Key0 > Max -> Key0;
		true -> Max
	end,
	encode_mixed_array([{Key,Value}|T], NewMax, Bin);
encode_mixed_array([{Key0,Value0}|T], Max, Bin) when is_list(Key0) ->
	Key = list_to_binary(Key0),
	KeySize = size(Key),
	Value = encode(Value0),
	KeyValue = <<0,KeySize:8,Key/binary,Value/binary>>,
	encode_mixed_array(T, Max, <<Bin/binary,KeyValue/binary>>).

decode_args(<<>>,Args) -> lists:reverse(Args);
decode_args(Data,Args) ->
	case parse(Data) of
		{Type,Value,Rest} -> decode_args(Rest,[{Type,Value}|Args]);
		{Value,Rest} -> decode_args(Rest,[Value|Args]);
		[{Type,{_TypeName, Value}} | Rest]  ->
		  decode_args(Rest, [{Type, Value} | Args]);
		_ -> parse(Data)
	end.

parse(<<?AMF_NUMBER:8, Double:64/float, Rest/binary>>) ->
    {number, round(Double), Rest};
parse(<<?AMF_BOOLEAN,0,Rest/binary>>) ->
	{boolean,false,Rest};
parse(<<?AMF_BOOLEAN,1,Rest/binary>>) ->
	{boolean,true,Rest};
parse(<<?AMF_STRING,_UTF8:8,Length:8,Rest/binary>>) ->
	<<Binary:Length/binary,Next/binary>> = Rest,
	{string,binary_to_list(Binary),Next};
parse(<<?AMF_OBJECT,Rest/binary>>) ->
	parse_object(Rest,[]);
parse(<<?AMF_NULL,Rest/binary>>) ->
	{null,null,Rest};
parse(<<?AMF_UNDEFINED,Rest/binary>>) ->
	{undefined,undefined,Rest};
parse(<<?AMF_MIXED_ARRAY,_Index:32,Rest/binary>>) ->
	parse_mixed_array(Rest,[]);
parse(<<?AMF_ARRAY,Length:32,Rest/binary>>) ->
%	?D(Length),
%	?D(Rest),
	parse_array(Rest,Length,[]);

parse(_Bin) -> ok.

parse_array(<<>>,_Length,Array) -> {{array,lists:reverse(Array)}, <<>>};
parse_array(Rest,Length,Array) when length(Array) == Length -> {{array,lists:reverse(Array)}, Rest};
parse_array(Data,Length,Array) ->
	{Type,Value,Rest} = parse(Data),
%	?D({Type,Value,size(Rest)}),
	parse_array(Rest,Length,[{Type,Value}|Array]).



parse_mixed_array(<<0,0,9,Rest/binary>>, Array) ->
  {{mixed_array, lists:reverse(Array)}, Rest};
parse_mixed_array(Data, Array) ->
	<<0,L:8,Rest/binary>> = Data,
	<<I:L/binary,Rest2/binary>> = Rest,
	Index = to_number(binary_to_list(I)),
	case parse(Rest2) of
		{Type,Value,Rest3} -> parse_mixed_array(Rest3,[{Index,{Type,Value}}|Array]);
		_ ->  case Rest2 of
				<<?AMF_MIXED_ARRAY,_Index:32,Rest4/binary>> ->
				  %muriel: this is a cue point info, needs to be added somewhere
					parse_mixed_array(Rest4, Array)
			end
	end.


parse_object(<<0, 0, 9, Rest/binary>>, KeyValueList) -> {{object, lists:reverse(KeyValueList)}, Rest};
parse_object(Data, KeyValueList) ->
    case parse_string(Data) of
        {Key, Rest} ->
            case parse(Rest) of
				{Value, Rest2} ->
                    NewKeyValueList =  [{list_to_atom(Key), Value} | KeyValueList],
					parse_object(Rest2, NewKeyValueList);
                {_Type, Value, Rest2} ->
                    NewKeyValueList =  [{list_to_atom(Key), Value} | KeyValueList],
					parse_object(Rest2, NewKeyValueList);
                _ ->
                    {error, Key, Rest}
            end
    end.

parse_string(<<Len:16/unsigned, Rest/binary>>) ->
    <<String:Len/binary, NewRest/binary>> = Rest,
    {binary_to_list(String), NewRest}.


to_number(String) ->
	case string:to_float(String) of
		{F,_} when is_float(F) -> F;
		{error,no_float} ->
			case string:to_integer(String) of
				{I,_} when is_integer(I) -> I;
				{error,no_integer} -> String
			end
	end.


%%--------------------------------------------------------------------
%% @spec (Number::number()) -> string()
%% @hidden
%% @doc Converts a numer into a string/list()
%% @end
%%--------------------------------------------------------------------
number_to_string(Float) when is_float(Float) -> float_to_list(Float);
number_to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer).
