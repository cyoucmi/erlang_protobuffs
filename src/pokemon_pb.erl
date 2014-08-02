%% Copyright (c) 2009
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(pokemon_pb).
-export([encode_pikachu/1, decode_pikachu/1]).
-export([encode/1, decode/2]).
-record(pikachu, {abc}).

%% ENCODE
encode([]) ->
    [];
encode(Record) ->
    encode(element(1, Record), Record).

encode_pikachu(Record) when is_record(Record, pikachu) ->
    encode(pikachu, Record).

encode(pikachu, Record) ->
    [iolist(pikachu, Record)].

iolist(pikachu, Record) ->
    [pack(1, required, with_default(Record#pikachu.abc, none), string, [])].

with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];

pack(_, repeated, undefined, _, _) -> [];


pack(FNum, required, undefined, Type, _) ->
    exit({error, {required_field_is_undefined, FNum, Type}});

pack(_, repeated, [], _, Acc) ->
    lists:reverse(Acc);

pack(FNum, repeated, [Head|Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type, [pack(FNum, optional, Head, Type, [])|Acc]);

pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName|_] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);

pack(FNum, _, Data, Type, _) when Type=:=bool;Type=:=int32;Type=:=uint32;
                  Type=:=int64;Type=:=uint64;Type=:=sint32;
                  Type=:=sint64;Type=:=fixed32;Type=:=sfixed32;
                  Type=:=fixed64;Type=:=sfixed64;Type=:=string;
                  Type=:=bytes;Type=:=float;Type=:=double ->
    protobuffs:encode(FNum, Data, Type).

%% DECODE
decode_pikachu(Bytes) when is_binary(Bytes) ->
    decode(pikachu, Bytes).

decode(pikachu, Bytes) when is_binary(Bytes) ->
    Types = [{1, abc, int32, []}, {2, def, double, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pikachu, Decoded).

decode(<<>>, Types, Acc) -> reverse_repeated_fields(Acc, Types);
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keyfind(FNum, 1, Types) of
        {FNum, Name, Type, Opts} ->
            {Value1, Rest1} =
                case lists:member(is_record, Opts) of
                    true ->
                        {{FNum, V}, R} = protobuffs:decode(Bytes, bytes),
                        RecVal = decode(Type, V),
                        {RecVal, R};
                    false ->
                        {{FNum, V}, R} = protobuffs:decode(Bytes, Type),
                        {unpack_value(V, Type), R}
                end,
            case lists:member(repeated, Opts) of
                true ->
                    case lists:keytake(FNum, 1, Acc) of
                        {value, {FNum, Name, List}, Acc1} ->
                            decode(Rest1, Types, [{FNum, Name, [Value1|List]}|Acc1]);
                        false ->
                            decode(Rest1, Types, [{FNum, Name, [Value1]}|Acc])
                    end;
                false ->
                    decode(Rest1, Types, [{FNum, Name, Value1}|Acc])
            end;
        false ->
            {ok, Skipped} = protobuffs:skip_next_field(Bytes),
            decode(Skipped, Types, Acc)
    end.

reverse_repeated_fields(FieldList, Types) ->
    [ begin
          case lists:keyfind(FNum, 1, Types) of
              {FNum, Name, _Type, Opts} ->
                  case lists:member(repeated, Opts) of
                      true ->
                          {FNum, Name, lists:reverse(Value)};
                      _ ->
                          Field
                  end;
              _ -> Field
          end
      end || {FNum, Name, Value}=Field <- FieldList ].

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(pikachu, DecodedTuples) ->
    lists:foldr(
        fun({_FNum, Name, Val}, Record) ->
            set_record_field(record_info(fields, pikachu), Record, Name, Val)
        end, #pikachu{}, DecodedTuples).

set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index+1, Record, Value).

list_index(Target, List) ->  list_index(Target, List, 1).

list_index(Target, [Target|_], Index) -> Index;
list_index(Target, [_|Tail], Index) -> list_index(Target, Tail, Index+1);
list_index(_, [], _) -> -1.
