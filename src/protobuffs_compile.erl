%% Copyright (c) 2009-2013
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
-module(protobuffs_compile).

-export([scan_file/1, scan_file/2, scan_string/2, scan_string/3]).

%%--------------------------------------------------------------------
%% @doc Generats a built .beam file and header file .hrl
%%--------------------------------------------------------------------
-spec scan_file(ProtoFile :: string()) ->
               ok | {error, _}.
scan_file(ProtoFile) ->
    scan_file(ProtoFile,[]).

-spec scan_string(String :: string(), BaseName :: string()) ->
             ok | {error, _}.
scan_string(String,BaseName) ->
    scan_string(String,BaseName,[]).

%%--------------------------------------------------------------------
%% @doc Generats a built .beam file and header file .hrl
%%      Considerd option properties: output_include_dir,
%%                                   output_ebin_dir,
%%                                   imports_dir
%%--------------------------------------------------------------------
-spec scan_file(ProtoFile :: string() | atom(), Options :: list()) ->
               ok | {error, _}.
scan_file(ProtoFile,Options) when is_list(ProtoFile) ->
    Basename = filename:basename(ProtoFile, ".proto") ++ "_pb",
    {ok,String} = parse_file(ProtoFile),
    scan_string(String,Basename,Options);
scan_file(ProtoFile,Options) when is_atom(ProtoFile) ->
    Basename = atom_to_list(ProtoFile) ++ "_pb",
    {ok,String} = parse_file(atom_to_list(ProtoFile) ++ ".proto"),
    scan_string(String,Basename,Options).

-spec scan_string(String :: string(), Basename :: string(), Options :: list()) ->
             ok | {error, _}.
scan_string(String,Basename,Options) ->
    {ok,Messages} = parse_string(String),
    output(Basename, Messages, Options).

%% @hidden
output(Basename, RawMessages, Options) ->
    Messages = lists:map(fun({message, Name, _, _,  Fields})->{Name, Fields} end, RawMessages),
    HeaderFile = case proplists:get_value(output_include_dir,Options) of
    undefined ->
        Basename ++ ".hrl";
    HeaderPath ->
        filename:join(HeaderPath,Basename) ++ ".hrl"
    end,

    error_logger:info_msg("Writing header file to ~p~n",[HeaderFile]),
    ok = write_header_include_file(HeaderFile, Messages),

    PbMapFile = case proplists:get_value(output_src_dir, Options) of
    undefined ->
        Basename ++ "_map.erl";
    SrcPath ->
        filename:join(SrcPath,Basename) ++ "_map.erl"
    end,
    Messages1 = lists:map(fun({message, Name, Cmd, Module,  _})->{Name, Cmd, Module} end, RawMessages),
%    io:format("RawMessages=~w", [RawMessages]),
    ok = write_pb_map_file(PbMapFile, Messages1),
    error_logger:info_msg("Writing map file to ~p~n",[PbMapFile]),

    PokemonBeamFile = code:where_is_file("pokemon_pb.beam"),
    {ok,{_,[{abstract_code,{_,Forms}}]}} = beam_lib:chunks(PokemonBeamFile, [abstract_code]),
%    io:format("Forms=~w~n",[Forms]),
    Forms1 = filter_forms(Messages, [], Forms, Basename, []),

%    io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(Forms1))]),

    {ok, _, Bytes, _Warnings} = protobuffs_file:compile_forms(Forms1, proplists:get_value(compile_flags,Options,[])),
    BeamFile = case proplists:get_value(output_ebin_dir,Options) of
    undefined ->
        Basename ++ ".beam";
    BeamPath ->
        filename:join(BeamPath,Basename) ++ ".beam"
    end,
    error_logger:info_msg("Writing beam file to ~p~n",[BeamFile]),
    protobuffs_file:write_file(BeamFile, Bytes).

%% @hidden
parse_file(FileName) ->
    {ok, InFile} = protobuffs_file:open(FileName, [read]),
    String = parse_file(InFile,[]),
    file:close(InFile),
    {ok,String}.

%% @hidden
parse_file(InFile,Acc) ->
    case protobuffs_file:request(InFile) of
        {ok,Token,_EndLine} ->
            parse_file(InFile,Acc ++ [Token]);
        {error,token} ->
            exit(scanning_error);
        {eof,_} ->
            Acc
    end.

parse_string(String) ->
    case lists:all(fun erlang:is_integer/1, String) of
        true ->
            {ok, Tokens, _Line} = protobuffs_scanner:string(String),
            io:format("Tokens=~w~n", [Tokens]),
            protobuffs_parser:parse(Tokens);
        false ->
            protobuffs_parser:parse(String)
    end.

%% @hidden
filter_forms(Msgs, Enums, [{attribute,L,file,{_,_}}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,file,{"src/" ++ Basename ++ ".erl",L}}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,module,pokemon_pb}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,module,list_to_atom(Basename)}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,export,[{encode_pikachu,1},{decode_pikachu,1}]}|Tail], Basename, Acc) ->
    Exports = lists:foldl(
        fun({Name,_}, Acc1) ->
            [{list_to_atom("encode_" ++ string:to_lower(Name)),1},
             {list_to_atom("decode_" ++ string:to_lower(Name)),1}|Acc1]
        end, [], Msgs),
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,export,Exports}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,record,{pikachu,_}}|Tail], Basename, Acc) ->
    Records = [begin
           OutFields = [string:to_lower(A) || {_, _, _, A, _} <- lists:keysort(1, Fields)],
           Frm_Fields = [{record_field,L,{atom,L,list_to_atom(OutField)}}|| OutField <- OutFields],
           {attribute, L, record, {atomize(Name), Frm_Fields}}
           end || {Name, Fields} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Records ++ Acc);

filter_forms(Msgs, Enums, [{function,L,encode_pikachu,1,[RecordClause]}|Tail], Basename, Acc) ->
    Functions = [begin
             {function,L,list_to_atom("encode_" ++ string:to_lower(Name)),1,[replace_atom(RecordClause, pikachu, atomize(Name))]}
         end || {Name, _} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, Enums, [{function,L,encode,2,[RecordClause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_encode_function(Msgs, L,RecordClause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,iolist,2,[Clause]}|Tail], Basename, Acc) ->
     filter_forms(Msgs, Enums, Tail, Basename, [expand_iolist_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,decode_pikachu,1,[Clause]}|Tail], Basename, Acc) ->
    Functions = [begin
             {function,
              L,
              list_to_atom("decode_" ++ string:to_lower(Name)),
              1,
              [replace_atom(Clause, pikachu, atomize(Name))]}
         end || {Name, _} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, Enums, [{function,L,decode,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_decode_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,to_record,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_to_record_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,_L,with_default,2,_Args}=Func|Tail],Basename,Acc) ->
    Acc2 = case any_message_has_fields(Msgs) of
        true -> [Func|Acc];
        false -> Acc
    end,
    filter_forms(Msgs,Enums,Tail,Basename,Acc2);

filter_forms(Msgs, Enums, [{function,_L,pack,5,_Clauses}=Func|Tail],Basename,Acc) ->
    Acc2 = case any_message_has_fields(Msgs) orelse any_message_has_extentions(Msgs) of
        true -> [Func|Acc];
        false -> Acc
    end,
    filter_forms(Msgs,Enums,Tail,Basename,Acc2);

filter_forms(Msgs, Enums, [Form|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [Form|Acc]);

filter_forms(_, _, [], _, Acc) -> lists:reverse(Acc).

any_message_has_extentions(Msgs) ->
    Predicate = fun({_,_,disallowed}) -> false; (_) -> true end,
    lists:any(Predicate, Msgs).

any_message_has_fields(Msgs) ->
    Predicate = fun({_,[],_}) -> false; (_) -> true end,
    lists:any(Predicate, Msgs).


%% @hidden
filter_record_encode_clause({MsgName, _Fields}, {clause,L,_Args,Guards,_Content}) ->
    ToIolist = {cons, L,
                {call,L, {atom,L,iolist}, [{atom,L,atomize(MsgName)},{var,L,'Record'}]},
                {nil,L}
               },
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Record'}],Guards,[ToIolist]}.

expand_iolist_function(Msgs, Line, Clause) ->
    {function,Line,iolist,2,[filter_iolist_clause(Msg, Clause) || Msg <- Msgs]}.

filter_iolist_clause({MsgName, []}, {clause,L,_Args,Guards,_Content}) ->
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'_Record'}],Guards,[{nil,L}]};
filter_iolist_clause({MsgName, Fields0}, {clause,L,_Args,Guards,_Content}) ->
    Fields = [
        case Tag of
        optional ->
            Field;
        _ ->
            {FNum,Tag,SType,SName,none}
        end
        || {FNum,Tag,SType,SName,_} = Field <- Fields0 ],
    Cons = lists:foldl(
         fun({FNum,Tag,SType,SName,Default}, Acc) ->
             {cons,L,
              {call,L,{atom,L,pack},[{integer,L,FNum},
                         {atom,L,Tag},
                         {call,L,
                          {atom,L,with_default},
                          [{record_field,L,
                        {var,L,'Record'},atomize(MsgName),
                        {atom,L,atomize(SName)}},
                           erl_parse:abstract(Default)]},
                         {atom,L,atomize(SType)},
                         {nil,L}]},
              Acc}
         end, {nil,L}, lists:reverse(lists:keysort(1, Fields))),
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Record'}],Guards,[Cons]}.

%% @hidden
expand_decode_function(Msgs, Line, Clause) ->
    {function,Line,decode,2,[filter_decode_clause(Msgs, Msg, Clause) || Msg <- Msgs]}.

%% @hidden
filter_decode_clause(Msgs, {MsgName, Fields}, {clause,L,_Args,Guards,[_,_,C,D]}) ->
    Types = lists:keysort(1, [begin
            {FNum, list_to_atom(SName),
                   atomize(SType),
                   decode_opts(Msgs, Tag, SType), Def} end ||
                 {FNum,Tag,SType,SName,Def} <- Fields]),
    Cons = lists:foldl(
         fun({FNum, FName, Type, Opts, _Def}, Acc) ->
             {cons,L,{tuple,L,[{integer,L,FNum},{atom,L,list_to_atom(string:to_lower(atom_to_list(FName)))},{atom,L,Type},erl_parse:abstract(Opts)]},Acc}
         end, {nil,L}, Types),
    Defaults = lists:foldr(
        fun
            ({_FNum, _FName, _Type, _Opts, none}, Acc) ->
                Acc;
            ({FNum, FName, _Type, _Opts, Def}, Acc) ->
                {cons,L,{tuple,L,[{integer,L,FNum},{atom,L,list_to_atom(string:to_lower(atom_to_list(FName)))},erl_parse:abstract(Def)]},Acc}
        end,
        {nil,L},
        Types),
    A = {match,L,{var,L,'Types'},Cons},
    B = {match,L,{var,L,'Defaults'},Defaults},
    D1 = replace_atom(D, pikachu, atomize(MsgName)),
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Bytes'}],Guards,[A,B,C,D1]}.

%% @hidden
expand_encode_function(Msgs, Line, RecordClause) ->
    Clauses = lists:foldl(fun(Msg, Acc) ->
        RecClause = filter_record_encode_clause(Msg, RecordClause),
        Acc ++ [RecClause]
    end, [], Msgs),
    {function,Line,encode,2,Clauses}.

%% @hidden
decode_opts(Msgs, Tag, Type) ->
    Opts0 = if Tag == repeated -> [repeated]; Tag == repeated_packed -> [repeated_packed]; true -> [] end,
    case lists:keymember(Type, 1, Msgs) of
        true ->
            [is_record|Opts0];
        false ->
            Opts0
    end.

%% @hidden
expand_to_record_function(Msgs, Line, Clause) ->
    {function,Line,to_record,2,[filter_to_record_clause(Msg, Clause) || Msg <- Msgs]}.

%% @hidden
filter_to_record_clause({MsgName, _}, {clause,L,[_Param1,Param2],Guards,[Fold]}) ->
    Fold1 = replace_atom(Fold, pikachu, atomize(MsgName)),
    {clause,L,[{atom,L,atomize(MsgName)},Param2],Guards,[Fold1]}.

%% @hidden
write_header_include_file(Basename, Messages) when is_list(Basename) ->
    {ok, FileRef} = protobuffs_file:open(Basename, [write]),
    write_header_include_file(FileRef, Messages),
    protobuffs_file:close(FileRef);
write_header_include_file(_FileRef, []) ->
    ok;
write_header_include_file(FileRef, [{Name, Fields} | Tail]) ->
    OutFields = [{string:to_lower(A), Optional, Default} || {_, Optional, _, A, Default} <- lists:keysort(1, Fields)],
    DefName = string:to_upper(Name) ++ "_PB_H",
    protobuffs_file:format(FileRef, "-ifndef(~s).~n-define(~s, true).~n", [DefName, DefName]),
    protobuffs_file:format(FileRef, "-record(~s, {~n    ", [string:to_lower(Name)]),
    WriteFields = generate_field_definitions(OutFields),
    FormatString = string:join(["~s" || _ <- lists:seq(1, length(WriteFields))], ",~n    "),
    protobuffs_file:format(FileRef, FormatString, WriteFields),
    protobuffs_file:format(FileRef, "~n}).~n", []),
    protobuffs_file:format(FileRef, "-endif.~n~n", []),
    write_header_include_file(FileRef, Tail).

%% @hidden
write_pb_map_file(BaseName, Messages) when is_list(BaseName)->
    {ok, FileRef} = protobuffs_file:open(BaseName, [write]),
    protobuffs_file:format(FileRef, "-module(~s).~n", [filename:basename(BaseName, ".erl")]),
    protobuffs_file:format(FileRef, "-export([find/2]).~n", []),
    write_pb_map_file(FileRef, Messages),
    protobuffs_file:format(FileRef, "find(_,_)->[].",[]),
    protobuffs_file:close(FileRef);
write_pb_map_file(_FileRef, []) ->
    ok;
write_pb_map_file(FileRef, [{Name, Cmd, Module}|Tail])->
    Name1 = string:to_lower(Name),
    if
        Cmd =:= none ->
            ok;
        true->
            protobuffs_file:format(FileRef, "find(record2cmd,~s)->~s;~n",[Name1, integer_to_list(Cmd)]),
            protobuffs_file:format(FileRef, "find(cmd2record,~s)->~s;~n",[integer_to_list(Cmd), Name1])
    end,
    if
        Module =:= none ->
            ok;
        true->
            protobuffs_file:format(FileRef, "find(cmd2module,~s)->~s;~n",[integer_to_list(Cmd), Module])
    end,
    write_pb_map_file(FileRef, Tail).

%% @hidden
generate_field_definitions(Fields) ->
    generate_field_definitions(Fields, []).

%% @hidden
generate_field_definitions([], Acc) ->
    lists:reverse(Acc);
generate_field_definitions([{Name, required, none} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s = erlang:error({required, ~s})", [Name, Name])),
    generate_field_definitions(Tail, [Head | Acc]);
generate_field_definitions([{Name, _, none} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s", [Name])),
    generate_field_definitions(Tail, [Head | Acc]);
generate_field_definitions([{Name, _, Default} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s = ~p", [Name, Default])),
    generate_field_definitions(Tail, [Head | Acc]).

%% @hidden
% handle ["symbol"]
atomize([String]) when is_list(String) ->
    atomize(String);
% handle ["symbol", "package_symbol"]
atomize([String|[_Rest]]) when is_list(String) ->
    atomize(String);
atomize(String) ->
    list_to_atom(string:to_lower(String)).

%% @hidden
replace_atom(Find, Find, Replace) -> Replace;
replace_atom(Tuple, Find, Replace) when is_tuple(Tuple) ->
    list_to_tuple([replace_atom(Term, Find, Replace) || Term <- tuple_to_list(Tuple)]);
replace_atom(List, Find, Replace) when is_list(List) ->
    [replace_atom(Term, Find, Replace) || Term <- List];
replace_atom(Other, _Find, _Replace) ->
    Other.

