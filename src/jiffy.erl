% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy).
-export([decode/1, decode/2, encode/1, encode/2, encode_stream_continue/2, encode_stream_end/1]).
-define(NOT_LOADED, not_loaded(?LINE)).

-compile([no_native]).

-on_load(init/0).


-type json_value() :: null
                    | true
                    | false
                    | json_string()
                    | json_number()
                    | json_object()
                    | json_array().

-type json_array()  :: [json_value()].
-type json_string() :: atom() | binary().
-type json_number() :: integer() | float().

-ifdef(JIFFY_NO_MAPS).

-type json_object() :: {[{json_string(),json_value()}]}.

-else.

-type json_object() :: {[{json_string(),json_value()}]}
                        | #{json_string() => json_value()}.

-endif.

-type jiffy_decode_result() :: json_value()
                        | {has_trailer, json_value(), binary()}.

-type decode_option() :: return_maps
                        | use_nil
                        | return_trailer
                        | dedupe_keys
                        | {null_term, any()}
                        | {bytes_per_iter, non_neg_integer()}
                        | {bytes_per_red, non_neg_integer()}.

-type encode_option() :: uescape
                        | pretty
                        | force_utf8
                        | use_nil
                        | escape_forward_slashes
                        | {bytes_per_iter, non_neg_integer()}
                        | {bytes_per_red, non_neg_integer()}.

-type decode_options() :: [decode_option()].
-type encode_options() :: [encode_option()].

-export_type([json_value/0, jiffy_decode_result/0]).


-spec decode(iolist() | binary()) -> jiffy_decode_result().
decode(Data) ->
    decode(Data, []).


-spec decode(iolist() | binary(), decode_options()) -> jiffy_decode_result().
decode(Data, Opts) when is_binary(Data), is_list(Opts) ->
    case nif_decode_init(Data, Opts) of
        {error, _} = Error ->
            throw(Error);
        {partial, EJson} ->
            finish_decode(EJson);
        {iter, Decoder, Val, Objs, Curr} ->
            decode_loop(Data, Decoder, Val, Objs, Curr);
        EJson ->
            EJson
    end;
decode(Data, Opts) when is_list(Data) ->
    decode(iolist_to_binary(Data), Opts).


-spec encode(json_value()) -> iodata().
encode(Data) ->
    encode(Data, []).


-spec encode(json_value(), encode_options()) -> iodata().
encode(Data, Options) ->
    ForceUTF8 = lists:member(force_utf8, Options),
    Stream = lists:keyfind(stream, 1, Options),
    case {Stream, ForceUTF8} of
        {{stream, _}, true} ->
            throw({badarg, "stream and force_utf8 do not work together"});
        _ ->
            ok
    end,
    encode_response(nif_encode_init(Data, Options), Data, Options, ForceUTF8).


encode_stream_continue(Data, {Encoder, Stack, ForceUTF8}) ->
    encode_response(nif_encode_stream_continue(Encoder, Stack, Data), Data, {stream_cont, Encoder, Stack}, ForceUTF8).


encode_stream_end({Encoder, Stack, ForceUTF8}) ->
    encode_response(nif_encode_stream_end(Encoder, Stack), <<>>, {stream_end, Encoder, Stack}, ForceUTF8).


finish_decode({bignum, Value}) ->
    list_to_integer(binary_to_list(Value));
finish_decode({bignum_e, Value}) ->
    {IVal, EVal} = case string:to_integer(binary_to_list(Value)) of
        {I, [$e | ExpStr]} ->
            {E, []} = string:to_integer(ExpStr),
            {I, E};
        {I, [$E | ExpStr]} ->
            {E, []} = string:to_integer(ExpStr),
            {I, E}
    end,
    IVal * math:pow(10, EVal);
finish_decode({bigdbl, Value}) ->
    list_to_float(binary_to_list(Value));
finish_decode({Pairs}) when is_list(Pairs) ->
    finish_decode_obj(Pairs, []);
finish_decode(Vals) when is_list(Vals) ->
    finish_decode_arr(Vals, []);
finish_decode(Val) ->
    maybe_map(Val).

-ifndef(JIFFY_NO_MAPS).
maybe_map(Obj) when is_map(Obj) ->
    maps:map(fun finish_decode_map/2, Obj);
maybe_map(Val) ->
    Val.

finish_decode_map(_, V) ->
    finish_decode(V).
-else.
maybe_map(Val) ->
    Val.
-endif.

finish_decode_obj([], Acc) ->
    {lists:reverse(Acc)};
finish_decode_obj([{K, V} | Pairs], Acc) ->
    finish_decode_obj(Pairs, [{K, finish_decode(V)} | Acc]).

finish_decode_arr([], Acc) ->
    lists:reverse(Acc);
finish_decode_arr([V | Vals], Acc) ->
    finish_decode_arr(Vals, [finish_decode(V) | Acc]).


finish_encode([], Acc) ->
    %% No reverse! The NIF returned us
    %% the pieces in reverse order.
    Acc;
finish_encode([<<_/binary>>=B | Rest], Acc) ->
    finish_encode(Rest, [B | Acc]);
finish_encode([Val | Rest], Acc) when is_integer(Val) ->
    Bin = list_to_binary(integer_to_list(Val)),
    finish_encode(Rest, [Bin | Acc]);
finish_encode([InvalidEjson | _], _) ->
    throw({error, {invalid_ejson, InvalidEjson}});
finish_encode(_, _) ->
    throw({error, invalid_ejson}).


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "jiffy"), 0).


decode_loop(Data, Decoder, Val, Objs, Curr) ->
    case nif_decode_iter(Data, Decoder, Val, Objs, Curr) of
        {error, _} = Error ->
            throw(Error);
        {partial, EJson} ->
            finish_decode(EJson);
        {iter, NewDecoder, NewVal, NewObjs, NewCurr} ->
            decode_loop(Data, NewDecoder, NewVal, NewObjs, NewCurr);
        EJson ->
            EJson
    end.


encode_response({error, {invalid_string, _}}, Data, Options, true) ->
    case Options of
        {stream_end, _Encoder, _Stack} ->
            % There is no easy way to support force_utf8 + stream for this case,
            % do disable it
            throw({error, internal_error});
        {stream_cont, _Encoder, _Stack} ->
            throw({error, internal_error});
        Options when is_list(Options) ->
            FixedData = jiffy_utf8:fix(Data),
            encode(FixedData, Options -- [force_utf8])
    end;
encode_response({error, {invalid_object_member_key, _}}, Data, Options, true) ->
    FixedData = jiffy_utf8:fix(Data),
    encode(FixedData, Options -- [force_utf8]);
encode_response({error, _} = Error, _Data, _Options, _ForceUTF8) ->
    throw(Error);
encode_response({iter, Encoder, Stack, IOBuf}, Data, Options, _ForceUTF8) ->
    encode_loop(Data, Options, Encoder, Stack, IOBuf);
encode_response({stream, IOData, Encoder, Stack}, _Data, _Options, ForceUTF8) ->
    {maybe_finish_encode(IOData), {Encoder, Stack, ForceUTF8}};
encode_response(IOData, _Data, _Options, _ForceUTF8) ->
    maybe_finish_encode(IOData).


maybe_finish_encode({partial, IOData}) ->
    finish_encode(IOData, []);
maybe_finish_encode(IOData) ->
    IOData.

encode_loop(Data, Options, Encoder, Stack, IOBuf) ->
    ForceUTF8 = lists:member(force_utf8, Options),
    encode_response(nif_encode_iter(Encoder, Stack, IOBuf), Data, Options, ForceUTF8).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_decode_init(_Data, _Opts) ->
    ?NOT_LOADED.

nif_decode_iter(_Data, _Decoder, _, _, _) ->
    ?NOT_LOADED.

nif_encode_init(_Data, _Options) ->
    ?NOT_LOADED.

nif_encode_stream_continue(_Encoder, _Stack, _Data) ->
    ?NOT_LOADED.

nif_encode_stream_end(_Encoder, _Stack) ->
    ?NOT_LOADED.

nif_encode_iter(_Encoder, _Stack, _IoList) ->
    ?NOT_LOADED.
