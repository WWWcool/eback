-module(json_proto).

-export([decode/1]).
-export([encode/1]).

-spec decode(binary()) ->
    map().

decode(Json) ->
    jiffy:decode(Json, [return_maps]).

-spec encode({atom(), term()}) ->
    binary().

encode({Type, Data} = Data) when is_tuple(Data) ->
    jiffy:encode(#{type => Type, data => Data});
encode(Data) when is_map(Data) ->
    jiffy:encode(#{message => Data});
encode(Data) ->
    jiffy:encode(#{encode_error_of => Data}).
