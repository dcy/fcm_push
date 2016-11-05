-module(fcm_push).

%%API
-export([push/1, push/2, push/3,
         notification/3, notification/4, notification/5,
         data/2, data/3, data/4,
         topics/3, topics/4
        ]).

-export([gen_headers/1,
         gen_authorization/1,
         send/2, send/3
        ]).

-include_lib("eutil/include/eutil.hrl").

-define(URL, <<"https://fcm.googleapis.com/fcm/send">>).

%% 通用API
push(Maps) ->
    ApiKey = get_conf_api_key(),
    Proxy = get_conf_proxy(),
    push(ApiKey, Proxy, Maps).

push(ApiKey, Maps) ->
    push(ApiKey, undefined, Maps).

push(ApiKey, Proxy, Maps) ->
    send(ApiKey, Proxy, Maps).

%% 通知栏
notification(To, Title, Content) ->
    ApiKey = get_conf_api_key(),
    Proxy = get_conf_proxy(),
    notification(ApiKey, Proxy, To, Title, Content).

notification(ApiKey, To, Title, Content) ->
    notification(ApiKey, undefined, To, Title, Content).

notification(ApiKey, Proxy, To, Title, Content) ->
    Notification = #{<<"title">> => unicode:characters_to_binary(Title),
                     <<"body">> => unicode:characters_to_binary(Content),
                     <<"sound">> => <<"default">>},
    Msg = #{<<"to">> => list_to_binary(To), <<"notification">> => Notification},
    send(ApiKey, Proxy, Msg).

%% 透传

data(To, Data) ->
    ApiKey = get_conf_api_key(),
    Proxy = get_conf_proxy(),
    data(ApiKey, Proxy, To, Data).

data(ApiKey, To, Data) ->
    data(ApiKey, undefined, To, Data).

data(ApiKey, Proxy, To, Data) ->
    Msg = #{<<"to">> => list_to_binary(To), <<"data">> => Data},
    send(ApiKey, Proxy, Msg).


%% 主题
topics(ApiKey, Topics, Data) ->
    topics(ApiKey, undefined, Topics, Data).

topics(ApiKey, Proxy, Topics, Data) ->
    ok.



gen_authorization(ApiKey) ->
    <<"key=", (eutil:to_binary(ApiKey))/binary>>.

gen_headers(ApiKey) ->
    Auth = gen_authorization(ApiKey),
    [{<<"Content-Type">>, <<"application/json; charset=utf-8">>},
     {<<"Authorization">>, Auth}].

get_conf_api_key() ->
    {ok, ApiKey} = application:get_env(fcm_push, api_key),
    ApiKey.

get_conf_proxy() ->
    {ok, Proxy} = application:get_env(fcm_push, proxy),
    Proxy.

send(ApiKey, PayloadMaps) ->
    send(ApiKey, undefined, PayloadMaps).

%% Proxy: "127.0.0.1:1081"
send(ApiKey, Proxy, PayloadMaps) ->
    Method = post,
    Headers = gen_headers(ApiKey),
    Payload = jiffy:encode(PayloadMaps),
    Options = case Proxy of
                  undefined ->[{pool, fcm}];
                  _ -> [{pool, fcm}, {proxy, Proxy}]
              end,
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, ?URL, Headers,
                                                                Payload, Options),
    %%todo: 返回待商讨
    case StatusCode of
        200 ->
            {ok, ResultBin} = hackney:body(ClientRef),
            Result = jiffy:decode(ResultBin, [return_maps]),
            case maps:get(<<"success">>, Result) of
                1 ->
                    ok;
                0 ->
                    lager:error("fcm_push error, PayloadMaps:~p, Result: ~p", [PayloadMaps, Result]),
                    ok 
            end;
        _ ->
            lager:error("fcm_push error, StatusCode: ~p, PayloadMaps: ~p", [StatusCode, PayloadMaps]),
            ok
    end.

