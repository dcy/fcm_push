-module(fcm_push).

%%API
-export([notification/4, notification/5,
         unvarnished/3, unvarnished/4,
         topics/3, topics/4
        ]).

-export([gen_headers/1,
         send/2, send/3
        ]).


-define(URL, <<"https://fcm.googleapis.com/fcm/send">>).

%% 通知栏
notification(AppSecret, To, Title, Content) ->
    notification(AppSecret, undefined, To, Title, Content).

notification(AppSecret, Proxy, To, Title, Content) ->
    Headers = gen_headers(AppSecret),
    Notification = #{<<"title">> => unicode:characters_to_binary(Title),
                     <<"body">> => unicode:characters_to_binary(Content)},
    Msg = #{<<"to">> => list_to_binary(To), <<"notification">> => Notification},
    send(Msg, Headers, Proxy).

%% 透传
unvarnished(AppSecret, To, Data) ->
    unvarnished(AppSecret, undefined, To, Data).

unvarnished(AppSecret, Proxy, To, Data) ->
    Msg = #{<<"to">> => list_to_binary(To), <<"data">> => Data},
    Headers = gen_headers(AppSecret),
    send(Msg, Headers, Proxy).


%% 主题
topics(AppSecret, Topics, Data) ->
    topics(AppSecret, undefined, Topics, Data).

topics(AppSecret, Proxy, Topics, Data) ->
    ok.



gen_authorization(AppSecret) ->
    <<"key=", (list_to_binary(AppSecret))/binary>>.

gen_headers(AppSecret) ->
    Auth = gen_authorization(AppSecret),
    [{<<"Content-Type">>, <<"application/json; charset=utf-8">>},
     {<<"Authorization">>, Auth}].


send(PayloadMaps, Headers) ->
    send(PayloadMaps, Headers, undefined).

%% Proxy: "127.0.0.1:1081"
send(PayloadMaps, Headers, Proxy) ->
    Method = post,
    Payload = jiffy:encode(PayloadMaps),
    Options = case Proxy of
                  undefined ->[{pool, fcm}];
                  _ -> [{pool, default}, {proxy, Proxy}]
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

