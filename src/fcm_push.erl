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
    AppSecret = get_conf_app_secret(),
    Proxy = get_conf_proxy(),
    push(AppSecret, Proxy, Maps).

push(AppSecret, Maps) ->
    push(AppSecret, undefined, Maps).

push(AppSecret, Proxy, Maps) ->
    Headers = gen_headers(AppSecret),
    send(Maps, Headers, Proxy).

%% 通知栏
notification(To, Title, Content) ->
    AppSecret = get_conf_app_secret(),
    Proxy = get_conf_proxy(),
    notification(AppSecret, Proxy, To, Title, Content).

notification(AppSecret, To, Title, Content) ->
    notification(AppSecret, undefined, To, Title, Content).

notification(AppSecret, Proxy, To, Title, Content) ->
    Headers = gen_headers(AppSecret),
    Notification = #{<<"title">> => unicode:characters_to_binary(Title),
                     <<"body">> => unicode:characters_to_binary(Content)},
    Msg = #{<<"to">> => list_to_binary(To), <<"notification">> => Notification},
    send(Msg, Headers, Proxy).

%% 透传

data(To, Data) ->
    AppSecret = get_conf_app_secret(),
    Proxy = get_conf_proxy(),
    data(AppSecret, Proxy, To, Data).

data(AppSecret, To, Data) ->
    data(AppSecret, undefined, To, Data).

data(AppSecret, Proxy, To, Data) ->
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

get_conf_app_secret() ->
    {ok, AppSecret} = application:get_env(fcm_push, app_secret),
    AppSecret.

get_conf_proxy() ->
    {ok, Proxy} = application:get_env(fcm_push, proxy),
    Proxy.

send(PayloadMaps, Headers) ->
    send(PayloadMaps, Headers, undefined).

%% Proxy: "127.0.0.1:1081"
send(PayloadMaps, Headers, Proxy) ->
    Method = post,
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

