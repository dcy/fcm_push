# fcm_push
> Google FCM push server sdk for Erlang  
> 集成版本：https://github.com/dcy/epush    
> 使用例子：[/src/fcm_push_example.erl](/src/huawei_push_example.erl)

## Authorization 
* gen_authorization(AppSecret)
* gen_headers(AppSecret)

## push 通用推送接口
* push(Maps)
* push(AppSecret, Maps)
* push(AppSecret, Proxy, Maps)
```erlang
Notification = #{<<"title">> => unicode:characters_to_binary("中文标题"),
                 <<"body">> => unicode:characters_to_binary("中文内容")},
Msg = #{<<"to">> => list_to_binary(?DEVICE_TOKEN), <<"notification">> => Notification},
fcm_push:push(Msg).
```

## notification 通知栏
* notification(To, Title, Content)
* notification(AppSecret, To, Title, Content)
* notification(AppSecret, Proxy, To, Title, Content)
```erlang
fcm_push:notification(?DEVICE_TOKEN, "中文标题", "中文内容").
```

## data 透传
* data(To, Data)
* data(AppSecret, To, Data)
* data(AppSecret, Proxy, To, Data)
```erlang
fcm_push:data(?DEVICE_TOKEN, #{<<"hello">> => <<"world">>}).
```

## Todo:
- [ ] topics
