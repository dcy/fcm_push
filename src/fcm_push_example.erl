-module(fcm_push_example).
-compile(export_all).

-define(DEVICE_TOKEN, "fROu070O3wU:APA91bHoc3ra1kxoRFA85NhLuLRYFhvv4AK0sfeOepAPSHuL6pBZ6ds4u8iW4WMFnlLon8x7QkYx1xSNQLfezj1_tt7Al55V5NqVqYBuMpB8jrtPy3Jt3YB8qIEelf9Jt2hPrI0_8tRB").

push() ->
    Notification = #{<<"title">> => unicode:characters_to_binary("中文标题"),
                     <<"body">> => unicode:characters_to_binary("中文内容")},
    Msg = #{<<"to">> => list_to_binary(?DEVICE_TOKEN), <<"notification">> => Notification},
    fcm_push:push(Msg).



notification() ->
    fcm_push:notification(?DEVICE_TOKEN, "中文标题", "中文内容").


%data() ->
%    fcm_push:data(?DEVICE_TOKEN, #{<<"hello">> => <<"world">>}).
data() ->
    fcm_push:data(?DEVICE_TOKEN, <<"hello world">>).

app_msg() ->
    fcm_push:app_msg(?DEVICE_TOKEN, #{<<"Hello">> => <<"World">>}).
