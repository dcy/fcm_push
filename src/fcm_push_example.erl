-module(fcm_push_example).
-compile(export_all).

-define(DEVICE_TOKEN, "cGP8QEX4ZLU:APA91bGP-Z5tqCVDCJf_KW7jtY2gq9DxmCCObN2JylndcX7MhMwRkSYJr4Ev1zrliIUZP2sJUsTl98m6aAHmcua6J15QjI59daAQyQ0ir1J35ywpH_Be5S5E4XEGwHh8z_3H2B89KQWV").

push() ->
    Notification = #{<<"title">> => unicode:characters_to_binary("中文标题"),
                     <<"body">> => unicode:characters_to_binary("中文内容")},
    Msg = #{<<"to">> => list_to_binary(?DEVICE_TOKEN), <<"notification">> => Notification},
    fcm_push:push(Msg).



notification() ->
    fcm_push:notification(?DEVICE_TOKEN, "中文标题", "中文内容").


data() ->
    fcm_push:data(?DEVICE_TOKEN, #{<<"hello">> => <<"world">>}).
