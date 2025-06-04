-module(rpc_server_app_tests).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [
        init_test
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_test(_Config) ->
    {ok, _} = application:ensure_all_started(rpc_server),
    ct:pal("Application started"),
    application:stop(rpc_server),
    timer:sleep(100),
    false = is_process_alive(whereis(rpc_server_sup)).



