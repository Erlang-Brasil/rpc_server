-module(rpc_server_sctp_listen_tests).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [
        test_is_running,
        test_stop_server
    ].

init_per_suite(Config) ->
    application:ensure_all_started(rpc_server),
    Config.

end_per_suite(_Config) ->
    application:stop(rpc_server),
    ok.

test_is_running(_Config) ->
    Pid = whereis(rpc_server_sctp_listen),
    ?assert(is_pid(Pid)),
    ?assert(process_info(Pid) =/= undefined).

test_stop_server(_Config) ->
    OldPid = whereis(rpc_server_sctp_listen),
    ?assert(is_pid(OldPid)),
    ?assertEqual(stopped, gen_server:call(OldPid, stop)),
    timer:sleep(200),
    NewPid = whereis(rpc_server_sctp_listen),
    ?assert(is_pid(NewPid)),
    ?assertNotEqual(OldPid, NewPid). 