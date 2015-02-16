#!/usr/bin/env escript
%%! -sname mule_test_server_node_ctl@localhost -setcookie mule_test_cookie

main(["test_send"]) -> test_send();
main(["test_gs_cast"]) -> test_gs_cast();
main(["test_gs_call"]) -> test_gs_call();
main(["stop"]) -> stop();
main(["info"]) -> io:format("Node: ~p Cookie: ~p~n", [node(), erlang:get_cookie()]);
main(_) -> usage().

usage() ->
  io:format("usage: test_server_ctl stop~n"),
  io:format("       test_server_ctl test_send~n"),
  io:format("       test_server_ctl test_gs_cast~n"),
  io:format("       test_server_ctl test_gs_call~n"),
  halt(1).

stop() ->
  erlang:send({mule_test_server, server_node()}, {self(), stop}),

  receive
    stopped -> io:format("test_server stopped~n"), halt(0)
  after
    1000 -> halt(1)
  end.

test_send() ->
  erlang:send({mule_test_server, server_node()}, {self(), testing}),

  receive
    {_Pid, {raw_ack, testing, _State}} -> io:format("send ok~n"), halt(0)
  after
    1000 -> halt(1)
  end.

test_gs_cast() ->
  case gen_server:cast({mule_test_gen_server, server_node()}, gs_cast) of
    ok -> io:format("gen_server cast ok~n"), timer:sleep(500), halt(0);
    _ -> halt(1)
  end.

test_gs_call() ->
  case gen_server:call({mule_test_gen_server, server_node()}, gs_call) of
    {gs_ack, gs_call, State} -> io:format("gen_server call ok with state: ~p~n", [State]), halt(0);
    _ -> halt(1)
  end.

server_node() ->
  list_to_atom("mule_test_server_node@localhost").

