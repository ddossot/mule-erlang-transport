#!/usr/bin/env escript
%%! -sname mule_test_server_ctl -setcookie mule_test

main(["stop"]) -> stop();
main(_) -> usage().

usage() ->
  io:format("usage: test_server stop~n"),
  halt(1).

stop() ->
  ServerNode = list_to_atom("mule_test_server@" ++ net_adm:localhost()),
  rpc:call(ServerNode, erlang, send, [mule_test_server, {self(), stop}]),

  receive
    stopped -> io:format("test_server stopped~n"), halt(0)
  after
    1000 -> halt(1)
  end.

