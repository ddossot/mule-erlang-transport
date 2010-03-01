#!/usr/bin/env escript
%%! -sname mule_test_server_node -setcookie mule_test_cookie -detached

main(_) ->
  compile:file(test_gen_server.erl),
  gen_server:start_link({local, mule_test_gen_server}, test_gen_server, [], []),
  register(mule_test_server, self()),
  io:format("test_server started~n"),
  loop().

loop() ->
  receive
    {Pid, stop} -> io:format("stopping test_server~n"), unregister(mule_test_server), Pid ! stopped;
    {Pid, Msg} -> io:format("replying to: ~p~n", [Pid]), Pid ! {self(), {ack, Msg}}, loop();
    Other -> io:format("ignoring: ~p~n", [Other]), loop()
  end.

