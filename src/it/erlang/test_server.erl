#!/usr/bin/env escript
%%! -sname mule_test_server -setcookie mule_test

main(_) ->
  io:format("test_server started~n"),
  global:register_name(mule_test_server, self()),
  loop().

loop() ->
  receive
    {Pid, stop} -> io:format("stopping test_server~n"), Pid ! stopped;
    {Pid, Msg} -> Pid ! {self(), {ack, Msg}}, loop();
    Other -> io:format("ignoring: ~p~n", [Other]), loop()
  end.

