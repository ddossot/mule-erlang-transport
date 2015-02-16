#!/usr/bin/env escript
%%! -sname mule_test_server_node -setcookie mule_test_cookie -detached

main(_) ->
  compile:file("test_gen_server.erl"),
  gen_server:start_link({local, mule_test_gen_server}, test_gen_server, [], []),
  register(mule_test_server, self()),
  io:format("test_server started~n"),
  loop(undefined).

loop(State) ->
  receive
    {Pid, stop} ->
      io:format("stopping test_server~n"),
      unregister(mule_test_server),
      Pid ! stopped;

    {Pid, Msg} ->
      io:format("replying to: ~p~n", [Pid]), Pid ! {self(), {raw_ack, Msg, State}},
      loop(Msg);

    Text when is_list(Text) ->
      io:format("calling capitalizer for: ~p~n", [Text]),
      MuleNode = mule_node(),
      CapitalizedText = gen_server:call({capitalizer, MuleNode}, Text),
      gen_server:cast({jms_bridge, MuleNode}, CapitalizedText),
      {jms_bridge, MuleNode}!lists:reverse(CapitalizedText),
      % PID wrapped to an async endpoint -> no reply
      {jms_bridge, MuleNode}!{self(), string:to_upper(Text)},

      loop(undefined);

    Other ->
      io:format("ignored: ~p~n", [Other]),
      loop(State)
  end.

mule_node() ->
  list_to_atom("MuleIT@" ++ net_adm:localhost()).

