-module(test_gen_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init([]) ->  {ok, undefined}.

%% return inbound message and current state - reset state
handle_call(Msg, _From, State) -> {reply, {ack, Msg, State}, undefined}.

%% set state to inbound message
handle_cast(Msg, _State) -> {noreply, Msg}.

handle_info(Info, State) -> io:format("ignoring: ~p~n", [Info]), {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

