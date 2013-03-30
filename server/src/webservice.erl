-module( webservice ).
-export( [ start/0, webservice/0 ] ).

%
% atoms
%
-define(TIMEOUT, 60000).
-define(PID2ID, comet_pid2id).
-define(ID2PID, comet_id2pid).
-define(ID2MSG, comet_id2msg).

%
% start function for web server
%
start() ->
    Mypid = spawn( ?MODULE, ?MODULE, [ ] ),
    register(?MODULE, Mypid),
    {ok, Mypid}.

%
% webservice
%
webservice() ->
    receive
        quit -> ok;
        [push, Id, Mid, Msg, Pid] ->
            try
                push(list_to_integer(Id), list_to_integer(Mid), Msg),
                Pid! [self(), "200"]
             catch 
                _Error:_Reason -> Pid! [self(), "400 Bad request"]
            end,
            webservice();
        [remove, Id, Mid, Msg] ->
            remove(Id, Mid, Msg),
            webservice()
    end.

%
% push message
%
% @param Id user id
% @param Mid message id
% @param Msg message
%
push(Id, Mid, Msg) ->
    %io:fwrite( "Got <~p> <~p> <~p>.~n", [ Id, Mid, Msg ] ),
    send(Id, Mid, Msg).

%
% send message
%
% @param Id user id
% @param Mid message id
% @param Msg message
%
send(Id, Mid, Msg) ->
    % get pids who are logged in as this Id
    Pids = [ P || { _Id, P } <- ets:lookup(?ID2PID, Id) ],
    ets:insert(?ID2MSG, {Id, Msg, Mid}),

    erlang:send_after(?TIMEOUT, self(), [remove, Id, Mid, Msg]),
    M = {send_msg, ""++Msg++""},
    [ Pid ! M || Pid <- Pids ].

%
% remove message
%
% @param Id user id
% @param Mid message id
% @param Msg message
%
remove (Id, Mid, Msg) ->
    ets:delete_object(?ID2MSG, {Id, Msg, Mid}).
    %io:format("~w removed out\n",[Id]).
