-module(router).
-behaviour(gen_server).

%
% list of functions
%
-export([
    code_change/3,
    handle_call/3,
    login/3,
    new_message/3,
    remove_message/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    ticket_decode/2,
    init/1,
    logout/1,
    static_gamma1/0,
    static_gamma2/0,
    start_link/0
]).

%
% list of atoms
%
-define(PID2ID, comet_pid2id).
-define(ID2PID, comet_id2pid).
-define(ID2MSG, comet_id2msg).

% will hold bidirectional mapping between id <--> pid
-record(state, {pid2id, id2pid, id2msg}).

%
% start link function
%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%
% login
%
% @param Id user id
% @param Pid process id
% @param Mid last message id
%
login(Id, Pid, Mid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {login, Id, Pid, Mid}).

%
% logout
%
% @param Pid process id
%
logout(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {logout, Pid}).

%
% initial function
%
init([]) ->
    % set this so we can catch death of logged in pids:
    process_flag(trap_exit, true),
    % use ets for routing tables
    {ok, #state{
              }
    }.

%
% handle call for login function
%
handle_call({login, Id, Pid, Mid}, _From, State) when is_pid(Pid) ->
    Msgs = ets:select(?ID2MSG, [{{Id, '$1', '$2'}, [{'>', '$2', Mid}], ['$1']}]),

    if
       Msgs == [] ->
           insert_to_ets(Pid, Id);
       true ->
           MsgsNew = lists:map(fun(X)-> ""++X++"" end, Msgs),
           Msg = string:join(MsgsNew, ","),
           M = {send_msg, Msg},
           Pid ! M
       end,
    {reply, ok, State};

%
% handle call for logout function
%
handle_call({logout, Pid}, _From, State) when is_pid(Pid) ->
    unlink(Pid),
    PidRows = ets:lookup(?PID2ID, Pid),
    case PidRows of
        [] ->
            ok;
        _ ->
            IdRows = [ {I,P} || {P,I} <- PidRows ], % invert tuples
            % delete all pid->id entries
            ets:delete(?PID2ID, Pid),
            % and all id->pid
            [ ets:delete_object(?ID2PID, Obj) || Obj <- IdRows ]
    end,
    %io:format("pid ~w logged out\n",[Pid]),
    {reply, ok, State}.

%
% handle death and cleanup of logged in processes
%
handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, _Why} ->
            % force logout:
            handle_call({logout, Pid}, blah, State);
        Wtf ->
            io:format("Caught unhandled message: ~w\n", [Wtf])
    end,
    {noreply, State}.

%
% handle cast for msg
%
% @param _Msg
% @param State
%
handle_cast(_Msg, State) ->
    {noreply, State}.

%
% terminate function
%
% @param _Reason
% @param _State
%
terminate(_Reason, _State) ->
    ok.
   
%
% insert Pid and Id to ets-table
%
% @param Pid process id
% @param Id user id
%
insert_to_ets(Pid,Id) ->
     ets:insert(?PID2ID, {Pid, Id}),
     ets:insert(?ID2PID, {Id, Pid}),
     link(Pid). % tell us if they exit, so we can log them out
     %io:format("~w logged in as ~w\n",[Pid, Id]).

%
% new message
%
% @param Id user id
% @param Mid message id
% @param Msg message
%
new_message(Id, Mid, Msg) ->
    % get pids who are logged in as this Id
    %io:format("~w Recvd msg ~w: '~s'\n", [Id, Mid, Msg]),

    Pids = [ P || { _Id, P } <- ets:lookup(?ID2PID, Id) ],
    ets:insert(?ID2MSG, {Id, Msg, Mid}),

    M = {send_msg, ""++Msg++""},
    [ Pid ! M || Pid <- Pids ].

%
% remove message
%
% @param Id user id
% @param Mid message id
% @param Msg message
%
remove_message(Id, Mid, Msg) ->
    %io:format("~w Remove msg ~w: '~s'\n", [Id, Mid, Msg]),
    ets:delete_object(?ID2MSG, {Id, Msg, Mid}).

%
% This is function for remove
%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% static gamma for ticket decoding for authorization
static_gamma() ->
    "static_gamma".

%
% decoding of ticket.
%
% !!!!!! You need to write here your own algorythm to decode Ticket and return UserId at the end !!!!!!
%
% @param Ticket hex integer
% @param T hex integer
%
ticket_decode(Ticket, T) ->

    UserId = Ticket,

    UserId.
