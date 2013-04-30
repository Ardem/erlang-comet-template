-module(server_web).

-export([loop/2, start/1, stop/0]).

-define(SEND_TIMEOUT, 120000).
-define(REQUEST_TIMEOUT, 50000).

%% External API

%
% start function
%
% @param Options
%
start(Options) ->

    ets:new(comet_id2pid, [bag, public, named_table]),
    ets:new(comet_pid2id, [bag, public, named_table]),
    ets:new(comet_id2msg, [bag, public, named_table]),

    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    % we'll set our maximum to 1 million connections. (default: 2048)
    mochiweb_http:start([{max, 1000000}, {name, ?MODULE}, {loop, Loop} | Options1]).

%
% stop the daemon
%
stop() ->
    mochiweb_http:stop(?MODULE).

%
% loop
%
% @param Req request
% @param _DocRoot
%
loop(Req, _DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD'; Method =:= 'POST' ->
            case Path of
                "getmessages" ->
                    try
                        Struct = mochijson2:decode(Req:recv_body()),
                        {struct, JsonData} = Struct,
                        HexTicket = proplists:get_value(<<"ticket">>, JsonData),
                        TJson = proplists:get_value(<<"t">>, JsonData),

                        if
                            is_binary(TJson) ->
                                T = list_to_integer(binary_to_list(TJson));
                            true ->
                                T = TJson
                        end,

                        MJson = proplists:get_value(<<"m">>, JsonData),
                        if
                            is_binary(MJson) ->
                                Mid = list_to_integer(binary_to_list(MJson));
                            true ->
                                Mid = MJson
                        end,

                        Id = router:ticket_decode(binary_to_list(HexTicket), T),
                     
                        if
                            is_integer(Id) ->
                                if
                                    Id == 0, Mid == 0 ->
                                        Req:respond({501, [], []});
                                    true ->
                                        router:login(Id, self(), Mid),
                                        feed(Req)
                                    end;
                            true ->
                                Req:respond({400, [], []})
                       end
                   catch 
                        _Error:_Reason -> Req:respond({400, [], []})
                   end;
             "send" ->
                   Req:respond({200, [], []}),
                   PostData = Req:parse_post(),

                   Mid = list_to_integer(proplists:get_value("mid", PostData)),
                   Uid  = list_to_integer(proplists:get_value("uid", PostData)),
                   Msg = proplists:get_value("message", PostData),

                   if
                       is_integer(Uid), is_integer(Mid) ->
                           router:new_message(Uid, Mid, Msg),
                          
	                   receive
                           after
                               ?SEND_TIMEOUT ->
	                           router:remove_message(Uid, Mid, Msg)                           
			   end
                     end;
               "ping" ->
                    Req:respond({200, [], []});
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%
% feed
%
% @param Req request
%
feed(Req) ->
    receive
    {send_msg, Msg} ->
        %io:format("~w Recvd2 msg: '~s'\n", [self(), Msg]),
        router:logout(self()),

        Req:respond({200, [{"Content-Type", "application/json"}, {"Access-Control-Allow-Origin", "*"}],
              "[" ++ Msg ++ "]"})
    after 
        ?REQUEST_TIMEOUT ->
            %io:format("timeout"),
            router:logout(self()),
            Req:respond({200, [{"Content-Type", "application/json"}, {"Access-Control-Allow-Origin", "*"}],  
            "{\"response\":0}"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
