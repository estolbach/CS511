-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to leave a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    ChatPID = case maps:is_key(ChatName, State#serv_st.chatrooms) of
        false -> spawn(chatroom, start_chatroom, [ChatName]);
        true -> {ok, CPID} = maps:find(ChatName, State#serv_st.chatrooms), CPID
    end,
    % look up client's nickname
    ClientNick = maps:get(ClientPID, State#serv_st.nicks),
    % tell chatroom that client is joining
    ChatPID!{self(), Ref, register, ClientPID, ClientNick},
    % server updates it's record of chatroom registrations to include the client in the list of clients registered to that chatroom.
    RegClients = case maps:find(ChatName, State#serv_st.registrations) of
        {ok, ClientsList} -> ClientsList;
        error -> []
    end,
    State1 = State#serv_st{chatrooms = maps:put(ChatName, ChatPID, State#serv_st.chatrooms),
                           registrations = maps:put(ChatName, RegClients ++ [ClientPID], State#serv_st.registrations)},
    State1.



%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    % look up chatroom’s PID
    ChatPID = case maps:is_key(ChatName, State#serv_st.chatrooms) of
        false -> {err, State};
        true -> {ok, CPID} = maps:find(ChatName, State#serv_st.chatrooms), CPID
    end,
    % remove the client from its local record of chatroom registrations.
    Remove = maps:remove(ClientPID, State#serv_st.registrations),
    State1 = State#serv_st{registrations = Remove},
    % server sends message to chatroom
    ChatPID!{self(), Ref, unregister, ClientPID},
    % server sends message to client
    ClientPID!{self(), Ref, ack_leave},
    State1.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    % check if the new nickname Nick is already used
    NickList = maps:values(State#serv_st.nicks),
    case lists:member(NewNick, NickList) of
        false ->
            % server update it’s record of nicknames by pointing ClientPID
            % to the new nickname
            UpdateName = maps:put(ClientPID, NewNick, State#serv_st.nicks),
            State1 = State#serv_st{nicks = UpdateName},

            % server finds all associated chatrooms
            lists:foreach(
                fun(Tuple) ->
                    {Key,Val} = Tuple,
                    case lists:member(ClientPID, Val) of
                        false -> pass;
                        true ->
                            ChatPid = maps:get(Key, State#serv_st.chatrooms),
                            ChatPid!{self(), Ref, update_nick, ClientPID, NewNick}
                    end
                end,
            maps:to_list(State#serv_st.registrations)),
            % send message to client
            ClientPID!{self(), Ref, ok_nick},
            State1;
        true -> % found nickname does exist
            ClientPID!{self(), Ref, err_nick_used},
            State
    end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    % Remove client from nicknames.
    State1 = State#serv_st{nicks = maps:remove(ClientPID, State#serv_st.nicks)},
    % server sends message to chatroom
    lists:foreach(
        fun(Key) ->
            case lists:member([ClientPID], maps:values(State#serv_st.registrations)) of
                false -> pass;
                true ->
                    ChatPid = maps:get(Key, State#serv_st.chatrooms),
                    do_leave(ChatPid, ClientPID, Ref,State)
            end
        end,
        maps:keys(State#serv_st.registrations)),
    ClientPID!{self(), Ref, ack_quit},
    State1.