-module(server).
-export([start/1,stop/1]).

% State of the server, consists of channels
-record(server_st, {
    channelList
}).

% State of a channel, consists of users
% !! We might be better off with creating a separate file for handling channels?!
-record(channel_st, {
   userList
}).

% Return initial state record of the server
initial_state() ->
    #server_st{
        channelList = []
    }.

% Return initial state record of the channel
initial_channel() -> 
    #channel_st {
        userList = []   
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).

% Start a new channel process
startChannel(Channel) ->
    genserver:start(list_to_atom(Channel), initial_channel(), fun channelHandle/2).

handle(St, {join, Channel, Nickname, Pid}) ->
    case lists:member(Channel, St#server_st.channelList) of
        % If a channel does not exist in the server, create one and it to the channels list
        false ->
            startChannel(Channel),
            NewChannelList = lists:append([Channel], St#server_st.channelList),
            Response = genserver:request(list_to_atom(Channel), {join, Pid}),
            {reply, Response, St#server_st{channelList = NewChannelList}};
        % If the channel exists, proceed normally
        true ->
            Response = genserver:request(list_to_atom(Channel), {join, Pid}),
           {reply, Response, St}
    end.

channelHandle(St, {join, Pid}) ->
    case lists:member(Pid, St#channel_st.userList) of
        % If user is already in channel, send appropriate error message
        true ->
            {reply, {error, user_already_joined, "User is already in the channel"}, St};
        % If user is not in channel, add his Pid to the list of users 
        false ->
            NewUserList = lists:append([Pid], St#channel_st.userList),
            {reply, ok, St#channel_st{userList = NewUserList}}
    end;

channelHandle(St, {leave, Channel, Nickname, Pid}) ->
    case lists:member(Pid, St#channel_st.userList) of
        % If user is in channel, update the user list by removing his Pid 
        true -> 
            NewUserList = lists:delete(Pid, St#channel_st.userList),
            {reply, ok, St#channel_st{userList = NewUserList}};
        % If user is not in channel, send appropriate error message
        false -> 
            {reply, {error, user_not_joined, "User is not in channel"}, St}
    end;

channelHandle(St, {message_send, Channel, Nickname, Pid, Msg}) ->

    case lists:member(Pid, St#channel_st.userList) of
        true ->
            Data = {request, self(), make_ref(), {message_receive, Channel, Nickname, Msg}},
            % Get the list of users in the channel except for the sender
            UsersInChannel = lists:delete(Pid, St#channel_st.userList),
            % Send the message to each user in the channel (except for the sender)
            % [User ! Data || User <- UsersInChannel]
            lists:foreach((fun(User) -> User ! Data end), UsersInChannel),
            {reply, ok, St};
        false ->
            {reply, {error, user_not_joined, "User is not in channel"}, St}
    end.
