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

startChannel(Channel) ->
    genserver:start(Channel, initial_channel(), fun channelHandle/2)

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
           {reply, Response, State}
    end.

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Server does not handle this command"}, St} .
