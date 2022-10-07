-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Make a request and wait for a response
request(Atom, Request) ->
    try genserver:request(Atom, Request) of
        Response -> Response
    catch 
        _:_ -> {error, server_not_reached, "Server not reached"}
    end.


% Join channel
handle(St, {join, Channel}) ->
    case request(St#client_st.server, {join, Channel, St#client_st.nick, self()}) of
        ok -> {reply, ok, St};
        Error -> {reply, Error, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    case request(list_to_atom(Channel), {leave, Channel, St#client_st.nick, self()}) of
        ok -> {reply, ok, St};
        {error, user_not_joined, Msg} -> {reply, {error, user_not_joined, Msg}, St};
        _ -> {reply, ok, St} % not sure if we need that, test without!
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    ChannelReply = request(list_to_atom(Channel), {message_send, Channel, St#client_st.nick, self(), Msg}),
    {reply, ChannelReply, St};

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
