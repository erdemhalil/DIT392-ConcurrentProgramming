## Chat commands ##

- `/join #channel` - Join a channel with the given name. By convention, all the channels’ names start with #.

- `/leave` - Leave the current channel.

- `/leave` - #channel Leave the specified channel.

- `/whoami` - Return current nick (see the System tab).

- `/nick newnick` - Change nick to newnick. The format for nicks is the same as for an Erlang atom, i.e. it starts with a lower case letter and is followed by letters, numbers, or underscore (_).

- `/quit` - Quit the client.

## How to run ##

After you have downloaded the skeleton code, compile everything with the command:
```
make all
```
You can then open the Erlang shell (erl) and start a client and the server with 
```
cchat:server().
cchat:client().
```

To compile, load Erlang, start a server and two clients in one go, try the following command from the system shell:

```
make && erl -eval "cchat:server(), cchat:client(), cchat:client()."
```

## Test cases ##
We provide some unit tests which check the basic functionality of your solution. All unit tests are contained in the file lib/test_client.erl and are run using EUnit. We have created entries in the Makefile to make life easier for you. To run these tests, simply execute the following:

```
make -s run_tests
```

To run a single test, use the following (replacing message_throughput with whichever test you want to run):
```
make && erl -eval "eunit:test({test, test_client, message_throughput_test}), halt()."
```

