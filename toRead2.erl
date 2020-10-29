-module(toRead2).
-compile(export_all).

server() -> 
    receive
        x -> io:fwrite("Received x ~n");
        y -> io:fwrite("Received y ~n"),
            receive % this will make it go through the mailbox again from the starting. 
             % Note : y is removed after the above line from the queue by now
               z -> io:fwrite("Hi I got z! ~n") 
            % this means I can only receive z once i have received y and once we receive z this 
            % receive will terminate and we will go to the top-level recieve again waiting for the message
       
             % y -> io:fwrite("Hi I got y again! ~n")  so if i do this i will never 
                                      % get out out of this recieve untill 
                                      % i will get a y again and it will never
                                      % receive x coz x is outside and x is '
                                      % after y in the queue 
            %  x -> io:fwrite("Hi I got x! ~n") this will work fine because the next
                                      % thing in the queue(mailbox) is x so it will 
                                      % receive x 
            end 
        end,
    server(). % after receiving all the messages this will keep go one forever looking for another msg in the mailbox



client(S) when S == 0 ->
    S!z,
 %   S!z, to understand line 11 and 12 more deeply
    S!y,
    S!x,
    io:fwrite("Received y ~n"),
    done.

client()->
   io:fwrite("Received y ~n"),
    done.


client(S, M) when M == 0->
   io:fwrite("Received ~w ~n", [S]),
    done;
client(S, M) ->
   io:fwrite("Received ~w ~w ~n", [S, M]),
    done.

example() -> 
    Server = spawn(?MODULE, server, []),
    client(Server).
% We get "done" in the result because the return result of the client is done and since 
% example is using client as a result result we get to see done in the result.

exampleCom() -> 
    Server = spawn(?MODULE, server, []),
    Client = spawn(?MODULE, client, [Server]),
  % Server.            to understand stuff from onward line 47 specially line 58
    [Server, Client].
    
% When I'm going to run this I'm not going to see the done msg because the done message that was 
% just seeing the return result of client and the return result inside spawn processes they don't
% go anywhere, they just disappear, they don't get return anywhere.So it's not going to return 
% down here. So we see the same messages from the server  plus the process id of Server and Client
% except done
 
% Since the server never end after it got called
% we can still send it new messages from the console and it will work properly
% Server = me:exampleCom().
% Server!x.
% Server!y.
% Note it will no work beacuse in the exampleCom() I'm returning the list of two
% different server id we need to return the Server process id to make it work
% change the last line to Server
% After changing 
% Server!x will return 
%  Received x 
%  x
%
% Server!z will return just
% z (because that's the return result for doing this send)
% and it's not printing out any message because it's not in a state where it can deal with z, rn
% it's at the top level receive whic can only deal with x's and ys'
% but now if we send that y it receives that y and the orevious z too
% Server!y will return 
% Received y 
% Hi I got z! 
% y
% and if we send multiple z before send y we have to send same numbers of y to receive those z's that
% we sent 
