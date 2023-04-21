
-module(test).
-export([test/0]).

test() ->
    io:fwrite("testing receive expressions testing_files/recv.erl:~n"),
    test(recv, 14),
    io:fwrite("testing call expressions testing_files/call.erl:~n"),
    test(call, 0),
    io:fwrite("testing random examples testing_files/examples.erl:~n"),
    test(random, 12),
    io:fwrite("testing clause expressions testing_files/clause.erl:~n"),
    test(clause, 4),
    io:fwrite("testing function expressions testing_files/function.erl:~n"),
    test(function, 10),
    io:fwrite("testing case expressions testing_files/case_of.erl:~n"),
    test(case_of, 1).


test(recv, X) ->
    recv_test(X);
test(call, X) ->
    call_test(X);
test(random, X) ->
    random_test(X);
test(clause, X) ->
    clause_test(X);
test(function, X) ->
    function_test(X);
test(case_of, X) ->
    case_test(X).

%%%%%%%%%%%%%%%%%%%%%%%%%% Testing receive expression cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv_test(0) ->
    generate_model:main("testing_files/recv.erl", "recv/2", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    {ok, Check_data} = file:read_file("testing_files/receive_examples/recv\~2.dot"),
    case Result == Check_data of
        true -> io:fwrite("Pass: 0~n", []);
        _ -> io:fwrite("Fail: recv/2~n", [])
end;

recv_test(X) ->
    Fun = lists:flatten(io_lib:format("recv~p", [X])),
    generate_model:main("testing_files/recv.erl", Fun ++ "/2", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    Test_file = lists:flatten(io_lib:format("testing_files/receive_examples/~s.dot", [Fun ++ "~2"])),
    {ok, Check_data} = file:read_file(Test_file),
    case Result == Check_data of
        true -> io:fwrite("Pass: ~p~n", [X]),
                recv_test(X-1);
        _    -> io:fwrite("Fail: recv~p/2~n", [X]),
                recv_test(X-1)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%% Testing call expression cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_test(0) ->
    generate_model:main("testing_files/call.erl", "call/2", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    {ok, Check_data} = file:read_file("testing_files/call_examples/call\~2.dot"),
    case Result == Check_data of
        true ->
            io:fwrite("Pass: 0~n", []);
        _    ->
            io:fwrite("Fail: call/2~n", [])
end;

call_test(X) ->
    Fun = lists:flatten(io_lib:format("call~p", [X])),
    generate_model:main("testing_files/call.erl", Fun ++ "/2", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    Test_file = lists:flatten(io_lib:format("testing_files/call_examples/call~s.dot", [Fun ++ "~2"])),
    {ok, Check_data} = file:read_file(Test_file),
    case Result == Check_data of
        true -> io:fwrite("Pass: ~p~n", [X]),
                call_test(X-1);
        _    -> io:fwrite("Fail: call~p/2~n", [X]),
                call_test(X-1)
end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%% Testing random examples %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_test(0) ->
    generate_model:main("testing_files/examples.erl", "e/1", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    {ok, Check_data} = file:read_file("testing_files/random_examples/e\~1.dot"),
    case Result == Check_data of
        true -> io:fwrite("Pass: 0~n", []);
        _ -> io:fwrite("Fail: e/1~n", [])
end;

random_test(9) ->
    generate_model:main("testing_files/examples.erl", "e9/3", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    {ok, Check_data} = file:read_file("testing_files/random_examples/e9\~3.dot"),
    case Result == Check_data of
        true -> io:fwrite("Pass: 9~n", []),
            random_test(8);
        _ -> io:fwrite("Fail: e9/3~n", []),
            random_test(8)
end;

random_test(X) ->
    Fun = lists:flatten(io_lib:format("e~p", [X])),
    generate_model:main("testing_files/examples.erl", Fun ++ "/2", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    Test_file = lists:flatten(io_lib:format("testing_files/random_examples/~s.dot", [Fun ++ "~2"])),
    {ok, Check_data} = file:read_file(Test_file),
    case Result == Check_data of
        true -> io:fwrite("Pass: ~p~n", [X]),
                random_test(X-1);
        _    -> io:fwrite("Fail: e/2~p~n", [X]),
                random_test(X-1)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%% Testing clause expression cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clause_test(0) ->
    generate_model:main("testing_files/clause.erl", "c/3", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    {ok, Check_data} = file:read_file("testing_files/clause_examples/c\~3.dot"),
    case Result == Check_data of
        true ->
            io:fwrite("Pass: 0~n", []);
        _    ->
            io:fwrite("Fail: c/3~n", [])
end;

clause_test(X) ->
    Fun = lists:flatten(io_lib:format("c~p", [X])),
    generate_model:main("testing_files/clause.erl", Fun ++ "/3", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    Test_file = lists:flatten(io_lib:format("testing_files/clause_examples/~s.dot", [Fun ++ "\~3"])),
    {ok, Check_data} = file:read_file(Test_file),
    case Result == Check_data of
        true -> io:fwrite("Pass: ~p~n", [X]),
                clause_test(X-1);
        _    -> io:fwrite("Fail: c~p/3~n", [X]),
                clause_test(X-1)
end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%% Testing function expression cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function_test(0) ->
    generate_model:main("testing_files/function.erl", "f/2", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    {ok, Check_data} = file:read_file("testing_files/function_examples/f\~2.dot"),
    case Result == Check_data of
        true ->
            io:fwrite("Pass: 0~n", []);
        _    ->
            io:fwrite("Fail: f/2~n", [])
end;

function_test(X) ->
    Fun = lists:flatten(io_lib:format("f~p", [X])),
    generate_model:main("testing_files/function.erl", Fun ++ "/2", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    Test_file = lists:flatten(io_lib:format("testing_files/function_examples/~s.dot", [Fun ++ "\~2"])),
    {ok, Check_data} = file:read_file(Test_file),
    case Result == Check_data of
        true -> io:fwrite("Pass: ~p~n", [X]),
                function_test(X-1);
        _    -> io:fwrite("Fail: f~p/2~n", [X]),
                function_test(X-1)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%% Testing case expression cases %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

case_test(0) ->
    generate_model:main("testing_files/case_of.erl", "c/1", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    {ok, Check_data} = file:read_file("testing_files/case_examples/c\~1.dot"),
    case Result == Check_data of
        true ->
            io:fwrite("Pass: 0~n", []);
        _    ->
            io:fwrite("Fail: c/1~n", [])
end;

case_test(X) ->
    Fun = lists:flatten(io_lib:format("c~p", [X])),
    generate_model:main("testing_files/case_of.erl", Fun ++ "/1", "testing_files/"),
    {ok, Result} = file:read_file("testing_files/graph.dot"),
    Test_file = lists:flatten(io_lib:format("testing_files/case_examples/~s.dot", [Fun ++ "\~1"])),
    {ok, Check_data} = file:read_file(Test_file),
    case Result == Check_data of
        true -> io:fwrite("Pass: ~p~n", [X]),
                function_test(X-1);
        _    -> io:fwrite("Fail: c~p/1~n", [X]),
                function_test(X-1)
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% create test file %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% recv

createTestFiles(recv, 0) ->
    Fun = lists:flatten(io_lib:format("recv", [])),
    generate_model:main("testing_files/recv.erl", Fun ++ "/2", "testing_files/receive_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~2"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\receive_examples\\graph.dot testing_files\\receive_examples\\~s.dot", [Filename])),
    os:cmd(Cmd);

createTestFiles(recv, X) ->
    Fun = lists:flatten(io_lib:format("recv~p", [X])),
    generate_model:main("testing_files/recv.erl", Fun ++ "/2", "testing_files/receive_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~2"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\receive_examples\\graph.dot testing_files\\receive_examples\\~s.dot", [Filename])),
    os:cmd(Cmd),
    createTestFiles(recv, X-1);

%% call

createTestFiles(call, 0) ->
    Fun = lists:flatten(io_lib:format("call", [])),
    generate_model:main("testing_files/call.erl", Fun ++ "/2", "testing_files/call_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~2"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\call_examples\\graph.dot testing_files\\call_examples\\~s.dot", [Filename])),
    os:cmd(Cmd);

createTestFiles(call, X) ->
    Fun = lists:flatten(io_lib:format("call~p", [X])),
    generate_model:main("testing_files/call.erl", Fun ++ "/2", "testing_files/call_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~2"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\call_examples\\graph.dot testing_files\\call_examples\\~s.dot", [Filename])),
    os:cmd(Cmd),
    createTestFiles(call, X-1);

%% random examples

createTestFiles(random, 0) ->
    Fun = lists:flatten(io_lib:format("e", [])),
    generate_model:main("testing_files/examples.erl", Fun ++ "/1", "testing_files/random_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~1"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\random_examples\\graph.dot testing_files\\random_examples\\~s.dot", [Filename])),
    os:cmd(Cmd);

createTestFiles(random, 9) ->
    Fun = lists:flatten(io_lib:format("e~p", [9])),
    generate_model:main("testing_files/examples.erl", Fun ++ "/3", "testing_files/random_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~3"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\random_examples\\graph.dot testing_files\\random_examples\\~s.dot", [Filename])),
    os:cmd(Cmd),
    createTestFiles(random, 8);

createTestFiles(random, X) ->
    Fun = lists:flatten(io_lib:format("e~p", [X])),
    generate_model:main("testing_files/examples.erl", Fun ++ "/2", "testing_files/random_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~2"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\random_examples\\graph.dot testing_files\\random_examples\\~s.dot", [Filename])),
    os:cmd(Cmd),
    createTestFiles(random, X-1);

%% function

createTestFiles(function, 0) ->
    Fun = lists:flatten(io_lib:format("f", [])),
    generate_model:main("testing_files/function.erl", Fun ++ "/2", "testing_files/function_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~2"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\function_examples\\graph.dot testing_files\\function_examples\\~s.dot", [Filename])),
    os:cmd(Cmd);

createTestFiles(function, X) ->
    Fun = lists:flatten(io_lib:format("f~p", [X])),
    generate_model:main("testing_files/function.erl", Fun ++ "/2", "testing_files/function_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~2"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\function_examples\\graph.dot testing_files\\function_examples\\~s.dot", [Filename])),
    os:cmd(Cmd),
    createTestFiles(function, X-1);

%% clause

createTestFiles(clause, 0) ->
    Fun = lists:flatten(io_lib:format("c", [])),
    generate_model:main("testing_files/clause.erl", Fun ++ "/3", "testing_files/clause_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~3"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\clause_examples\\graph.dot testing_files\\clause_examples\\~s.dot", [Filename])),
    os:cmd(Cmd);

createTestFiles(clause, X) ->
    Fun = lists:flatten(io_lib:format("c~p", [X])),
    generate_model:main("testing_files/clause.erl", Fun ++ "/3", "testing_files/clause_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~3"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\clause_examples\\graph.dot testing_files\\clause_examples\\~s.dot", [Filename])),
    os:cmd(Cmd),
    createTestFiles(clause, X-1);

%% case

createTestFiles(case_of, 0) ->
    Fun = lists:flatten(io_lib:format("c", [])),
    generate_model:main("testing_files/case_of.erl", Fun ++ "/1", "testing_files/case_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~1"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\case_examples\\graph.dot testing_files\\case_examples\\~s.dot", [Filename])),
    os:cmd(Cmd);

createTestFiles(case_of, X) ->
    Fun = lists:flatten(io_lib:format("c~p", [X])),
    generate_model:main("testing_files/case_of.erl", Fun ++ "/1", "testing_files/case_examples/"),
    Filename = lists:flatten(io_lib:format("~s~s", [Fun, "\~1"])),
    Cmd = lists:flatten(io_lib:format("move testing_files\\case_examples\\graph.dot testing_files\\case_examples\\~s.dot", [Filename])),
    os:cmd(Cmd),
    createTestFiles(case_of, X-1).