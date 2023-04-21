-module(examples).
-compile(export_all).

exampleAll() -> example1(),
                example2(),
                example3(),
                example4(),
                example5(),
                example6(),
                example7(),
                example8().

example1() -> io:format("Example 1, input is [{1, [ {1,{send,{var,1,'P'},m},2} ]}, {1, [ {1,{send,{var,1,'P'},x},2} ]}]\n"),
    generate_code:main([{1, [ {1,{send,{var,1,'P'},m},2} ]}, {1, [ {1,{send,{var,1,'P'},x},2} ]}]).

example2() -> io:format("Example 2, input is [{1,[{1,{send,{var,1,'P'},m},2}] },{10,[{10,{recv,m},20}]}]\n"),
    generate_code:main([{1,[{1,{send,{var,1,'P'},message},2}] },{1,[{1,{recv,message},2}]}]).

example3() -> io:format("Example 3, input is [{1,[{1,{send,{var,1,'P'},m},2},{2,{send,{var,1,'P2'},m2},3},{3,{send,{var,1,'P3'},m3},4}] }]\n"),
    generate_code:main([{1,[{1,{send,{var,1,'P'},m},2},{2,{send,{var,1,'P2'},m2},3},{3,{send,{var,1,'P3'},m3},4}] }]).

example4() -> io:format("Example 4, input is [{1, [{1, {send,'P',m}, 2}, {2, {recv,a}, 3}]  }, {10, [{10, {send,'P',a}, 20}, {20, {recv,m}, 30}] }]\n"),
    generate_code:main([{1, [{1, {send,'P',m}, 2}, {2, {recv,a}, 3}]  }, {10, [{10, {send,'P',a}, 20}, {20, {recv,m}, 30}] }]).

example5() -> io:format("Example 5, input is [{1, [{1,{recv,b},1},{2,{recv,a},2},{1,{recv,a},2},{2,{recv,b},1}]}, {10, [{10, {send,{var,1,'P'},a}, 20}]}, {100, [{100, {send,{var,1,'P'},b}, 200}]}]\n"),
    generate_code:main([{1, [{1,{recv,b},1},{2,{recv,a},2},{1,{recv,a},2},{2,{recv,b},1}]}, {10, [{10, {send,{var,1,'P'},a}, 20}]}, {100, [{100, {send,{var,1,'P'},b}, 200}]}]).

example6() -> io:format("Example 6, input is [{1, [ {1,{recv,a},3},{1,{recv,b},2},{2,{recv,x},1},{3,{recv,b},2},{3,{recv,x},4} ]}, {10, [ {10,{send,{var,1,'P'},a},20} ]}, {100, [ {100,{send,{var,1,'P'},b},200}, {200, {send,{var,1,'P'},x}, 300} ]}]\n"),
    generate_code:main([{1, [ {1,{recv,prepareExit},3},{1,{recv,compute},2},{2,{recv,stop},1},{3,{recv,compute},2},{3,{recv,stop},4} ]}, {1, [ {1,{send,{var,1,'P'},prepareExit},2} ]}, {1, [ {1,{send,{var,1,'P'},compute},2}, {2, {send,{var,1,'P'},stop}, 3} ]}]).

example7() -> io:format("Example 7, input is [{1, [{1,{send,{var,1,'P'},start},2}, {2,{recv,a},3}, {2,{recv,b},4}, {4,{send,{var,1,'P'},bVal},6}, {6,{send,{var,1,'P'},stop},7}, {3,{send,{var,1,'P'},aVal},5}, {5,{send,{var,1,'P'},stop},7}] },
{1, [{1,{recv,stop},3},{1,{recv,start},2},{2,{send,{var,1,'P'},b},4},{4,{recv,bVal},1}] }]\n"),
    generate_code:main([{1, [{1,{send,{var,1,'P'},start},2}, {2,{recv,a},3}, {2,{recv,b},4}, {4,{send,{var,1,'P'},bVal},6}, {6,{send,{var,1,'P'},stop},7}, {3,{send,{var,1,'P'},aVal},5}, {5,{send,{var,1,'P'},stop},7}] },
        {1, [{1,{recv,stop},3},{1,{recv,start},2},{2,{send,{var,1,'P'},b},4},{4,{recv,bVal},1}] }]).

example8() ->
    io:format("Example 8, input is [{1, [{1,{send,{var,1,'S'},open},2},{2,{recv,error},1},{2,{recv,go},3},{3,{send,{var,1,'S'},data},4},{4,{recv,error},3},{4,{recv,info},5},{5,{send,{var,1,'S'},close},1}]},
    {1, [{1,{recv,open},2},{1,{recv,close},3},{2,{send,{var,1,'D'},try1},4},{4,{recv,error},6},{4,{recv,work},5},{6,{send,{var,1,'C'},error},1},{5,{send,{var,1,'C'},go},7},{7,{recv,lose},3},{7,{recv,data},8},{8,{send,{var,1,'D'},ask},9},{9,{recv,error},10},{9,{recv,info},11},{10,{send,{var,1,'C'},error},7},{11,{send,{var,1,'C'},info},12},{12,{recv,data},8},{12,{recv,close},3}]},
    {1,[{1,{recv,try1},2},{2,{send,{var,1,'S'},work},1},{1,{recv,data},3},{3,{send,{var,1,'S'},info},1}]}]\n"),
    generate_code:main([{1, [{1,{send,{var,1,'S'},open},2},{2,{recv,error},1},{2,{recv,go},3},{3,{send,{var,1,'S'},data},4},{4,{recv,error},3},{4,{recv,info},5},{5,{send,{var,1,'S'},close},1}]},
    {1, [{1,{recv,open},2},{1,{recv,close},3},{2,{send,{var,1,'D'},try1},4},{4,{recv,error},6},{4,{recv,work},5},{6,{send,{var,1,'C'},error},1},{5,{send,{var,1,'C'},go},7},{7,{recv,lose},3},{7,{recv,data},8},{8,{send,{var,1,'D'},ask},9},{9,{recv,error},10},{9,{recv,info},11},{10,{send,{var,1,'C'},error},7},{11,{send,{var,1,'C'},info},12},{12,{recv,data},8},{12,{recv,close},3}]},
    {1,[{1,{recv,try1},2},{2,{send,{var,1,'S'},work},1},{1,{recv,data},3},{3,{send,{var,1,'S'},info},1}]}]).

example9() ->
    generate_code:main([{1,[{1,{send,{var,1,'P'},{var,1,'X'}},2},{2,{recv,less},5},{2,{recv,more},3},{3,{send,{var,1,'P'},{var,1,'Y'}},4},{5,{send,{var,1,'S'},{var,1,'X'}},6}]},{1,[{1,{recv,{var,1,'X'}},2},{2,{send,{var,1,'P'},more},3}]}]).
