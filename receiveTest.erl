-module(receiveTest).
-export([test/0]).

print(I, true) -> io:fwrite("~w pass~n", [I]);
print(I, false) -> io:fwrite("~w fail~n", [I]).


test() ->
    Recv = {0,
        [{0,{send,{var,191,'S'},{var,191,'Z'}},1},
        {1,{recv,[{tuple,193,[{atom,193,hello},{var,193,'C'}]}]},3},
        {3,{send,{var,193,'C'},{string,193,"hi"}},2},
        {1,{recv,[{tuple,194,[{atom,194,bye},{var,194,'C'}]}]},5},
        {5,{send,{var,194,'C'},{string,194,"Gn"}},2}]},
    case Recv ==  eCFSM:main("example.erl", "recv", 2) of
      true -> print(0, true);
      _ -> print(0, false)
    end,

    Recv1 = {0,
        [{0,{send,{var,198,'S'},{var,198,'Z'}},1},
         {1,{recv,[{tuple,200,[{atom,200,hello},{var,200,'C'}]}]},3},
         {3,{send,{var,200,'C'},{string,200,"hi"}},0},
         {1,{recv,[{tuple,201,[{atom,201,bye},{var,201,'C'}]}]},5},
         {5,{send,{var,201,'C'},{string,201,"Gn"}},0}]},
    case Recv1 ==  eCFSM:main("example.erl", "recv1", 2) of
      true -> print(1, true);
      _ -> print(1, false)
    end,

    Recv2 = {0,
        [{0,{send,{var,206,'S'},{var,206,'Z'}},1},
         {1,{recv,[{tuple,208,[{atom,208,hello},{var,208,'C'}]}]},3},
         {3,{send,{var,208,'C'},{string,208,"hi"}},2},
         {1,{recv,[{tuple,209,[{atom,209,bye},{var,209,'C'}]}]},5},
         {5,{send,{var,209,'C'},{string,209,"Gn"}},2},
         {2,{send,{var,211,'S'},{string,211,"Hi"}},0}]},
    case Recv2 ==  eCFSM:main("example.erl", "recv2", 2) of
      true -> print(2, true);
      _ -> print(2, false)
    end,

    Recv3 = {0,
        [{0,{send,{var,215,'S'},{var,215,'Z'}},1},
         {1,{recv,[{tuple,217,[{atom,217,hello},{var,217,'C'}]}]},3},
         {3,{send,{var,217,'C'},{string,217,"hi"}},2},
         {1,{recv,[{tuple,218,[{atom,218,bye},{var,218,'C'}]}]},5},
         {5,{send,{var,218,'C'},{string,218,"Gn"}},0},
         {2,{send,{var,221,'S'},{string,221,"Hi"}},0}]},
    case Recv3 ==  eCFSM:main("example.erl", "recv3", 2) of
      true -> print(3, true);
      _ -> print(3, false)
    end,

    Recv4 = {0,
        [{0,{send,{var,225,'S'},{var,225,'Z'}},1},
         {1,{recv,[{tuple,227,[{atom,227,hello},{var,227,'C'}]}]},3},
         {3,{send,{var,227,'C'},{string,227,"hi"}},2},
         {1,{recv,[{tuple,228,[{atom,228,bye},{var,228,'C'}]}]},5},
         {5,{send,{var,228,'C'},{string,228,"Gn"}},0},
         {2,{send,{var,231,'S'},{string,231,"Hi"}},6},
         {6,{send,{var,232,'S'},{string,232,"Bye"}},0}]},
    case Recv4 ==  eCFSM:main("example.erl", "recv4", 2) of
      true -> print(4, true);
      _ -> print(4, false)
    end,

    Recv5 = {0,
        [{0,{send,{var,236,'S'},{var,236,'Z'}},1},
         {1,{recv,[{tuple,238,[{atom,238,hello},{var,238,'C'}]}]},3},
         {3,{send,{var,238,'C'},{string,238,"hi"}},2},
         {1,{recv,[{tuple,239,[{atom,239,bye},{var,239,'C'}]}]},5},
         {5,{send,{var,239,'C'},{string,239,"Gn"}},2},
         {2,{send,{var,241,'S'},{string,241,"Hi"}},7},
         {7,{send,{var,242,'S'},{string,242,"Bye"}},0}]},
    case Recv5 ==  eCFSM:main("example.erl", "recv5", 2) of
      true -> print(5, true);
      _ -> print(5, false)
    end,

    Recv6 = {0,
        [{0,{send,{var,246,'S'},{var,246,'Z'}},1},
         {1,{recv,[{tuple,248,[{atom,248,hello},{var,248,'C'}]}]},3},
         {3,{send,{var,248,'C'},{string,248,"hi"}},4},
         {4,{recv,[{atom,250,x}]},6},
         {6,{send,{var,250,'C'},{atom,250,x}},5},
         {4,{recv,[{atom,251,y}]},8},
         {8,{send,{var,251,'C'},{atom,251,y}},5},
         {5,{send,{var,253,'C'},{atom,253,done}},2},
         {1,{recv,[{tuple,254,[{atom,254,bye},{var,254,'C'}]}]},11},
         {11,{send,{var,254,'C'},{string,254,"Gn"}},12},
         {12,{recv,[{atom,256,x}]},14},
         {14,{send,{var,256,'C'},{atom,256,x}},2},
         {12,{recv,[{atom,257,y}]},16},
         {16,{send,{var,257,'C'},{atom,257,y}},2},
         {2,{send,{var,260,'S'},{string,260,"Hi"}},18},
         {18,{send,{var,261,'S'},{string,261,"Bye"}},0}]},
    case Recv6 ==  eCFSM:main("example.erl", "recv6", 2) of
      true -> print(6, true);
      _ -> print(6, false)
    end,

    Recv7 = {0,
        [{0,{send,{var,265,'S'},{var,265,'Z'}},1},
         {1,{recv,[{tuple,267,[{atom,267,hello},{var,267,'C'}]}]},3},
         {3,{recv,[{atom,268,x}]},5},
         {5,{send,{var,268,'C'},{atom,268,x}},4},
         {3,{recv,[{atom,269,y}]},7},
         {7,{send,{var,269,'C'},{atom,269,y}},4},
         {4,{send,{var,271,'C'},{atom,271,done}},2},
         {1,{recv,[{tuple,272,[{atom,272,bye},{var,272,'C'}]}]},10},
         {10,{recv,[{atom,273,x}]},12},
         {12,{send,{var,273,'C'},{atom,273,x}},2},
         {10,{recv,[{atom,274,y}]},14},
         {14,{send,{var,274,'C'},{atom,274,y}},2},
         {2,{send,{var,277,'S'},{string,277,"Hi"}},16},
         {16,{send,{var,278,'S'},{string,278,"Bye"}},0}]},
    case Recv7 ==  eCFSM:main("example.erl", "recv7", 2) of
      true -> print(7, true);
      _ -> print(7, false)
    end,

    Recv8 = {0,
        [{0,{send,{var,282,'S'},{var,282,'Z'}},1},
         {1,{recv,[{atom,284,x}]},3},
         {3,{send,{var,284,'S'},{atom,284,x}},2},
         {1,{recv,[{atom,285,y}]},5},
         {5,{send,{var,285,'S'},{atom,285,y}},2},
         {2,{recv,[{atom,288,x}]},8},
         {8,{send,{var,288,'S'},{atom,288,x}},7},
         {2,{recv,[{atom,289,y}]},10},
         {10,{send,{var,289,'S'},{atom,289,y}},7},
         {7,{send,{var,291,'S'},{string,291,"Hi"}},12},
         {12,{send,{var,292,'S'},{string,292,"Bye"}},0}]},
    case Recv8 ==  eCFSM:main("example.erl", "recv8", 2) of
      true -> print(8, true);
      _ -> print(8, false)
    end,

    Recv9 = {0,
        [{0,{send,{var,296,'S'},{var,296,'Z'}},1},
         {1,{recv,[{tuple,298,[{atom,298,hello},{var,298,'C'}]}]},3},
         {3,{recv,[{atom,299,x}]},5},
         {5,{send,{var,299,'C'},{atom,299,x}},4},
         {3,{recv,[{atom,300,y}]},7},
         {7,{send,{var,300,'C'},{atom,300,y}},4},
         {4,{recv,[{atom,303,x}]},10},
         {10,{send,{var,303,'C'},{atom,303,x}},9},
         {4,{recv,[{atom,304,y}]},12},
         {12,{send,{var,304,'C'},{atom,304,y}},9},
         {9,{send,{var,306,'C'},{atom,306,done}},2},
         {1,{recv,[{tuple,307,[{atom,307,bye},{var,307,'C'}]}]},15},
         {15,{recv,[{atom,308,x}]},17},
         {17,{send,{var,308,'C'},{atom,308,x}},16},
         {15,{recv,[{atom,309,y}]},19},
         {19,{send,{var,309,'C'},{atom,309,y}},16},
         {16,{recv,[{atom,312,x}]},22},
         {22,{send,{var,312,'C'},{atom,312,x}},2},
         {16,{recv,[{atom,313,y}]},24},
         {24,{send,{var,313,'C'},{atom,313,y}},2},
         {2,{send,{var,317,'S'},{string,317,"Hi"}},26},
         {26,{send,{var,318,'S'},{string,318,"Bye"}},0}]},
    case Recv9 ==  eCFSM:main("example.erl", "recv9", 2) of
      true -> print(9, true);
      _ -> print(9, false)
end,

    Recv10  = {0,
    [{0,{send,{var,322,'S'},{var,322,'Z'}},1},
     {1,{recv,[{tuple,324,[{atom,324,hello},{var,324,'C'}]}]},3},
     {3,{send,{var,324,'C'},{string,324,"hi"}},0},
     {1,{recv,[{tuple,326,[{atom,326,bye},{var,326,'C'}]}]},4},
     {4,{send,{var,326,'C'},{string,326,"Gn"}},0}]},
     case Recv10 == eCFSM:main("example.erl", "recv10", 2) of
       true -> print(10, true);
       _    -> print(10, false)
  end.

    Recv11 = {0,
    [{0,{send,{var,334,'S'},{var,334,'Z'}},1},
     {1,{recv,[{tuple,336,[{atom,336,hello},{var,336,'C'}]}]},3},
     {3,{send,{var,336,'C'},{string,336,"hi"}},4},
     {4,{recv,[{atom,338,x}]},6},
     {6,{send,{var,338,'C'},{atom,338,x}},0},
     {4,{recv,[{atom,340,y}]},7},
     {7,{send,{var,340,'C'},{atom,340,y}},0},
     {1,{recv,[{tuple,344,[{atom,344,bye},{var,344,'C'}]}]},8},
     {8,{send,{var,344,'C'},{string,344,"Gn"}},9},
     {9,{recv,[{atom,346,x}]},11},
     {11,{send,{var,346,'C'},{atom,346,x}},2},
     {9,{recv,[{atom,347,y}]},13},
     {13,{send,{var,347,'C'},{atom,347,y}},2},
     {2,{send,{var,350,'S'},{string,350,"Hi"}},15},
     {15,{send,{var,351,'S'},{string,351,"Bye"}},0}]},
    case Recv11 == eCFSM:main("example.erl", "recv11", 2) of
      true -> print(11, true);
      false -> print(11, false)
    end,

    Recv12 = {0,
    [{0,{send,{var,355,'S'},{var,355,'Z'}},1},
     {1,{recv,[{tuple,357,[{atom,357,hello},{var,357,'C'}]}]},3},
     {3,{send,{var,357,'C'},{string,357,"hi"}},4},
     {4,{recv,[{atom,359,x}]},6},
     {6,{send,{var,359,'C'},{atom,359,x}},0},
     {4,{recv,[{atom,361,y}]},7},
     {7,{send,{var,361,'C'},{atom,361,y}},0},
     {1,{recv,[{tuple,365,[{atom,365,bye},{var,365,'C'}]}]},8},
     {8,{send,{var,365,'C'},{string,365,"Gn"}},9},
     {9,{recv,[{atom,367,x}]},11},
     {11,{send,{var,367,'C'},{atom,367,x}},0},
     {9,{recv,[{atom,369,y}]},12},
     {12,{send,{var,369,'C'},{atom,369,y}},0}]},
    case Recv12 == eCFSM:main("example.erl", "recv12", 2) of
      true -> print(12, true);
      false -> print(12, false)
end.