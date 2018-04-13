:-initialization sched.

%treeNode(Machine, Task, Penalty) are added during runtime
%fpaNode(Machine, Task) are added during runtime
%forbidMNode(Machine, Task) are added during runtime
%tntNode(Task, Task) are added during runtime
%tnpNode(Machine, Task, Penalty) are added during runtime\
%bestPath().
%bestQual().

sched :-
    %Get the arguments from command line
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, InputFile), % get first argument
    nth0(1, Argv, OutputFile), % get second argument
    assert(outputFileName(OutputFile)),
    %Open the file passed into the program on command line
    open(InputFile, read, Str),
    read_file(Str,Lines),
    close(Str),
    maplist(removeLastSpaces, Lines, SpacelessLines),
    generatePredSeeds,
    validInput(SpacelessLines, Out),
    destroyPredSeeds,
    printAll,
    \+ logicMaster(),nl,
    write("Print to output"),nl,
    printOutput(OutputFile).

%Creates an invalid version assignment of the predicates so inital checks dont crash
%(they are removed later)
generatePredSeeds() :-
    assert(fpaNode(-1,-1)),
    assert(forbidMNode(-1,-1)),
    assert(tntNode(-1,-1)),
    assert(treeNode(-1,-1,-1)),
    assert(tnpNode(-1,-1,-1)),
    assert(bestQual(0)),
    assert(bestPath(0)).

%Destroys predicate seeds
destroyPredSeeds() :-
    retract(fpaNode(-1,-1)),
    retract(forbidMNode(-1,-1)),
    retract(tntNode(-1,-1)),
    retract(treeNode(-1,-1,-1)),
    retract(tnpNode(-1,-1,-1)),
    retract(bestQual(0)),
    retract(bestPath(0)).

%Debug print statments-------------------------------------
printAll() :- printFPA, printForbidM, printTNT, printTreeNodes, printTNP, nl,nl.
printFPA() :- write("---fpa---"), nl, fpaNode(Task, Machine), write((Task, Machine)), nl, fail; true.
printForbidM() :- nl, write("---forbid M---"), nl,forbidMNode(Task, Machine), write((Task, Machine)), nl, fail; true.
printTNT() :- nl, write("---tnt---"), nl,tntNode(Task1, Task2), write((Task1, Task2)), nl, fail; true.
printTreeNodes() :- nl, write("---PenaltyNodes---"), nl,treeNode(Task, Machine, Penalty), write((Task, Machine, Penalty)), nl, fail; true.
printTNP() :- nl, write("---tnp---"), nl,tnpNode(Task, Machine, Penalty), write((Task, Machine, Penalty)), nl, fail; true.
%----------------------------------------------------------
%Put all of the file into a list
read_file(Stream,[]) :-
    at_end_of_stream(Stream).
%Split up the characters into row by row in a list and turns them into atoms?
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.
%--------------------------PARSING START--------------------------------
%--------------------------PARSING START--------------------------------
%----------------------DCG For Parsing Start------------------------
validInput --> name,fpa,forbidM,tnt,machs,tnp.
%DCG for parsing Name information
name --> after, nameHeader, after, nameBody, arbLines.
nameHeader --> ['Name:'];{writeToFile(6)}.
nameBody --> [X], {hasInternalSpace(X)}.

%DCG for parsing Forced Partial Assignment information
fpa --> fpaHeader,fpaBody('0').
fpaHeader --> ['forced partial assignment:'].
fpaBody(Flag) --> fpaEatPair(NewFlag),
    {\+ NewFlag = '2'}, fpaBody(NewFlag) ;
        {Flag = '1'}, [];
        {Flag = '0'}, arbLines.
fpaEatPair(NewFlag) -->
        [X], {parseFPA(X), NewFlag = '0'};
        [''], {NewFlag = '1'};
        [], {NewFlag = '2'}.
%DCG for parsing Forbidden Machine information
forbidM --> forbidHeader,forbidBody('0').
forbidHeader --> ['forbidden machine:'].
forbidBody(Flag) --> forbidEatPair(NewFlag),
        {\+ NewFlag = '2'}, forbidBody(NewFlag) ;
        {Flag = '1'}, [];
        {Flag = '0'}, arbLines.
forbidEatPair(NewFlag) -->
        [X], {parseForbid(X), NewFlag = '0'};
        [''], {NewFlag = '1'};
        [], {NewFlag = '2'}.
%DCG for parsing Too-near Penalty information
tnt --> tntHeader,tntBody('0').
tntHeader --> ['too-near tasks:'].
tntBody(Flag) --> tntEatPair(NewFlag),
        {\+ NewFlag = '2'}, tntBody(NewFlag) ;
        {Flag = '1'}, [];
        {Flag = '0'}, arbLines.
tntEatPair(NewFlag) -->
        [X], {parseTNT(X), NewFlag = '0'};
        [''], {NewFlag = '1'};
        [], {NewFlag = '2'}.
%DCG for parsing Machine Penalty information
machs --> machsHeader,after,machsBody.
machsHeader --> ['machine penalties:'].


specialFailCase --> {write("Check for :"),nl}, ['too-near penalities:'],{writeToFile(6)}.


%do we need two afters here?
machsBody --> mN,after,after,mN,after,mN,after,mN,after,mN,after,mN,after,mN,after,mN,marbLines; {writeToFile(3)}.
mN --> [X], {parseArray(X);writeToFile(6)}; writeToFile(6).
%mNSpecial --> [X], {parseArray(X);writeToFile(3)}; writeToFile(3).
%DCG for parsing Too-near Penalty information
tnp --> tnpHeader,tnpBody('0').
tnpHeader --> ['too-near penalities'].
tnpBody(Flag) --> tnpEatPair(NewFlag),
        {\+ NewFlag = '2'}, tnpBody(NewFlag) ;
        {Flag = '1'}, [];
        {Flag = '0'}, after.
tnpEatPair(NewFlag) -->
        [X], {parseTNP(X), NewFlag = '0'};
        [''], {NewFlag = '1'};
        [], {NewFlag = '2'}.

%DCG for blank Newlines
arbLines --> [''],after.
after --> [''],after.
after --> [].

%Special DCG for machine penalty (it needs to return )
marbLines --> {write("arb ome"),nl},[''],after; {write("special check"),nl},specialFailCase;{write("file"),nl},{writeToFile(3)}.

%----------------------DCG For Parsing End------------------------
%-------------------------Parsers Start------------------------------
%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseFPA(Atom) :-
    \+ Atom = '',
    \+ Atom = 'forbidden machine:',
    atom_codes(Atom, List),
    check2Tuple(List,0),
    List = [OB,INT,COMMA,CHAR,CB|Remainder],
    checkChar(OB,"(",2),
    checkIntBounds(INT),
    checkChar(COMMA,",",2),
    checkCharBounds(CHAR,2),
    checkChar(CB,")",2),
    checkEmpty(Remainder),
    Mach is INT-"0", Task is CHAR-"@",
    checkPA(Mach,Task),
    assert(fpaNode(Mach,Task)).         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form
%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseForbid(Atom) :-
    \+ Atom = '',
    \+ Atom = 'too-near tasks:',
    atom_codes(Atom, List),
    check2Tuple(List,0),
    List = [OB,INT,COMMA,CHAR,CB|Remainder],
    checkChar(OB,"(",2),
    checkIntBounds(INT),
    checkChar(COMMA,",",2),
    checkCharBounds(CHAR,2),
    checkChar(CB,")",2),
    checkEmpty(Remainder),
    Mach is INT-"0", Task is CHAR-"@",
    assert(forbidMNode(Mach,Task)).         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form
%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseTNT(Atom) :-
    \+ Atom = '',
    \+ Atom = 'machine penalties:',
    atom_codes(Atom, List),
    check2Tuple(List,0),
    List = [OB,CHAR1,COMMA,CHAR2,CB|Remainder],
    checkChar(OB,"(",2),
    checkCharBounds(CHAR1,2),
    checkChar(COMMA,",",2),
    checkCharBounds(CHAR2,2),
    checkChar(CB,")",2),
    checkEmpty(Remainder),
    Task1 is CHAR1-"@", Task2 is CHAR2-"@",
    assert(tntNode(Task1,Task2)).

%Confirms array is of correct form, all nums >= 0, and asserts the results
parseArray(Atom):-
    write(Atom),nl,
    split_string(Atom,' ','',AtomList),
    length(AtomList, 8),
    write("past length check"),nl,
    parseArrayElems(AtomList,8),
    maplist(atom_number,AtomList,IntList),
    getMachine(1,Machine),
    assertArray(Machine,1,IntList);
    write("ALT error"),nl,
    writeToFile(3).

%look at first 8
%if any non >=0 ints, invalid penalty
%look at elements after the 8 first elements
%If there are more numbers, machine penalty error
%If there are more non-numbers, parser error


parseArrayElems(TailOf8 ,0) :-
    TailOf8 = []; %If nothing is left, true
    TailOf8 = [H|Tail], %If something left and it is int, machine penalty error
    atom_number(H,_),
    writeToFile(3);
    writeToFile(6).

parseArrayElems([H|Tail], Counter) :-
    \+ H = '',
    \+ H = "",
    atom_number(H, Number),
    integer(Number),
    Number >= 0,
    NewCounter is Counter-1,
    parseArrayElems(Tail,NewCounter);
    \+ H = '',
    \+ H = "",
    writeToFile(5);
    writeToFile(6).  %non >=0 ints in first 8, invalid penalty error

parseArrayElems([],_) :- writeToFile(3).  %less than 8 elements, machine penalty error

%Finds next unassigned machine (Check =< 8 is intentional, do not make it 7)
getMachine(Check, Machine) :-
    \+ treeNode(Check,_,_),
    Machine is Check;
    Check =< 8,
    NewCheck is Check+1,
    getMachine(NewCheck, Machine).
%Asserts all elements in the list as new treeNodes
assertArray(_,_,[]).
assertArray(Machine, Task, [Penalty|Remaining]) :-
    assert(treeNode(Machine, Task, Penalty)),
    NewTask is Task+1,
    assertArray(Machine, NewTask, Remaining).


%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseTNP(Atom) :-
    \+ Atom = '',
    atom_codes(Atom, List),
    check3Tuple(List,0),
    List = [OB,CHAR1,COMMA1,CHAR2,COMMA2|Remainder],
    checkChar(OB,"(",4),
    checkCharBounds(CHAR1,4),
    checkChar(COMMA1,",",4),
    checkCharBounds(CHAR2,4),
    checkChar(COMMA2,",",4),
    checkPenalty(Remainder, Penalty, End),
    Penalty >= 0,
    checkEmpty(End),
    Task1 is CHAR1-"@", Task2 is CHAR2-"@",
    \+ retractIfExist(Task1,Task2),
    assert(tnpNode(Task1,Task2,Penalty)).


retractIfExist(Task1,Task2) :-
    retract(tnpNode(Task1,Task2,_)), fail.

%-------------------------Parsers End------------------------------
%---------------------Validity Checks Start------------------------
%Checks if name is of valid form (no spaces)
hasInternalSpace(X):-
    atom_length(X,Y),
    Y>0,
    split_string(X,' ','',Z),
    length(Z,1);
    writeToFile(6),
    write('Error while parsing input file'),nl,halt(0).
%Checks if partial assignment has already been made to the task or machine
checkPA(Mach, Task) :-
    \+ fpaNode(Mach,_),
    \+ fpaNode(_,Task);
    writeToFile(1),
    write('partial assignment error'),nl,halt(0).
%Special check for TNP last number which could be more than one digit
%!Warning! This predicate is NOT VERSATILE, it only was designed for one use.
checkPenalty(String, Number, Remainder) :-
    atom_chars(Atom,String),
    split_string(Atom,')','',AtomList),
    AtomList = [H|T],
    atom_number(H, Number),
    Remainder = T;
    writeToFile(5),
    write('invalid task0'),halt(0).


%Checks format of pairing variabls and throws error if fail

%Call with Error 4 for Soft, 2 for Hard
checkChar(Char, ExpectedChar, Error) :-
    Char =:= ExpectedChar;
    writeToFile(Error),
    write('invalid task1'),halt(0).

checkCharBounds(Char, Error) :-
    Char >= "A",
    Char =< "H";
    writeToFile(Error),
    write('invalid task2'),halt(0).

checkIntBounds(Int) :-
    Int >= "1",
    Int =< "8";
    %write('invalid task3'),halt(0),
    writeToFile(2).

checkEmpty(Remainder) :-
    Remainder = [];
    Remainder = [""];   %<-- Unlikely to cause issues but can be split off if needed
    writeToFile(6),
    write('Error while parsing input file Empty'),nl,halt(0).


%Checks that line starts with (_arb_,_arb_), Flag should always be started as 0
check2Tuple(List,Flag) :-
    Flag = 0, List = [H|T], H =:= "(", check2Tuple(T,1);
    Flag = 1, List = [H|T], \+ H =:= 32, check2Tuple(T,2);
    Flag = 2, List = [H|T], H =:= ",", check2Tuple(T,3);
    Flag = 3, List = [H|T], \+ H =:= 32,check2Tuple(T,4);
    Flag = 4, List = [H|T], H =:= ")", check2Tuple(T,5);
    Flag = 5;
    List = [H|_], H =:= 32,writeToFile(6), write('Error while parsing input file'),halt(0);  %32 is ascii ''
    List = [_|T], check2Tuple(T,Flag);
    writeToFile(6),
    write('Error while parsing input file 2T'),nl,halt(0).
%Checks that line starts with (_arb_,_arb_), Flag should always be started as 0
check3Tuple(List,Flag) :-
    Flag = 0, List = [H|T], H =:= "(", check3Tuple(T,1);
    Flag = 1, List = [H|T], \+ H =:= 32, check3Tuple(T,2);
    Flag = 2, List = [H|T], H =:= ",", check3Tuple(T,3);
    Flag = 3, List = [H|T], \+ H =:= 32,check3Tuple(T,4);
    Flag = 4, List = [H|T], H =:= ",", check3Tuple(T,5);
    Flag = 5, List = [H|T], \+ H =:= 32,check3Tuple(T,6);
    Flag = 6, List = [H|T], H =:= ")", check3Tuple(T,7);
    Flag = 7;
    List = [H|_], H =:= 32, writeToFile(6),write('Error while parsing input file'),nl,halt(0);  %32 is ascii ''
    List = [_|T], check3Tuple(T,Flag);
    writeToFile(6),
    write('Error while parsing input file 3T'),nl,halt(0).
%------------------------Validity Checks End---------------------------
%Takes a single element and returns that element without spaces at end
removeLastSpaces(AtomStart, Return) :-
    atom_chars(AtomStart, List),
    reverse(List,Zspace),
    removeSpaces(Zspace,Z),
    reverse(Z, AtomFinish),
    atom_chars(Return, AtomFinish).

removeSpaces(List, ListMinusSpaces) :-
    List = [H|T], H = '\s',
    removeSpaces(T, ListMinusSpaces).

removeSpaces(List, List).
%--------------------------PARSING END--------------------------------
%---------------------------LOGIC START--------------------------------
logicMaster:-
    runFPA([0,0,0,0,0,0,0,0],1,Tasks),
    unassignedTasks(Tasks, [1,2,3,4,5,6,7,8], 1, UnassignedList),
    write(Tasks),nl,
    write(UnassignedList),nl,!,
    permutation(UnassignedList, PossibleAssignment),
    %\+ printIf(PossibleAssignment, [4,7,5,1,6,2,8], "Checking the best solution"),
    %write("New possble assignment "), write(PossibleAssignment),nl,
    %write("Bastpath so far found was: "), write(Tasks), write( " with a penalty of: "), write(Penalty),nl.
    %write("New permutation"), nl,
    possibleSol(Tasks,Tasks,PossibleAssignment,1).

    %Debug tool
printIf(Check, Trigger, Message) :-
    Check = Trigger,
    write(Message),nl,fail.

isBest(Tasks) :-
    %write("new is best"),nl,
    Tasks = [H|Tail],
    findPenalty(Tasks, H, Penalty,1),
    setBest(Tasks,Penalty).

setBest(Tasks, Penalty) :-
    %write("check best"), nl,
    bestQual(Best),
    Penalty < Best,
    %write("new best quality of "), write(Penalty), nl,
    retract(bestQual(_)),
    assert(bestQual(Penalty)),
    retract(bestPath(_)),
    assert(bestPath(Tasks));
    bestQual(_);
    %write("first best quality of "), write(Penalty), nl,
    assert(bestQual(Penalty)),
    assert(bestPath(Tasks)).

findPenalty([Task1,Task2|Other],FirstTask,NewPenalty, Machine) :-
    \+ tnpNode(Task1,Task2,_),
    treeNode(Machine,Task1,BasePenalty),
    NewMachine is Machine+1,
    findPenalty([Task2|Other],FirstTask,Penalty,NewMachine),
    NewPenalty is Penalty+BasePenalty;

    tnpNode(Task1,Task2,TNP),
    treeNode(Machine,Task1,BasePenalty),
    NewMachine is Machine+1,
    findPenalty([Task2|Other],FirstTask,Penalty,NewMachine),
    NewPenalty is Penalty+TNP+BasePenalty.

findPenalty([Task1|_],Task2,NewPenalty,Machine) :-
    \+ tnpNode(Task1,Task2,_),
    treeNode(Machine,Task1,BasePenalty),
    NewPenalty is BasePenalty;
    tnpNode(Task1,Task2,TNP),
    treeNode(Machine,Task1,BasePenalty),
    NewPenalty is TNP+BasePenalty.

possibleSol(Tasks,_,[],_) :-
    %\+ printIf(Tasks, [3,4,7,5,1,6,2,8], "Best possible solution found"),
    %write("New Possible Solution Check: "), write(Tasks),nl,
    Tasks = [H|_],
    tntCheck(Tasks, H),
    isBest(Tasks),
    !,fail.

possibleSol(Tasks, [H|Tail],[NextTask|RemainTask],Machine):-
    NextMachine is Machine+1,
    member(H,[0]),                              %If head of list is 0
    \+ forbidMNode(Machine, NextTask),
    insertTask(Tasks, NextTask, Machine, NewTasks),
    possibleSol(NewTasks, Tail, RemainTask,NextMachine);
    \+ member(H,[0]),                           %This is taken when H is not 0
    NextMachine is Machine+1,
    possibleSol(Tasks,Tail,[NextTask|RemainTask],NextMachine).

tntCheck([Task1,Task2|Other],FirstTask) :-
    %write("TNT check"),nl,
    \+ tntNode(Task1,Task2),
    tntCheck([Task2|Other],FirstTask);
    !,fail.

tntCheck([Task1|_],Task2) :-
    \+ tntNode(Task1,Task2).

permutation([],[]).
permutation([X|L],P) :-
    length(L, Length),
    permutation(L,L1),
    insert(X,L1,P).

insert(X, List, BiggerList) :-
    del(X, BiggerList, List).

del(X,[X|Tail], Tail).
del(X,[Y|Tail],[Y|Tail1]) :-
    del(X,Tail,Tail1).

unassignedTasks([H|T],List,8,UnassignedList):-
    member(H,List),
    delete(List,H,UnassignedList);
    List=UnassignedList.

unassignedTasks([H|T],List,N,UnassignedList):-
    member(H,List),
    delete(List,H,Updated),
    Next is N+1,
    unassignedTasks(T,Updated, Next, UnassignedList);
    Next is N+1,
    unassignedTasks(T,List, Next, UnassignedList).

runFPA(Sch, 9, Sch).

runFPA(Sch, Mach, Returned):-
    Next is Mach+1,
    safeFPA(Sch,Mach,NewSch),!,
    runFPA(NewSch,Next,Returned),!.

safeFPA(Tasks, Mach, Return):-
    fpaNode(Mach,Task),
    \+forbidMNode(Mach,Task),
    insertTask(Tasks, Task,Mach,Return).

safeFPA(List,Mach,List):-
    \+fpaNode(Mach,Task);
    writeToFile(7).

insertTask([_|T], Task, 1,[Task|T]).
insertTask([H|T], Task, Assignment,[H|T2]):-
    NextAssignment is Assignment -1,
    insertTask(T,Task,NextAssignment,T2).

%----------------------------LOGIC END----------------------------------
printOutput(OutputFile) :-
    bestPath(Path),
    bestQual(Qual),
    writeToFile(Path,Qual,OutputFile);
    writeToFile(7),nl.

reformatPath([],PathString,ConvertedString) :- ConvertedString = PathString.
reformatPath([H|Tail], PathString, ConvertedString) :-
    mapChar(H,Letter),
    NewPathString = [Letter|PathString],
    reformatPath(Tail, NewPathString,ConvertedString).

mapChar(1,"A").
mapChar(2,"B").
mapChar(3,"C").
mapChar(4,"D").
mapChar(5,"E").
mapChar(6,"F").
mapChar(7,"G").
mapChar(8,"H").

mapError(1,"partial assignment error").
mapError(2,"invalid machine/task").
mapError(3,"machine penalty error").
mapError(4,"invalid task").
mapError(5,"invalid penalty").
mapError(6,"Error while parsing input file").
mapError(7,"No valid solution possible!").

writeToFile([M1,M2,M3,M4,M5,M6,M7,M8],Qual,OutputFile) :-
    open(OutputFile , write, Stream),
    mapChar(M1,C1),
    mapChar(M2,C2),
    mapChar(M3,C3),
    mapChar(M4,C4),
    mapChar(M5,C5),
    mapChar(M6,C6),
    mapChar(M7,C7),
    mapChar(M8,C8),
    write(Stream, "Solution"), write(Stream, " "),
    write(Stream, C1), write(Stream, " "),
    write(Stream, C2), write(Stream, " "),
    write(Stream, C3), write(Stream, " "),
    write(Stream, C4), write(Stream, " "),
    write(Stream, C5), write(Stream, " "),
    write(Stream, C6), write(Stream, " "),
    write(Stream, C7), write(Stream, " "),
    write(Stream, C8),
    write(Stream, "; Quality: "),
    write(Stream, Qual),
    close(Stream),
    halt(0).

writeToFile(Error) :-
    outputFileName(X),
    open(X, write, Stream),
    mapError(Error, Message),
    write(Stream, Message),
    close(Stream),
    halt(0).
