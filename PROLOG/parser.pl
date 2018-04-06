%treeNode(Task, Machine, Penalty) are added during runtime
%fpaNode(Task, Machine) are added during runtime
%forbidMNode(Task, Machine) are added during runtime
%tntNode(Task, Task) are added during runtime
%tnpNode(Task, Machine, Penalty) are added during runtime

main :-
    %Get the arguments from command line
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, InputFile), % get first argument
    nth0(1, Argv, OutputFile), % get second argument
    %Open the file passed into the program on command line
    open(InputFile, read, Str), %this really doesn't need a comment to explain
    read_file(Str,Lines),
    close(Str),
    maplist(removeLastSpaces(), Lines, SpacelessLines),
    generatePredSeeds,
    validInput(SpacelessLines, Out),
    destroyPredSeeds,
    printAll.



%Creates an invalid version assignment of the predicates so inital checks dont crash
%(they are removed later)
generatePredSeeds() :-
    assert(fpaNode(-1,-1)),
    assert(forbidMNode(-1,-1)),
        assert(tntNode(-1,-1)),
        assert(treeNode(-1,-1,-1)),
        assert(tnpNode(-1,-1,-1)).

%Destroys predicate seeds
destroyPredSeeds() :-
    retract(fpaNode(-1,-1)),
    retract(forbidMNode(-1,-1)),
        retract(tntNode(-1,-1)),
        retract(treeNode(-1,-1,-1)),
        retract(tnpNode(-1,-1,-1)).




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




%----------------------DCG For Parsing Start------------------------
validInput --> name,fpa,forbidM,tnt,machs,tnp.

%DCG for parsing Name information
name --> after, nameHeader, after, nameBody, arbLines.

nameHeader --> ['Name:'].
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
machs --> machsHeader,after,machsBody,arbLines.

machsHeader --> ['machine penalties:'].
machsBody --> mN,after,after,mN,after,mN,after,mN,after,mN,after,mN,after,mN,after,mN.
mN --> [X], {parseArray(X)}.



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
%----------------------DCG For Parsing End------------------------


%-------------------------Parsers Start------------------------------
%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseFPA(Atom) :-
    \+ Atom = '',
        \+ Atom = 'forbidden machine:',
    atom_codes(Atom, List),
    check2Tuple(List,0),
    List = [OB,INT,COMMA,CHAR,CB|Remainder],
    checkChar(OB,"("),
    checkIntBounds(INT),
    checkChar(COMMA,","),
    checkCharBounds(CHAR),
    checkChar(CB,")"),
    checkEmpty(Remainder),
    Task is INT-"1", Mach is CHAR-"A",
    checkPA(Task,Mach),
    assert(fpaNode(Task,Mach)).         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form



%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseForbid(Atom) :-
    \+ Atom = '',
    \+ Atom = 'too-near tasks:',
    atom_codes(Atom, List),
    check2Tuple(List,0),
    List = [OB,INT,COMMA,CHAR,CB|Remainder],
    checkChar(OB,"("),
    checkIntBounds(INT),
    checkChar(COMMA,","),
    checkCharBounds(CHAR),
    checkChar(CB,")"),
    checkEmpty(Remainder),
    Task is INT-"1", Mach is CHAR-"A",
    assert(forbidMNode(Task,Mach)).         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form



%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseTNT(Atom) :-
    \+ Atom = '',
        \+ Atom = 'machine penalties:',
    atom_codes(Atom, List),
    check2Tuple(List,0),
    List = [OB,INT1,COMMA,INT2,CB|Remainder],
    checkChar(OB,"("),
    checkIntBounds(INT1),
    checkChar(COMMA,","),
    checkIntBounds(INT2),
    checkChar(CB,")"),
    checkEmpty(Remainder),
    Task1 is INT1-"1", Task2 is INT2-"1",
    assert(tntNode(Task1,Task2)).         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form


%Confirms array is of correct form, all nums >= 0, and asserts the results
parseArray(Atom):-
    split_string(Atom,' ','',AtomList),
    length(AtomList,8),
    maplist(atom_number(),AtomList,IntList),
    maplist(integer(),IntList),
    maplist(<(0) ,IntList),
    getMachine(0,Machine),
    assertArray(0,Machine,IntList).

%Finds next unassigned machine (Check =< 8 is intentional, do not make it 7)
getMachine(Check, Machine) :-
    \+ treeNode(_,Check,_), Machine is Check;
    Check =< 8, NewCheck is Check+1, getMachine(NewCheck, Machine).

%Asserts all elements in the list as new treeNodes
assertArray(_,_,[]).
assertArray(Task, Machine, [Penalty|Remaining]) :-
    assert(treeNode(Task, Machine, Penalty)),
    NewTask is Task+1,
    assertArray(NewTask, Machine, Remaining).



%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseTNP(Atom) :-
    \+ Atom = '',
    atom_codes(Atom, List),
    check3Tuple(List,0),
    List = [OB,INT1,COMMA1,CHAR,COMMA2|Remainder],
    checkChar(OB,"("),
    checkIntBounds(INT1),
    checkChar(COMMA1,","),
    checkCharBounds(CHAR),
    checkChar(COMMA2,","),
    checkPenalty(Remainder, Penalty, End),
    Penalty >= 0,
    checkEmpty(End),
    Task is INT1-"1", Mach is CHAR-"A",
    assert(tnpNode(Task,Mach,Penalty)).         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form


%-------------------------Parsers End------------------------------



%---------------------Validity Checks Start------------------------
%Checks if name is of valid form (no spaces)
hasInternalSpace(X):-
    atom_length(X,Y),
    Y>0,
    split_string(X,' ','',Z),
    length(Z,1);
    write('Error while parsing input file IS'),nl,halt(0).

%Checks if partial assignment has already been made to the task or machine
checkPA(Task, Mach) :-
    \+ fpaNode(Task,_),
    \+ fpaNode(_,Mach);
    write('partial assignment error'),nl,halt(0).


%Special check for TNP last number which could be more than one digit
%!Warning! This predicate is NOT VERSATILE, it only was designed for one use.
checkPenalty(String, Number, Remainder) :-
    atom_chars(Atom,String),
    split_string(Atom,')','',AtomList),
    AtomList = [H|T],
    atom_number(H, Number),
    Remainder = T;
    write('invalid task0'),halt(0).



%Checks format of pairing variabls and throws error if fail
checkChar(Char, ExpectedChar) :-
    Char =:= ExpectedChar;
    write('invalid task1'),halt(0).

checkCharBounds(Char) :-
    Char >= "A",
    Char =< "H";
    write('invalid task2'),halt(0).


checkIntBounds(Int) :-
    Int >= "1",
    Int =< "7";
    write('invalid task3'),halt(0).

checkEmpty(Remainder) :-
    Remainder = [];
    Remainder = [""];   %<-- Unlikely to cause issues but can be split off if needed
    write('Error while parsing input file Empty'),nl,halt(0).

%Checks that line starts with (_arb_,_arb_), Flag should always be started as 0
check2Tuple(List,Flag) :-
    Flag = 0, List = [H|T], H =:= "(", check2Tuple(T,1);
    Flag = 1, List = [H|T], \+ H =:= 32, check2Tuple(T,2);
    Flag = 2, List = [H|T], H =:= ",", check2Tuple(T,3);
    Flag = 3, List = [H|T], \+ H =:= 32,check2Tuple(T,4);
    Flag = 4, List = [H|T], H =:= ")", check2Tuple(T,5);
    Flag = 5;
    List = [H|_], H =:= 32, write('Error while parsing input file'),halt(0);  %32 is ascii ''
    List = [_|T], check2Tuple(T,Flag);
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
    List = [H|_], H =:= 32, write('Error while parsing input file'),nl,halt(0);  %32 is ascii ''
    List = [_|T], check3Tuple(T,Flag);
    write('Error while parsing input file 3T'),nl,halt(0).

%------------------------Validity Checks End---------------------------






%Takes a single element and returns that element without spaces at end
removeLastSpaces(AtomStart, Return) :- atom_chars(AtomStart, List), reverse(List,Zspace), removeSpaces(Zspace,Z), reverse(Z, AtomFinish), atom_chars(Return, AtomFinish).

removeSpaces(List, ListMinusSpaces) :- List = [H|T], H = '\s', removeSpaces(T, ListMinusSpaces).
removeSpaces(List, List).



/*
%%To_Do
%Define goal function, define output functionfor error messages, best path solutionandno valid solution.
%Best_path
bestPath = [].
%bestScore
%Define functions that have the best possible machine task assignmentas to resolve
%Getting the current command line argument
%Open the file of command line argument 2as the file foroutput
%tell(outputFile).
told
%Write error message/no valid solution/best solution in proper format to file
%%get_element_by_index
nth0(1, [a, b, c, d], E) % E = b
%get_index_by_element
nth0(I, [a, b, c, d], b) % I = 1
%enumerate_elements_with_their_corresponding_indexes
List = [a, b, c, d],
forall(nth0(I, List, E), format('List[~w]=~w~n', [I, E])).
%%example_above_prints
%Conclusion = "Solution: "
%List[0]=a
%List[1]=b
%List[2]=c
%List[3]=d
*/

?-main.
?-halt(0).

