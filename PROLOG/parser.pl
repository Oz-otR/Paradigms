%treeNode(Task, Machine, Penalty) will be added during runtime
%fpaNode(Task, Machine) will be added during runtime
%forbidMNode(Task, Machine) will be added during runtime
%tntNode(Task, Task) will be added during runtime

main :-
    %Get the arguments from command line
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, InputFile), % get first argument
    nth0(1, Argv, OutputFile), % get second argument
    %Open the file passed into the program on command line
    open(InputFile, read, Str), %this really doesn't need a comment to explain
    read_file(Str,Lines),
    close(Str),
%    write(Lines), nl,
    maplist(removeLastSpaces(), Lines, SpacelessLines),
    generatePredSeeds,
    validInput(SpacelessLines, Out),
    destroyPredSeeds,
%    write(Out), nl,
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
printTreeNodes() :- nl, write("---Tree---"), nl,treeNode(Task, Machine, Penalty), write((Task, Machine, Penalty)), nl, fail; true.
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




%-----------------DCG for parsing--------------------
validInput --> name,fpa,forbidM,tnt,machs,tnp.

%DCG for parsing Name information
name --> nameHeader, nameBody,arbLines.

nameHeader --> ['Name:'].
nameBody --> [X], {hasInternalSpace(X)}.


%DCG for parsing Forced Partial Assignment information
fpa --> fpaHeader,fpaBody('0').

fpaHeader --> ['forced partial assignment:'].
fpaBody(Flag) --> fpaEatPair(Flag, NewFlag), {NewFlag = '0'; NewFlag = '1'}, fpaBody(NewFlag) ;
	{NewFlag = '2', Flag = '1'}, [];
	{NewFlag = '2', Flag = '0'}, arbLines.
	
fpaEatPair(Flag, NewFlag) --> 
	[X], {parseFPA(X), NewFlag = '0'}; 
	[''], {NewFlag = '1'};
	[], {NewFlag = '2'}.


	
%DCG for parsing Forbidden Machine information
forbidM --> forbidHeader,forbidBody('0').

forbidHeader --> ['forbidden machine:'].
forbidBody(Flag) --> forbidEatPair(Flag, NewFlag), 
	{NewFlag = '0'; NewFlag = '1'}, forbidBody(NewFlag) ;
	{NewFlag = '2', Flag = '1'}, [];
	{NewFlag = '2', Flag = '0'}, arbLines.
	
forbidEatPair(Flag, NewFlag) --> 
	[X], {parseForbid(X), NewFlag = '0'}; 
	[''], {NewFlag = '1'};
	[], {NewFlag = '2'}.

	
	
%DCG for parsing Too-near Penalty information
tnt --> tntHeader,tntBody('0').

tntHeader --> ['too-near tasks:'].
tntBody(Flag) --> tntEatPair(Flag, NewFlag), 
	{NewFlag = '0'; NewFlag = '1'}, tntBody(NewFlag) ;
	{NewFlag = '2', Flag = '1'}, [];
	{NewFlag = '2', Flag = '0'}, arbLines.
	
tntEatPair(Flag, NewFlag) --> 
	[X], {parseTNT(X), NewFlag = '0'}; 
	[''], {NewFlag = '1'};
	[], {NewFlag = '2'}.


	
%DCG for parsing Machine Penalty information
machs --> machsHeader,machsBody,arbLines.

machsHeader --> ['machine penalties:'].
machsBody --> mN,mN,mN,mN,mN,mN,mN,mN.
mN --> [X], {parseArray(X)}.



%DCG for parsing Too-near Penalty information
tnp --> tnpHeader,tnpBody('0').

tnpHeader --> ['too-near penalities'].
tnpBody(Flag) --> tnpEatPair(Flag, NewFlag), 
	{NewFlag = '0'; NewFlag = '1'}, tnpBody(NewFlag) ;
	{NewFlag = '2', Flag = '1'}, [];
	{NewFlag = '2', Flag = '0'}, after.
	
tnpEatPair(Flag, NewFlag) --> 
	[X], {parseTNP(X), NewFlag = '0'}; 
	[''], {NewFlag = '1'};
	[], {NewFlag = '2'}.



%DCG for blank Newlines
arbLines --> [''],after.
after --> [''],after.
after --> [].





%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseFPA(Atom) :-
    atom_codes(Atom, List),
    List = [OB,INT,COMMA,CHAR,CB|Remainder],
    OB =:= "(",
    INT >= "1", INT =< "7",
    COMMA =:= ",",
    CHAR >= "A", CHAR =< "H",
    CB =:= ")",
    Remainder = [],
    char_code(Task, INT), char_code(Mach, CHAR),
    assert(fpaNode(Task,Mach));         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form
	\+ Atom = '', 
	\+ Atom = 'forbidden machine:', 
	write('Error with fpa format for pairing |'), write(Atom), write('|'), nl,halt(0).

	
	
	
%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseForbid(Atom) :-
    atom_codes(Atom, List),
    List = [OB,INT,COMMA,CHAR,CB|Remainder],
    OB =:= "(",
    INT >= "1", INT =< "7",
    COMMA =:= ",",
    CHAR >= "A", CHAR =< "H",
    CB =:= ")",
    Remainder = [],
    char_code(Task, INT), char_code(Mach, CHAR),
    assert(forbidMNode(Task,Mach));         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form
	\+ Atom = '', 
	\+ Atom = 'too-near tasks:', 
	write('Error with forbid format for pairing |'), write(Atom), write('|'), nl,halt(0).


%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseTNT(Atom) :-
    atom_codes(Atom, List),
    List = [OB,INT1,COMMA,INT2,CB|Remainder],
    OB =:= "(",
    INT1 >= "1", INT1 =< "7",
    COMMA =:= ",",
    INT2 >= "1", INT2 =< "7",
    CB =:= ")",
    Remainder = [],
    char_code(Task1, INT1), char_code(Task2, INT2),
    assert(tntNode(Task1,Task2));         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form
	\+ Atom = '', 
	\+ Atom = 'machine penalties:', 
	write('Error with tnt format for pairing |'), write(Atom), write('|'), nl,halt(0).
	
	
%Confirms an assignment FPA pair ex:(1,A) is of correct form.
parseTNP(Atom) :-
    atom_codes(Atom, List),
    List = [OB,INT1,COMMA,CHAR,COMMA,INT2,CB|Remainder],
    OB =:= "(",
    INT1 >= "1", INT1 =< "7",
    COMMA =:= ",",
    CHAR >= "A", INT2 =< "H",
	INT2 >= "1", INT1 =< "9",				%Needs to accept any int, not 1<X<9
    CB =:= ")",
    Remainder = [],
    char_code(Task, INT1), char_code(Mach, CHAR), char_code(Penalty, INT2),
    assert(tnpNode(Task,Mach,Penalty));         %Can change to  "assert(fpaNode(INT,CHAR));" if you want it in # not atom form
	\+ Atom = '', 
	write('Error with tnt format for pairing |'), write(Atom), write('|'), nl,halt(0).
	
	
	
	
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

	



%Checks if name is of valid form (no spaces)
hasInternalSpace(X):-
    atom_length(X,Y),
    Y>0,
    split_string(X,' ','',Z),
    length(Z,1);
    write('Error while parsing input file'),halt(0).


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
