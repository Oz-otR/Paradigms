%treeNode(Task, Machine, Penalty) will be added during runtime

main :-
    %Get the arguments from command line
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, InputFile), % get first argument
    nth0(1, Argv, OutputFile), % get second argument
    %Open the file passed into the program on command line
    open(InputFile, read, Str), %this really doesn't need a comment to explain
    read_file(Str,Lines),
    close(Str),
    write(Lines), nl,
    maplist(removeLastSpaces(), Lines, SpacelessLines),
    assert(treeNode(-1,-1,-1)), %Creates first treeNode to allow others to be added
    validInput(SpacelessLines, Out),
    retract(treeNode(-1,-1,-1)),
    write(Out), nl,
    printTreeNodes.


%Prints content of all current "treeNode" predicates for
printTreeNodes() :- treeNode(Task, Machine, Penalty), write((Task, Machine, Penalty)), nl, fail; true.


%Put all of the file into a list
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

%Split up the characters into row by row in a list and turns them into atoms?
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.

%DCG for parsing
validInput --> name,fpa,forbidM,tnt,machs,tnp.

%DCG for parsing Name information
name --> nameHeader, nameBody,arbLines.

nameHeader --> ['Name:'].
nameBody --> [X], {hasInternalSpace(X)}. %Checks if name is of valid form (no spaces)

%DCG for parsing Forced Partial Assignment information 
fpa-->fpaHeader,fpaBody,arbLines.

fpaHeader--> ['forced partial assignment:'].
fpaBody--> [].

%DCG for parsing Forbidden Machine information
forbidM-->forbidHeader,forbidBody,arbLines.

forbidHeader--> ['forbidden machine:'].
forbidBody-->[].

%DCG for parsing Too-near Penalty information
tnt-->tntHeader,tntBody,arbLines.

tntHeader--> ['too-near tasks:'].
tntBody-->[].

%DCG for parsing Machine Penalty information
machs-->machsHeader,machsBody,arbLines.

machsHeader--> ['machine penalties:'].
machsBody-->mN,mN,mN,mN,mN,mN,mN,mN.
mN --> [X], {parseArray(X)}.  

%Confirms array is of correct form, all nums >= 0, and asserts the results
parseArray(SpaceList):- 
    split_string(SpaceList,' ','',NoSpaceList),
    length(NoSpaceList,8),
    maplist(atom_number(),NoSpaceList,IntList),
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



%DCG for parsing Too-near Penalty information
tnp-->tnpsHeader,tnpsBody,after.

tnpsHeader--> ['too-near penalities'].
tnpsBody-->[].


arbLines --> [''],after.
after-->[].
after-->[''],after.

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


%spaceCheck(X):-


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
