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
    %validInput(Lines, Remainder), %needs to be connected
    maplist(removeLastSpaces(), Lines, SpacelessLines),
    validInput(SpacelessLines, Out),
    write(Out).



%Put all of the file into a list
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

%Split up the characters into row by row in a list and turns them into atoms?
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.

validInput --> name,fpa,forbidM,tnt,machs,tnp.

name --> nameHeader, nameBody,arbLines.

nameHeader --> ['Name:'].
nameBody --> [X], {hasInternalSpace(X)}.

%this is a section 
fpa-->fpaHeader,fpaBody,arbLines.

fpaHeader--> ['forced partial assignment:'].
fpaBody--> [].

%SECTIIONNN EXODIA
forbidM-->forbidHeader,forbidBody,arbLines.

forbidHeader--> ['forbidden machine:'].
forbidBody-->[].

%SECTIIONNN YAAS
tnt-->tntHeader,tntBody,arbLines.

tntHeader--> ['too-near tasks:'].
tntBody-->[].

%TRANSFORMERS SECTIONS
machs-->machsHeader,machsBody,arbLines.

machsHeader--> ['machine penalties:'].
machsBody-->mN,mN,mN,mN,mN,mN,mN,mN.
mN --> [X], {function(X)}.

function(X):- split_string(X,' ','',Y),length(Y,8),maplist(atom_number(),Y,Z),maplist(integer(),Z),maplist(<(0) ,Z).

%BEEEEEES?!
tnp-->tnpsHeader,tnpsBody,after.

tnpsHeader--> ['too-near penalities'].
tnpsBody-->[].


arbLines --> [''],after.
after-->[].
after-->[''],after.

hasInternalSpace(X):- atom_length(X,Y),Y>0,split_string(X,' ','',Z),length(Z,1);write('Error while parsing input file'),halt(0).
%validName(X) :- /+atom_length(X,0), maplist(white(),X,WhiteTable), write(WhiteTable), ln.


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
