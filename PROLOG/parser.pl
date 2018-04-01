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
    isName(Lines). %needs to be connected


%Put all of the file into a list
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

%Split up the characters into row by row in a list and turns them into atoms?
read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.
/*
parseStart(List):-
    write("here"),
    isName(Name:,List).
    */

isName(Y):-
    Y = [X|Tail],
    D ='Name:',
    X == D.

isName(X):-
    (X = [Y|Tail],
    %W = '',
    split_string(Y,':','',Z),
    W = 'Name',
    Z=[H|T],
    W==H,
    write("Recieved the correct 'Name:'  "),nl
    );
    %write(T)
    write('Error while parsing input file'), nl,
    halt(0).
    %T == "".

spaceCheck(X):-
    X == '';
    X == '\s'.

%spaceCheck(X):-



?-main.

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
?-halt(0).
