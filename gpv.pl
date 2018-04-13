% compile cmd
% gplc <file.pl>
% run cmd 
%╚═➤ GLOBALSZ=131000, LOCALSZ=32768 ./<exe_name> in.txt out.txt
%Shit takes too much mem

:- dynamic(
    content/1,
    partialAssignment/2,
    forbiddenMachine/2,
    machinePenalty/3,
	tooNearPenalty/3,
	tooNear/2,
	error/1,
    score/1,
    path/1).

score(99999999999999999).
path([]).

task('A').
task('B').
task('C').
task('D').
task('E').
task('F').
task('G').
task('H').

:- initialization(main).
main :- 
    argument_value(1, X),
    argument_value(2, Y),
    inout(X,Y);
	halt.

inout(In,Out):-
	clrdb,
	see(In),
	read_file(1,X1),
	seen,
	removeLast(X1, Y1),
	append(Y1,"\n", Y2),%ensure whitepaces 
	asserta(content(Y2)), 
	asserta(error(nil)),
	parse,
	error(Z),!,
	err_check(Z,Out),
	retractall(error(_)),
	asserta(error(nil)),
	solver2,
	checkSolution,
	error(Other),!,
	err_check(Other, Out),
	retract(error(_)),
	solutionformat(Str),
	writeout(Out,Str),!,
	halt.
  
clrdb :-
	retractall(content(_)),
	retractall(error(_)),
	retractall(partialAssignment(_,_)),
	retractall(forbiddenMachine(_,_)),
	retractall(tooNear(_,_)),
	retractall(machinePenalty(_,_,_)),
	retractall(tooNearPenalty(_,_,_)).
  
% Read the current input stream 
% unify Code with the character 
% code of the next character. 
% is unified with -1 on end of file. 
read_file(-1,[]).
read_file(_,Y):-
    get0(Z),
    read_file(Z,W),
    Y = [Z|W]. 

writeout(Path,Str):-
  tell(Path),
  prnt(Str),
  told,!.

prnt(Str):-
  atom_codes(X,Str),  
  write(X).
  
%pass, no err
err_check(nil, _).
err_check(invalidPartialAssignment, X):-
  writeout(X,"partial assignment error"), fail.
err_check(invalidMachineTask, X):-
  writeout(X,"invalid machine/task"), fail.
err_check(invalidMachinePenalty, X):-
  writeout(X,"machine penalty error"), fail.
err_check(invalidTask, X):-
  writeout(X,"invalid task"), fail.
err_check(invalidPenalty, X):-
  writeout(X,"invalid penalty"), fail.
err_check(parseErr, X):-
  writeout(X,"Error while parsing input file"), fail.
err_check(noValidSolution, X):-
  writeout(X,"No valid solution possible!"), fail.

checkSolution:-
  path([]),
  retract(error(_)),
  asserta(error(noValidSolution)).
checkSolution.
    
solutionformat(FinalOutput):-
  Output1 = "Solution ",
  path(X),
  nth(1,X,Y1), atom_codes(Y1,Z1), append(Output1,Z1,Output1_), append(Output1_," ",Output2),
  nth(2,X,Y2), atom_codes(Y2,Z2), append(Output2,Z2,Output2_), append(Output2_," ",Output3),
  nth(3,X,Y3), atom_codes(Y3,Z3), append(Output3,Z3,Output3_), append(Output3_," ",Output4),
  nth(4,X,Y4), atom_codes(Y4,Z4), append(Output4,Z4,Output4_), append(Output4_," ",Output5),
  nth(5,X,Y5), atom_codes(Y5,Z5), append(Output5,Z5,Output5_), append(Output5_," ",Output6),
  nth(6,X,Y6), atom_codes(Y6,Z6), append(Output6,Z6,Output6_), append(Output6_," ",Output7),
  nth(7,X,Y7), atom_codes(Y7,Z7), append(Output7,Z7,Output7_), append(Output7_," ",Output8),
  nth(8,X,Y8), atom_codes(Y8,Z8), append(Output8,Z8,Output9), append(Output9,"; Quality: ",Output10),
  score(Y9),
  number_codes(Y9,Z9),
  append(Output10,Z9,FinalOutput), !.

%--------------------------PARSING START--------------------------------

parse :-
  parse_.
parse :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
parse.

parse_ :-
  content(X),!,
  titleHeader(X, R1),!,
  fpaHeader(R1, R2),!,
  fmHeader(R2,R3),!,
  tntHeader(R3, R4),!,
  mpHeader(R4, R5), !,
  tnpHeader(R5, R6),!,
  hasNoCrap(R6).

  %-------------------------Title.-------------------------
  
titleHeader([], []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
titleHeader(X, R) :- 
  removePrefix("Name:", X, Q),!,
  line_end(Q, R1),!,
  getTrimmedLine(R1, _, R2),!,
  line_end(R2, R).

fpaHeader([], []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
fpaHeader(X, R) :-
  removePrefix("forced partial assignment:", X, Q),!,
  line_end(Q, R1),!,
  parsePartialAssignments(R1, R).

fmHeader([], []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
fmHeader(X, R) :-
  removePrefix("forbidden machine:", X, Q),!,
  line_end(Q, L),!,
  parseForbiddenMachines(L, R).

tntHeader([], []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
tntHeader(X, R) :-
  removePrefix("too-near tasks:", X, Q),!,
  line_end(Q, L),!,
  parseTooNearTasks(L, R).

mpHeader([], []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
mpHeader(X, R) :-
  removePrefix("machine penalties:", X, Q),!,
  line_end(Q, L),!,
  parseMachinePenalties(L, R).

tnpHeader([], []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
tnpHeader(X, R) :-
  removePrefix("too-near penalities", X, Q),!,
  line_end(Q, L),!,
  parseTooNearPenalties(L, R).

hasNoCrap([]).
hasNoCrap([10|I]) :-
  hasNoCrap(I).
hasNoCrap([9|I]) :-
  hasNoCrap(I).
hasNoCrap([13|I]) :-
  hasNoCrap(I).
hasNoCrap([32|I]) :-
  hasNoCrap(I).
  

% --------------------Forced partial assignments.------------------

parsePartialAssignments(I, R) :-
  getTrimmedLine(I, [], R).
parsePartialAssignments(I, R) :-
  getTrimmedLine(I, Line, R1),!,
  parsePartialAssignment(Line),!,
  parsePartialAssignments(R1, R).

parsePartialAssignment(I) :-
  parsePartialAssignment_(I),!.
parsePartialAssignment(_) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidPartialAssignment)).

parsePartialAssignment_(I) :-
  error(nil),!,
  paTuple(I, M, T),!,
  \+ partialAssignment(M,_X),!,
  \+ partialAssignment(_Y,T),!,
  assertz(partialAssignment(M,T)), !.

paTuple(Word, M, T) :-
  paTuple_(Word, M, T).
paTuple(_, 0, 0) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
paTuple(_, 0, 0).
  
paTuple_(Word, M, T) :-
  removePrefix("(", Word, R1),!,
  notSpace(R1),!,
  machineNumber(R1, M, R2),!,
  removePrefix(",", R2, R3),!,
  notSpace(R3),!,
  taskConstraint(R3, T, R4),!,
  removePrefix(")", R4, []),!.


% ------------------Forbidden machines.---------------------

parseForbiddenMachines(I, R) :-
  getTrimmedLine(I, [], R).
parseForbiddenMachines(I, R) :-
  getTrimmedLine(I, Line, R1),!,
  parseForbiddenMachine(Line),!,
  parseForbiddenMachines(R1, R).

parseForbiddenMachine(I) :-
  parseForbiddenMachine_(I),!.
parseForbiddenMachine(_) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidForbiddenMachine)).

parseForbiddenMachine_(I) :-
  error(nil),
  fmTuple(I, M, T),!,
  assertz(forbiddenMachine(M,T)), !.

fmTuple(Word, M, T) :-
  fmTuple_(Word, M, T).
fmTuple(_, 0, 0) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
  
fmTuple_(Word, M, T) :-
  removePrefix("(", Word, R1),!,
  notSpace(R1),!,
  machineNumber(R1, M, R2),!,
  removePrefix(",", R2, R3),!,
  notSpace(R3),!,
  taskConstraint(R3, T, R4),!,
  removePrefix(")", R4, []),!.


% --------------------Too-near tasks.-------------------

parseTooNearTasks(I, R) :-
  getTrimmedLine(I, [], R).
parseTooNearTasks(I, R) :-
  getTrimmedLine(I, Line, R1),!,
  parseTooNearTask(Line),!,
  parseTooNearTasks(R1, R).

parseTooNearTask(I) :-
  parseTooNearTask_(I),!.
parseTooNearTask(_) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidTooNearTask)).

parseTooNearTask_(I) :-
  error(nil),
  tnTuple(I, M, T),!,
  assertz(tooNear(M,T)), !.

tnTuple(Word, M, T) :-
  tnTuple_(Word, M, T).
tnTuple(_, 0, 0) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
  
tnTuple_(Word, M, T) :-
  removePrefix("(", Word, R1),!,
  notSpace(R1),!,
  taskConstraint(R1, M, R2),!,
  removePrefix(",", R2, R3),!,
  notSpace(R3),!,
  taskConstraint(R3, T, R4),!,
  removePrefix(")", R4, []),!.

% -----------------Machine penalties.----------------

parseMachinePenalties(I, R) :-
  parseMachinePenalties_(I, R1, 1),
  line_end(R1, R).
parseMachinePenalties(_, []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidMachinePenalty)).

 %Checks rows
parseMachinePenalties_(I, R, 8) :-
  error(nil),
  getTrimmedLine(I, Line, R),!,
  parseMachinePenalty(Line, 8).
parseMachinePenalties_(I, R, Num) :-
  error(nil),
  getTrimmedLine(I, Line, R1),!,
  parseMachinePenalty(Line, Num),!,
  Next is Num + 1,!,
  parseMachinePenalties_(R1, R, Next).
%
parseMachinePenalty(I, Row) :-
  parseMachinePenalty_(I, 1, Row),!.
parseMachinePenalty(_, _) :-
  error(nil),!,
  retract(error(nil)),!,
  asserta(error(parseErr)).

parseMachinePenalty_([], _, _) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidMachinePenalty)).
parseMachinePenalty_(I, _, _):-
  removePrefix(" ", I, _),!,
  error(nil),!,
  retract(error(nil)),!,
  asserta(error(parseErr)).
parseMachinePenalty_(I, 8, Row) :-
  error(nil),!,
  parseWord(I, Row, 8, []).
parseMachinePenalty_(I, Num, Row):-
  error(nil),!,
  parseWord(I, Row, Num, R),!,
  Next is Num + 1,!,
  parseMachinePenalty_(R, Next, Row).

parseWord(Line, M, T, R) :-
  getWord(Line, Word, R),!,
  penaltyNumber(Word, P, []),!,
  mapChar(T, Letter),!,
  assertz(machinePenalty(M, Letter, P)),!.


% -----------------------Too near penalties.------------------------

parseTooNearPenalties(I, R) :-
  getTrimmedLine(I, [], R).
parseTooNearPenalties(I, R) :-
  getTrimmedLine(I, Line, R1),!,
  parseTooNearPenalty(Line),!,
  parseTooNearPenalties(R1, R).

parseTooNearPenalty(I) :-
  parseTooNearPenalty_(I),!.
parseTooNearPenalty(_) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidTooNearTask)).

parseTooNearPenalty_(I) :-
  error(nil),
  tnpTuple(I, M, T, P),!,
  assertz(tooNearPenalty(M,T,P)), !.

tnpTuple(Word, M, T, P) :-
  tnpTuple_(Word, M, T, P).
tnpTuple(_, 0, 0) :-
  error(nil),
  retract(error(nil)),
  asserta(error(parseErr)).
  
tnpTuple_(Word, M, T, P) :-
  removePrefix("(", Word, R1),!,
  notSpace(R1),!,
  taskPenalty(R1, M, R2),!,
  removePrefix(",", R2, R3),!,
  notSpace(R3),!,
  taskPenalty(R3, T, R4),!,
  removePrefix(",", R4, R5),!,
  notSpace(R5),!,
  penaltyNumber(R5, P, R6),!,
  removePrefix(")", R6, []),!.


taskConstraint(I, P, T) :-
  error(nil),
  taskNumber(I, P, T).
taskConstraint(_, _, []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidMachineTask)).

taskPenalty(I, P, T) :-
  error(nil),
  taskNumber(I, P, T).
taskPenalty(_, _, []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidTask)).

taskNumber([65|T], 'A', T).
taskNumber([66|T], 'B', T).
taskNumber([67|T], 'C', T).
taskNumber([68|T], 'D', T).
taskNumber([69|T], 'E', T).
taskNumber([70|T], 'F', T).
taskNumber([71|T], 'G', T).
taskNumber([72|T], 'H', T).
mapChar(1, 'A').
mapChar(2, 'B').
mapChar(3, 'C').
mapChar(4, 'D').
mapChar(5, 'E').
mapChar(6, 'F').
mapChar(7, 'G').
mapChar(8, 'H').

penaltyNumber([], _, _) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidPenalty)).
penaltyNumber(I, O, R) :-
  error(nil),
  number(I, O, R).
penaltyNumber(_, _, []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidPenalty)).

isSpace([9|_]).
isSpace([10|_]).
isSpace([13|_]).
isSpace([32|_]).
notSpace(I) :- \+ isSpace(I).

getWord([], [], []).
getWord([9|I], [], I).
getWord([10|I], [], I).
getWord([10|I], [], I).
getWord([32|I], [], I).
getWord([C|I], [C|O], R) :-
	C \== 9,
	C \== 10,
	C \== 13,
	C \== 32,
	getWord(I, O, R).

  getLine([],[],[]).
getLine([10|I], [], I).
getLine([C|I], [C|Next], R) :-
  getLine(I, Next, R).

  getTrimmedLine(I, O, R):-
  getLine(I, Line, R),!,
  trim(Line, O).

%trim white space
trim([],[]).
trim([9], []).
trim([10], []).
trim([13], []).
trim([32], []).
trim([9|T], []) :- trim(T, []).
trim([10|T], []) :- trim(T, []).
trim([13|T], []) :- trim(T, []).
trim([32|T], []) :- trim(T, []).

trim([H|T], [H|O]) :-
  trim(T, O).

machineNumber([],_, _) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidMachineTask)).
machineNumber(I, O, R) :- 
  error(nil),
  number(I, O, R),
  O < 9,
  O > 0.
machineNumber(_, _, []) :-
  error(nil),
  retract(error(nil)),
  asserta(error(invalidMachineTask)).

number([H|T], O, R) :-
  error(nil),
  isDigit(H),!,
  number_([H|T], [], N, R),!,
  number_codes(O, N),!.

number_([H|T2], CSTATE, X, Res) :-
  error(nil),
  isDigit(H),
  append(CSTATE, [H], NEXT),
  number_(T2, NEXT, X, Res).
number_(T2, CSTATE, CSTATE, T2) :-
  error(nil).

%ascii code for digits 1-8
isDigit(N) :- N > 47,!, N < 58.

%rm end line Chars
line_end(X, R) :- removePrefix(" ", X, R1), line_end(R1, R).
line_end(X, R) :- removePrefix("\r",X,R1), line_end(R1,R).
line_end(X, R) :- removePrefix("\n", X, R).


% remove_last(IN_LIST, OUT_LIST).
% needed to remove end of file flag from list
removeLast([_|[]], []).
removeLast([X|T1], [X|T2]) :- removeLast(T1, T2).

removePrefix([], Y, Y).
removePrefix([H|[]], [H|Y], Y).
removePrefix([H|X], [H|Y], R) :- removePrefix(X, Y, R).


% -----------------------------------Logic------------------------


solver2 :-
  initFP([0,0,0,0,0,0,0,0], 1, State),
  getRemainingTasks_helper(State, ['A','B','C','D','E','F','G','H'], 1, Remaining),
  solve(State, Remaining),!.
solver2.

solve(State, []) :-
  eval_leaf(State).
solve(State, []).
solve(State, Remaining) :-
  eval_inner(State),
  solve_(State, Remaining, Remaining).
solve(State, Remaining).

solve_(State, Remaining, []).
solve_(State, Remaining, [Task|Tasks]) :-
  rmfst(Task, Remaining, Remaining_),
  assign(State, Task, State_),
  solve(State_, Remaining_),
  solve_(State, Remaining, Tasks).

assign(State, Task, State_) :-
  assign_(State, Task, State_).

assign_([], Task, []).
assign_([0|List], Task, [Task|List]).
assign_([X|List], Task, [X|State_]) :-
  assign_(List, Task, State_),!.

eval_inner(State) :-
  eval(State, Penalty),
  score(BestPenalty),
  Penalty < BestPenalty.

eval_leaf(State) :-
  eval_inner(State),
  getList(State, Penalty),
  retract(path(_)),
  asserta(path(State)),
  retract(score(_)),
  asserta(score(Penalty)).

eval(State, Penalty) :-
  valid(State),!,
  getList(State, Penalty),!.
  
valid(State) :-
  valid_(State, State, 1).

valid_([Task2|State], [Task1], Machine) :-
  check_for_forbidden(Machine, Task1),
  \+tooNear(Task1, Task2).
valid_(State, [Task1, Task2|Tasks], Machine) :-
  check_for_forbidden(Machine, Task1),
  \+tooNear(Task1, Task2),
  NextMachine is Machine + 1,
  valid_(State, [Task2|Tasks], NextMachine).


initFP(List,9,List).
initFP(List,Mach,Returned) :-
	error(nil),
	NewMach is Mach +1,
	setup(List,Mach,ModList),!,
	error(nil),
	initFP(ModList,NewMach,Returned),!.
	
setup(List,Mach,Return) :-
	partialAssignment(Mach,Task),
	check_for_forbidden(Mach,Task),
	replace_at_position(List,Task,Mach,Return).
	
setup(List,Mach,List) :-
	\+partialAssignment(Mach,Task).
	
setup(List,Mach,List) :-	
	partialAssignment(Mach,Task),
	\+check_for_forbidden(Mach,Task),
	retract(error(nil)),
	asserta(error(NoValid)).

replace_at_position([_|T],Task,1,[Task|T]).
replace_at_position([H|T],Task,Position,[H|Rest]) :-
	NextPosition is Position - 1,
	replace_at_position(T,Task,NextPosition,Rest).
	

check_for_forbidden(Mach,Task) :-
	\+forbiddenMachine(Mach,Task).	

getRemainingTasks_helper(List,Remain,N,Return) :-
	getRemainingTasks(List,Remain,N,Return),!.
	
getRemainingTasks([H|T],ListOfRemaining,8,RemainingTasks) :- 
	task(H),
	rmfst(H,ListOfRemaining,RemainingTasks).

getRemainingTasks([H|T],ListOfRemaining,N,RemainingTask) :- 
	task(H),
	rmfst(H,ListOfRemaining,List),
	Next is N+1,
	getRemainingTasks_helper(T,List,Next,RemainingTask).

getRemainingTasks([H|T],ListOfRemaining,8,ListOfRemaining) :- 
	\+task(H).


getRemainingTasks([H|T],ListOfRemaining,N,RemainingTask) :-
	\+task(H),
	Next is N+1,
	getRemainingTasks_helper(T,ListOfRemaining,Next,RemainingTask).
	
rmfst(Element,List,Return) :-
	select(Element,List,Return),!.
	
getList([],0).
getList(L,V) :-
	calc_nearPen(L,0,Result),
	giveList(L,1,X),
	sumList(X,Z),
	V is Result + Z.

sumList([],0).
sumList([H|T],Sum) :-
	sumList(T,Rest),
	Sum is H + Rest.

giveList([],_,[]).	

giveList([0|T],N,[0|Y]) :-	
	Next is N+1,
	giveList(T,Next,Y),!.

giveList([H|T],N,[X|Y]) :- 
	machinePenalty(N,H,X),
	Next is N+1,
	giveList(T,Next,Y),!.
	
calc_nearPen(L,Current,R) :-
	nearPen(L,Current,Result),!,
	last(L,Y),!,
	[Head|_]=L,
	too_near_pen(Y,Head,V),
	R is Result + V.

nearPen([],Current,Current).
nearPen([N],Current,Current).	
nearPen([N,M|Rest],Current,Result) :-
	too_near_pen(N,M,Val),
	Sum is Current + Val,
	nearPen([M|Rest],Sum,Result).
	
too_near_pen(X,Y,V) :-
	tooNearPenalty(X,Y,Z),!,
	0 < Z,
	V is Z;
	V is 0.

