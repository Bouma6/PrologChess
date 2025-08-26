% same square check
pos(C,R):- between(1,8,C), between(1,8,R).

% sort pieces so states compare equal even if order differs
canonical_pieces(Ps,PsC):- msort(Ps,PsC).


% Checks whether a position is occupied
occupied((C,R), Pieces):- member(piece(_, _,(C,R)), Pieces).
occupied((C,R), Pieces, Color) :- member(piece(_, Color, (C,R)), Pieces).

% Controls whether a path between two positions is clear
% But allow to capture on the last square
path_clear((Col1,Row1), (Col2,Row2), Pieces) :-
    DCol is sign(Col2 - Col1),
    DRow is sign(Row2 - Row1),
    path_clear_step((Col1,Row1), (Col2,Row2), (DCol,DRow), Pieces).

path_clear_step((Col,Row), (Col2,Row2), _, _) :-
    Col =:= Col2, Row =:= Row2, !.

path_clear_step((Col,Row), (Col2,Row2), (DCol,DRow), Pieces) :-
    NCol is Col + DCol, NRow is Row + DRow,
    (NCol =:= Col2, NRow =:= Row2 -> true
    ; \+ occupied((NCol,NRow), Pieces),
    path_clear_step((NCol,NRow), (Col2,Row2), (DCol,DRow), Pieces)).

% Legality of the moves 

% King
figure_move(king, _, (Col,Row), (NCol,NRow),_) :-
    pos(NCol,NRow),
    DCol is abs(Col-NCol), DRow is abs(Row-NRow),
    DCol =< 1, DRow =< 1, (DCol > 0 ; DRow > 0).

% Rook
figure_move(rook, _, (Col,Row), (NCol,NRow), Pieces) :-
    pos(NCol,NRow),
    (Col = NCol ; Row = NRow),
    (Col \= NCol ; Row \= NRow),
    path_clear((Col,Row), (NCol,NRow), Pieces).

% Bishop
figure_move(bishop, _, (Col,Row), (NCol,NRow), Pieces) :-
    pos(NCol,NRow),
    abs(Col-NCol) =:= abs(Row-NRow),
    (Col \= NCol ; Row \= NRow),
    path_clear((Col,Row), (NCol,NRow), Pieces).

% Queen
figure_move(queen, _, (Col,Row), (NCol,NRow), Pieces) :-
    pos(NCol,NRow),
    ( Col = NCol ; Row = NRow ; abs(Col-NCol) =:= abs(Row-NRow) ),
    (Col \= NCol ; Row \= NRow),
    path_clear((Col,Row), (NCol,NRow), Pieces).

% Knight 
figure_move(knight, _, (Col,Row), (NCol,NRow), _) :-
    pos(NCol,NRow),
    DCol is abs(Col-NCol), DRow is abs(Row-NRow),
    ( (DCol =:= 2, DRow =:= 1) ; (DCol =:= 1, DRow =:= 2) ).

% Pawn white
figure_move(pawn, white, (Col,Row), (Col,NRow), Pieces) :-
    NRow is Row+1, pos(Col,NRow),
    \+ occupied((Col,NRow), Pieces).

figure_move(pawn, white, (Col,Row), (Col,NRow), Pieces) :-           
    Row =:= 2, NRow is Row+2, pos(Col,NRow),
    Mid is Row+1,
    \+ occupied((Col,Mid), Pieces),
    \+ occupied((Col,NRow), Pieces).

figure_move(pawn, white, (Col,Row), (NCol,NRow), Pieces) :-          
    NRow is Row+1, (NCol is Col+1 ; NCol is Col-1),
    pos(NCol,NRow),
    occupied((NCol,NRow), Pieces, black).

% Pawn black

figure_move(pawn, black, (Col,Row), (Col,NRow), Pieces) :-           
    NRow is Row-1, pos(Col,NRow),
    \+ occupied((Col,NRow), Pieces).

figure_move(pawn, black, (Col,Row), (Col,NRow), Pieces) :-           
    Row =:= 7, NRow is Row-2, pos(Col,NRow),
    Mid is Row-1,
    \+ occupied((Col,Mid), Pieces),
    \+ occupied((Col,NRow), Pieces).

figure_move(pawn, black, (Col,Row), (NCol,NRow), Pieces) :-          
    NRow is Row-1, (NCol is Col+1 ; NCol is Col-1),
    pos(NCol,NRow),
    occupied((NCol,NRow), Pieces, white).

% controls whether a piece is under attack
attacks(king, _, (Col,Row), (NCol,NRow), _) :-
    pos(NCol,NRow),
    DCol is abs(Col-NCol), DRow is abs(Row-NRow),
    DCol =< 1, DRow =< 1, (DCol>0 ; DRow>0).

attacks(rook, _, From, To, Pieces)   :- From \= To, same_line(From, To),   path_clear(From, To, Pieces).
attacks(bishop, _, From, To, Pieces) :- From \= To, diagonal(From, To),     path_clear(From, To, Pieces).
attacks(queen, _, From, To, Pieces)  :- From \= To, (same_line(From, To) ; diagonal(From, To)), path_clear(From, To, Pieces).


attacks(knight, _, (Col,Row), (NCol,NRow), _) :-
    pos(NCol,NRow),
    DCol is abs(Col-NCol), DRow is abs(Row-NRow),
    ( (DCol =:= 2, DRow =:= 1) ; (DCol =:= 1, DRow =:= 2) ).

attacks(pawn, white, (Col,Row), (NCol,NRow), _) :-
    NRow is Row+1, (NCol is Col+1 ; NCol is Col-1), pos(NCol,NRow).
attacks(pawn, black, (Col,Row), (NCol,NRow), _) :-
    NRow is Row-1, (NCol is Col+1 ; NCol is Col-1), pos(NCol,NRow).

attacked((C,R), Pieces, AttackingColor) :-
    member(piece(Type, AttackingColor, From), Pieces),
    attacks(Type, AttackingColor, From, (C,R), Pieces).
    
same_line((C,R1),(C,R2)) :- R1 \= R2.
same_line((C1,R),(C2,R)) :- C1 \= C2.
diagonal((C1,R1),(C2,R2)) :- abs(C1-C2) =:= abs(R1-R2).

% Remove piece at Pos if present
capture_at(_, [], []).
capture_at(Pos, [piece(_,_,Pos)|T], T) :- !.
capture_at(Pos, [H|T], [H|NT]) :- capture_at(Pos, T, NT).


% check if is side king in check
in_check(Color, Pieces) :-
    member(piece(king, Color, KP), Pieces),
    enemy(Color, Enemy),
    attacked(KP, Pieces, Enemy).

enemy(white, black).
enemy(black, white).

start_row(white,1).
start_row(black,8).

promotion_row(white,8).
promotion_row(black,1).

% If a pawn is at promotion square => promote else keep
promote_or_keep(Type, Player, (C,R), Rest, Pieces1) :-
    ( Type = pawn, promotion_row(Player, R) ->
        member(PType, [queen, rook, bishop, knight]),
        Pieces1 = [piece(PType, Player, (C,R))|Rest]
    ;   Pieces1 = [piece(Type, Player, (C,R))|Rest]
    ).

% Empty and not attacked checks 
empty_sq(Pieces, (C,R)) :- \+ occupied((C,R), Pieces).

sq_not_attacked(Player, Pieces, (C,R)) :-
    enemy(Player, Enemy),
    \+ attacked((C,R), Pieces, Enemy).

%puts pieces into correct order after castle
apply_castle(Pieces, Player, KingFrom, RookFrom, KingTo, RookTo, NewPieces) :-
    select(piece(king, Player, KingFrom), Pieces, P1),
    select(piece(rook, Player, RookFrom), P1, Rest),
    NewPieces = [piece(king, Player, KingTo), piece(rook, Player, RookTo) | Rest].

% castle king side(O-O)
castle_kingside(Player, Pieces, NewPieces) :-
    start_row(Player, Row),
    % king and rook in position 
    member(piece(king, Player, (5,Row)), Pieces),
    member(piece(rook, Player, (8,Row)), Pieces),
    %empty in between them while not attacked
    empty_sq(Pieces, (6,Row)),
    empty_sq(Pieces, (7,Row)),
    \+ in_check(Player, Pieces),
    sq_not_attacked(Player, Pieces, (6,Row)),
    sq_not_attacked(Player, Pieces, (7,Row)),
    apply_castle(Pieces, Player, (5,Row), (8,Row), (7,Row), (6,Row), NewPieces).

% castle qeen side (O-O-O)
castle_queenside(Player, Pieces, NewPieces) :-
    start_row(Player, Row),
    % king and rook in position 
    member(piece(king, Player, (5,Row)), Pieces),
    member(piece(rook, Player, (1,Row)), Pieces),
    %empty in between them while not attacked
    empty_sq(Pieces, (4,Row)),
    empty_sq(Pieces, (3,Row)),
    empty_sq(Pieces, (2,Row)),
    \+ in_check(Player, Pieces),
    sq_not_attacked(Player, Pieces, (4,Row)),
    sq_not_attacked(Player, Pieces, (3,Row)),
    apply_castle(Pieces, Player, (5,Row), (1,Row), (3,Row), (4,Row), NewPieces).

% Generates all legal moves 
% Start by trying out casteling
move(state(Player, Pieces0), state(NextPlayer, PiecesC)) :-
    canonical_pieces(Pieces0, Pieces),
    enemy(Player, NextPlayer),
    (castle_kingside(Player, Pieces, NewPieces);castle_queenside(Player, Pieces, NewPieces)),
    canonical_pieces(NewPieces, PiecesC).

% All other moves
move(state(Player, Pieces0), state(NextPlayer, PiecesC)) :-
    canonical_pieces(Pieces0, Pieces),
    enemy(Player, NextPlayer),
    % pick a piece and a target square consistent with movement rules
    member(piece(Type, Player, Pos), Pieces),
    figure_move(Type, Player, Pos, NewPos, Pieces),
    % cannot land on own piece
    \+ occupied(NewPos, Pieces, Player),
    % remove captured enemy if any                         
    capture_at(NewPos, Pieces, Temp1),
    select(piece(Type, Player, Pos), Temp1, Rest),
    promote_or_keep(Type, Player, NewPos, Rest, Pieces1),
    % own king not in check after move
    \+ in_check(Player, Pieces1),          
    canonical_pieces(Pieces1, PiecesC).

% Mate detection
is_mate(Pieces) :-
    in_check(black, Pieces),
    \+ move(state(black, Pieces), _).

forced_mate_white(N, state(white, Pieces), Visited, [state(white, Pieces)|Tail]) :-
    N >= 0,
    % try a legal white move
    move(state(white, Pieces), state(black, AfterWhite)),
    StateAfterWhite = state(black, AfterWhite),
    \+ member(StateAfterWhite, Visited),
    (is_mate(AfterWhite)-> Tail = [StateAfterWhite];
    % otherwise must still force mate after any Black defense
        N > 0,
        findall(NextW, move(StateAfterWhite, state(white, NextW)), BlackResponses),
        BlackResponses \= [],
        all_responses_win(BlackResponses, N-1, [StateAfterWhite|Visited]),
        member(ChosenW, BlackResponses),
        forced_mate_white(N-1, state(white, ChosenW), [state(white, ChosenW),StateAfterWhite|Visited], SubTail),
        Tail = [StateAfterWhite|SubTail]
    ).

% Check that for every black reply white can still force mate in N-1
all_responses_win([], _, _) :- !.
all_responses_win([WPos|Rest], N, Visited) :-
    can_mate_white(N, state(white, WPos), [state(white, WPos)|Visited]),
    all_responses_win(Rest, N, Visited).

% Can White force mate from this White-to-move position within N moves?
can_mate_white(N, state(white, Pieces), Visited) :-
    N >= 0,
    move(state(white, Pieces), state(black, AfterWhite)),
    StateAfterWhite = state(black, AfterWhite),
    \+ member(StateAfterWhite, Visited),
    (is_mate(AfterWhite);   N > 0,
    findall(NextW, move(StateAfterWhite, state(white, NextW)), BlackResponses),
    BlackResponses \= [],
    all_responses_win(BlackResponses, N-1, [StateAfterWhite|Visited])
    ).

% Path is a forward list of states from start (white to move) to final mate state (black to move).
solve(Path, MaxMoves, InitialPieces0) :-
    canonical_pieces(InitialPieces0, InitialPieces),
    Max1 is MaxMoves-1,
    forced_mate_white(Max1, state(white, InitialPieces), [state(white, InitialPieces)], Path),
    write('Solution found:'), nl,
    print_path(Path).

% Find a moved piece between two states:
% 1) normal move same Type before and after
% 2) promotion pawn -> queen,rook,bishop,knight
moved_or_promo(Player, Before, After, Type, (C1,R1), (C2,R2), Promo) :-
    % try normal move first
    member(piece(Type, Player, (C2,R2)), After),
    \+ member(piece(Type, Player, (C2,R2)), Before),
    member(piece(Type, Player, (C1,R1)), Before),
    \+ member(piece(Type, Player, (C1,R1)), After),
    Promo = none.

moved_or_promo(Player, Before, After, pawn, (C1,R1), (C2,R2), promo(NewType)) :-
    % promotion => After has NewType at To, Before had a pawn at From
    member(piece(NewType, Player, (C2,R2)), After),
    NewType \= pawn,
    \+ member(piece(_, Player, (C2,R2)), Before),
    member(piece(pawn, Player, (C1,R1)), Before),
    \+ member(piece(_, Player, (C1,R1)), After).

% square name changer to pretty (C,R) -> A1..H8
sq_name((C,R), Name) :-
    LCode is 64 + C, char_code(L, LCode),
    format(atom(Name), '~w~w', [L, R]).

print_path([_]).  % last state has no move
print_path([state(Player, Before), state(_, After)|Rest]) :-
    (   % --- detect castling (king moved two files) ---
        member(piece(king, Player, (C1,R)), Before),
        member(piece(king, Player, (C2,R)), After),
        abs(C2-C1) =:= 2
    ->  ( C2 > C1
        -> format('~w castles kingside (O-O)~n', [Player])
        ;  format('~w castles queenside (O-O-O)~n', [Player])
        )
    ;   % --- normal move or promotion ---
        moved_or_promo(Player, Before, After, Type, From, To, Promo),
        sq_name(From, FromA), sq_name(To, ToA),
        (   Promo = promo(NewType)
        ->  format('~w moves ~w from ~w to ~w=~w~n',
                   [Player, Type, FromA, ToA, NewType])
        ;   format('~w moves ~w from ~w to ~w~n',
                   [Player, Type, FromA, ToA])
        )
    ),
    print_path([state(_, After)|Rest]).
/*
?- solve(Path, 2, [
        piece(king, white, (3,3)),
        piece(queen, white, (4,4)),
        piece(king, black, (1,1)),
        piece(bishop, black, (1,2)),
        piece(rook, black, (2,1))
    ]).

?- solve(Path, 1, [
        piece(king, white, (6,5)),
        piece(rook, white, (4,3)),
        piece(king, black, (8,5))
    ]).

?- solve(Path, 4, [
        piece(king, white, (1,1)),
        piece(rook, white, (2, 1)),
        piece(rook, white, (4, 2)),
        piece(king, black, (5, 8))
    ]).

?- solve(Path, 4, [
        piece(king, white, (1,1)),
        piece(king, black, (6, 6))
    ]).

?- solve(Path, 3, [
        piece(king, white, (1,1)),
        piece(knight, white, (2,2)),
        piece(knight, white, (3,3)),
        piece(king, black, (6, 6))
    ]).    
    7 1 = g 1
?- solve(Path, 2, [
        piece(king, white, (7,1)),
        piece(pawn, white, (3,2)),
        piece(pawn, white, (1,3)),
        piece(pawn, white, (8,3)),
        piece(knight, white, (6,3)),
        piece(bishop, white, (5,3)),
        piece(queen, white, (6,4)),
        piece(pawn, white, (5,6)),
        piece(rook, white, (3,7)),
        piece(rook, black, (5,8)),
        piece(pawn, black, (8,7)),
        piece(pawn, black, (7,7)),
        piece(queen, black, (5,2)),
        piece(bishop, black, (2,2)),
        piece(bishop, black, (5,4)),
        piece(pawn, black, (1,6)),
        piece(pawn, black, (2,5)),
        piece(pawn, black, (4,5)),
        piece(king, black, (7,8))
    ]).
?- solve(Path, 2, [
        piece(king, white, (1,1)),
        piece(rook, white, (2,7)),
        piece(pawn, white, (1,6)),
        piece(king, black, (8, 8))
    ]).  

? - solve(Path,2,[
    piece(king,  white, (5,1)),
    piece(rook,  white, (1,1)),
    piece(rook,  white, (8,1)),
    piece(bishop,white, (5,2)),
    piece(knight,white, (6,6)),
    piece(knight,white, (7,4)),
    piece(pawn,  white, (1,2)),
    piece(pawn,  white, (2,2)),
    piece(pawn,  white, (3,2)),
    piece(pawn,  white, (4,2)),
    piece(pawn,  white, (6,2)),
    piece(pawn,  white, (7,3)),
    piece(pawn,  white, (8,4)),
    piece(king,  black, (7,2)),
    piece(queen, black, (5,7)),
    piece(rook,  black, (1,8)),
    piece(rook,  black, (6,8)),
    piece(bishop,black, (2,7)),
    piece(knight,black, (2,8)),
    piece(pawn,  black, (1,7)),
    piece(pawn,  black, (2,6)),
    piece(pawn,  black, (3,7)),
    piece(pawn,  black, (4,7)),
    piece(pawn,  black, (5,6)),
    piece(pawn,  black, (7,7))    
    ]).

*/
