
% version for SICStus 4.x

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(helper).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% HELPER FUNCTIONS
possible_moves(Current, Other, PossMoves) :-
    findall([R,C,NR,NC],(member([R,C], Current),
                         neighbour_position(R,C,[NR,NC]),
	                     \+member([NR,NC], Current),
	                     \+member([NR,NC], Other)),
	                     PossMoves).

move_with_bigger_x([X1, Move1], [X2, _], X1, Move1):-
    X1 > X2.

move_with_bigger_x([X1, _], [X2, Move2], X2, Move2):-
    X2 >= X1.

move_with_smaller_x([X1, Move1], [X2, _], X1, Move1):-
    X1 < X2.

move_with_smaller_x([X1, _], [X2, Move2], X2, Move2):-
    X2 =< X1.

smaller(X, Y, X) :-
    X < Y.

smaller(X, Y, Y) :-
    X >= Y.

bigger(X, Y, X) :-
    X > Y.

bigger(X, Y, Y) :-
    X =< Y.

update_count_d(Draw, NDraw, 'draw') :-
    NDraw is Draw + 1.
update_count_d(Draw, NDraw, 'exhaust') :-
    NDraw is Draw + 1.
update_count_d(Draw, NDraw, 'stalemate') :-
    NDraw is Draw + 1.
update_count_d(D, D, W) :-
    \+ (W == 'draw' ; 
        W == 'stalemate'; 
        W == 'exhaust').

update_count_b(BWin, NBWin, 'b') :- 
    NBWin is BWin + 1.
update_count_b(B, B, W) :-
    \+ (W == 'b').

update_count_r(RWin, NRWin, 'r') :- 
    NRWin is RWin + 1.
update_count_r(R, R, W) :- 
    \+ (W == 'r').

update_count(Draw, NDraw, BWin, NBWin, RWin, NRWin, W) :-
    update_count_d(Draw, NDraw, W),
    update_count_b(BWin, NBWin, W),
    update_count_r(RWin, NRWin, W).


%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% BLOODLUST

bloodlust_move(Alive, OtherPlayerAlive, Move) :-
    possible_moves(Alive, OtherPlayerAlive, PossMoves),
    move_with_least_opponents(Alive, OtherPlayerAlive, PossMoves, Move).

move_with_least_opponents(Curr, Opponents, PossMoves, Best) :-
    m_w_l_opp(Curr, Opponents, PossMoves, 100, [], Best).

m_w_l_opp(_, _, [], _, A_Best, A_Best).

m_w_l_opp(Curr, Opponents, [Move | Rest], MinOpp, A_BestMove, BestMove) :-
    alter_board(Move, Curr, NewCurr),
    next_generation([NewCurr, Opponents], [_, CrankedOpp]),
    length(CrankedOpp, NumOpp),
    move_with_smaller_x([NumOpp, Move], [MinOpp, A_BestMove],
                        NewMinOpp, NewBestMove),
    m_w_l_opp(Curr, Opponents, Rest, NewMinOpp, NewBestMove, BestMove).

bloodlust('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
    bloodlust_move(AliveBlues, AliveReds, Move),
    alter_board(Move, AliveBlues, NewAliveBlues).

bloodlust('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
    bloodlust_move(AliveReds, AliveBlues, Move),
    alter_board(Move, AliveReds, NewAliveReds).


%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SELF PRESERVATION

self_preservation_move(Alive, Other, Move) :-
    possible_moves(Alive, Other, PossMoves),
    move_with_biggest_curr(Alive, Other, PossMoves, Move).

move_with_biggest_curr(Curr, Opponents, PossMoves, Best) :-
    m_w_b_c(Curr, Opponents, PossMoves, -1, [], Best).

m_w_b_c(_, _, [], _, A_Best, A_Best).

m_w_b_c(Curr, Opponents, [Move | Rest], MaxCurr, A_BestMove, BestMove) :-
    alter_board(Move, Curr, NewCurr),
    next_generation([NewCurr, Opponents], [CrankedCurr, _]),
    length(CrankedCurr, NumCurr),
    move_with_bigger_x([NumCurr, Move], [MaxCurr, A_BestMove],
                        NewMaxCurr, NewBestMove),
    m_w_b_c(Curr, Opponents, Rest, NewMaxCurr, NewBestMove, BestMove).


self_preservation('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds],
                  Move) :-
    self_preservation_move(AliveBlues, AliveReds, Move),
    alter_board(Move, AliveBlues, NewAliveBlues).

self_preservation('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds],
                  Move) :-
    self_preservation_move(AliveReds, AliveBlues, Move),
    alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% LAND GRAB
land_grab_move(Alive, Other, Move) :-
    possible_moves(Alive, Other, PossMoves),
    move_with_biggest_diff(Alive, Other, PossMoves, Move).

move_with_biggest_diff(Curr, Opponents, PossMoves, Best) :-
    m_w_b_d(Curr, Opponents, PossMoves, -100, [], Best).

m_w_b_d(_, _, [], _, A_Best, A_Best).

m_w_b_d(Curr, Opponents, [Move | Rest], MaxDiff, A_BestMove, BestMove) :-
    alter_board(Move, Curr, NewCurr),
    next_generation([NewCurr, Opponents], [CrankedCurr, CrankedOpp]),
    length(CrankedCurr, NumCurr),
    length(CrankedOpp, NumOpp),
    Diff is NumCurr - NumOpp,
    move_with_bigger_x([Move, Diff], [A_BestMove, MaxDiff], NewBestMove, NewMaxDiff),
    m_w_b_d(Curr, Opponents, Rest, NewMaxDiff, NewBestMove, BestMove).


land_grab('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
    land_grab_move(AliveBlues, AliveReds, Move),
    alter_board(Move, AliveBlues, NewAliveBlues).

land_grab('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
    land_grab_move(AliveReds, AliveBlues, Move),
    alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% MINIMAX

minimax_move(Alive, Other, Move) :-
    possible_moves(Alive, Other, PossMoves),
    maximise_min_utility(Alive, Other, PossMoves, Move).

maximise_min_utility(Curr, Opponents, PossMoves, Best) :-
    m_m_u(Curr, Opponents, PossMoves, -200, [], Best).

m_m_u(_, _, [], _, A_BestMove, A_BestMove).

m_m_u(Curr, Opponents, [Move | Rest], MinMaxUtility, A_BestMove, BestMove) :-
    alter_board(Move, Curr, NewCurr),
    next_generation([NewCurr, Opponents], [CrankedCurr, CrankedOpp]),
    possible_moves(CrankedOpp, CrankedCurr, PossOppMoves),
    move_with_min_utility(CrankedCurr, CrankedOpp, PossOppMoves, MinUtility),
    move_with_bigger_x([MinUtility, Move], [MinMaxUtility, A_BestMove],
                        NewMinMaxUtility, NewBestMove),
    m_m_u(Curr, Opponents, Rest, NewMinMaxUtility, NewBestMove, BestMove).

move_with_min_utility(Curr, Opp, OppPossMoves, MinUtility) :-
    m_w_m_u(Curr, Opp, OppPossMoves, 200, MinUtility).

m_w_m_u(_, _, [], A_MinUtility, A_MinUtility).

m_w_m_u(Curr, Opp, [OppMove | Rest], A_MinUtility, MinUtility) :-
    alter_board(OppMove, Opp, NewOpp),
    next_generation([Curr, NewOpp], [CrankedCurr, CrankedOpp]),
    length(CrankedCurr, NumCurr),
    length(CrankedOpp, NumOpp),
    CurrUtility is NumCurr - NumOpp,
    smaller(CurrUtility, A_MinUtility, NewMinUtility),
    m_w_m_u(Curr, Opp, Rest, NewMinUtility, MinUtility).

minimax('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
    minimax_move(AliveBlues, AliveReds, Move),
    alter_board(Move, AliveBlues, NewAliveBlues).

minimax('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
    minimax_move(AliveReds, AliveBlues, Move),
    alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% TEST_STRATEGY

test_strategy(N, S1, S2) :-
    start_config(random, Board),
    test_strategy_h(N, S1, S2, Board, 
                    0, 0, 0, 0,
                    250, 0, 0, 0).


test_strategy_h(0, S1, S2, _,
                Draws, BWin, RWin,
                MaxMoves, MinMoves, MoveCount,
                TotalTime, GameCount) :-
                AvgMove is (MoveCount / GameCount),
                AvgTime is (TotalTime / GameCount),
                format('~w vs ~w ~n', [S1, S2]),
                format('Draw : ~d ~n', [Draws]),
                format('Blue Won : ~d ~n', [BWin]),
                format('Red Won : ~d ~n', [RWin]),
                format('Longest game : ~d ~n', [MaxMoves]),
                format('Shortest game: ~d ~n', [MinMoves]),
                format('Average length : ~d ~n', [AvgMove]),
                format('Average time : ~3f ~n', [AvgTime]).

test_strategy_h(N, S1, S2, Board, 
                Draws, BWin, RWin,
                MaxMoves, MinMoves,
                MoveCount,
                TotalTime,
                GameCount) :- 
    now(Then), 
    make_move(Board, quiet, _, 'b', S1, 'r', S2, 0, TotalMoves, Winner),
    now(Now),
    NTotalTime is TotalTime + Now - Then,
    update_count(Draws, NDraws, BWin, NBWin, RWin, NRWin, Winner),
    NewN is N - 1,
    (Winner \== 'exhaust', 
    MaxMoves < TotalMoves ->
        NMaxMoves = TotalMoves;
    NMaxMoves = MaxMoves),
    smaller(TotalMoves, MinMoves, NMinMoves),
    (Winner \== 'exhaust' ->
        NMoveCount is MoveCount + TotalMoves ;
    NMoveCount = MoveCount),
    NGameCount is GameCount + 1,
    start_config(random, NBoard),
    test_strategy_h(NewN, S1, S2, NBoard, 
                    NDraws, NBWin, NRWin,
                    NMaxMoves, NMinMoves,
                    NMoveCount,
                    NTotalTime,
                    NGameCount).


