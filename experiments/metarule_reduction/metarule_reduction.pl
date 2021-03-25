:-module(metarule_reduction, [metarule_variation/6
                             ]).

:-use_module('../learning_curve/learning_curve.pl').


metarule_variation(T,M,K,S,Ms,SDs):-
        learning_curve:start_logging(T)
        ,configuration:learning_curve_time_limit(L)
        ,learning_curve:log_experiment_setup(T,L,M,K,[S])
        ,experiment_data(T,Pos,Neg,BK,MS)
        ,metarule_variations(T,L,[Pos,Neg,BK,MS],M,K,S,Rs)
        ,Rs \= []
        ,pairs_averages(Rs,Ms)
        ,pairs_sd(Rs,Ms,SDs)
        ,learning_curve:log_experiment_results(M,Ms,SDs)
        ,learning_curve:print_r_vectors(T,M,[S],Ms,SDs)
        ,learning_curve:close_log(learning_curve).


metarule_variations(T,L,Ts,M,K,S,Rs):-
        metarule_variations(0,K,T,L,Ts,M,S,[],Rs).

metarule_variations(K,K,_T,_L,_Ts,_M,_S,Acc,Rs):-
        reverse(Acc,Rs)
        ,!.
metarule_variations(I,K,T,L,Ts,M,S,Acc,Bind):-
        debug(progress,'Step ~w of ~w',[I,K])
        ,metarule_subset_evaluations(T,L,Ts,M,S,Vs)
        ,succ(I,I_)
        ,metarule_variations(I_,K,T,L,Ts,M,S,[Vs|Acc],Bind).


metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS],M,S,Vs):-
        random_permutation(MS,MS_)
        ,debug(progress,'Random permutation of MS: ~w',[MS])
        ,metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS_],M,S,[],Vs).

metarule_subset_evaluations(_T,_L,[_Pos,_Neg,_BK,[]],_M,_S,Acc,Vs):-
        reverse(Acc,Vs)
        ,!.
metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS],time,S,Acc,Bind):-
        !
        ,debug(progress,'Metarules: ~w',[MS])
        ,evaluation:train_test_splits(S,Pos,Pos_Train,_Pos_Test)
        ,evaluation:train_test_splits(S,Neg,Neg_Train,_Neg_Test)
        ,learning_curve:learn_timing([Pos_Train,Neg_Train,BK,MS],L,Ps,D)
        ,debug(progress,'Duration: ~w sec',[D])
        ,debug(metarule_reduction_full,'Duration: ~w sec',[D])
        ,debug_clauses(metarule_reduction_full,'Learned:',Ps)
        ,length(Ps, Len)
        ,debug(progress,'Hypothesis size: ~w',[Len])
        ,debug(metarule_reduction_full,'Hypothesis size: ~w',[Len])
        ,length(MS, N)
        ,selectchk(_,MS,MS_)
        ,metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS_],time,S,[N-D|Acc],Bind).

metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS],M,S,Acc,Bind):-
        !
        ,debug(progress,'Metarules: ~w',[MS])
        % steps when training with nondet learning predicates
        ,once(timed_train_and_test(T,S,L,[Pos,Neg,BK,MS],Ps,M,V))
        ,debug(progress,'Measured ~w: ~w',[M,V])
        ,debug(metarule_reduction_full,'Measured ~w: ~w sec',[M,V])
        ,debug_clauses(metarule_reduction_full,'Learned:',Ps)
        ,length(Ps, Len)
        ,debug(progress,'Hypothesis size: ~w',[Len])
        ,debug(metarule_reduction_full,'Hypothesis size: ~w',[Len])
        ,length(MS, N)
        ,selectchk(_,MS,MS_)
        ,metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS_],M,S,[N-V|Acc],Bind).

