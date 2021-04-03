:-module(metarule_reduction, [metarule_variation/7
                             ]).

:-use_module(learning_curve).
:-use_module(src(louise)).
:-use_module(src(meta_learning)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).
:-use_module(lib(evaluation/evaluation)).


metarule_variation(T,M,K,S,Hs,Ms,SDs):-
        learning_curve:start_logging(T)
        % Overrides metarules defined in experiment file
        ,configuration:metarules(MS)
        ,configuration:learning_curve_time_limit(L)
        ,experiment_data(T,Pos,Neg,BK,_)
        ,log_experiment_setup(T,L,M,K,[S],Pos,Neg,BK,MS,Hs)
        ,metarule_variations(T,L,[Pos,Neg,BK,MS],Hs,M,K,S,Rs)
        ,Rs \= []
        ,pairs_averages(Rs,Ms)
        ,pairs_sd(Rs,Ms,SDs)
        ,learning_curve:log_experiment_results(M,Ms,SDs)
        ,learning_curve:print_r_vectors(T,M,[S],Ms,SDs)
        ,learning_curve:close_log(learning_curve).


metarule_variations(T,L,Ts,Hs,M,K,S,Rs):-
        metarule_variations(0,K,T,L,Ts,Hs,M,S,[],Rs).

metarule_variations(K,K,_T,_L,_Ts,_Hs,_M,_S,Acc,Rs):-
        reverse(Acc,Rs)
        ,!.
metarule_variations(I,K,T,L,Ts,Hs,M,S,Acc,Bind):-
        debug(progress,'Step ~w of ~w',[I,K])
        ,metarule_subset_evaluations(T,L,Ts,Hs,M,S,Vs)
        ,succ(I,I_)
        ,metarule_variations(I_,K,T,L,Ts,Hs,M,S,[Vs|Acc],Bind).


metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS],Hs,M,S,Vs):-
        random_permutation(MS,MS_)
        ,debug(progress,'Random permutation of MS: ~w',[MS])
        ,metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS_],Hs,M,S,[],Vs).

metarule_subset_evaluations(T,L,[Pos,Neg,BK,[]],Hs,M,S,Acc,Vs):-
        !
        ,metarule_subset_evaluation(T,L,[Pos,Neg,BK,[]],Hs,M,S,N,V)
        ,reverse([N-V|Acc],Vs).
metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS],Hs,M,S,Acc,Bind):-
        metarule_subset_evaluation(T,L,[Pos,Neg,BK,MS],Hs,M,S,N,V)
        ,selectchk(_,MS,MS_)
        ,metarule_subset_evaluations(T,L,[Pos,Neg,BK,MS_],Hs,M,S,[N-V|Acc],Bind).


metarule_subset_evaluation(T,L,[Pos,Neg,BK,MS],Hs,M,S,N,V):-
        debug(progress,'User-defined metarules: ~w',[MS])
        ,learn_meta_reduction(T,L,[Pos,Neg,BK,MS],Hs,M,S,Ps,V)
        ,debug(progress,'Measured ~w: ~w',[M,V])
        ,debug(metarule_reduction_full,'Measured ~w: ~w sec',[M,V])
        ,debug_clauses(metarule_reduction_full,'Learned:',Ps)
        ,length(Ps, Len)
        ,debug(progress,'Hypothesis size: ~w',[Len])
        ,debug(metarule_reduction_full,'Hypothesis size: ~w',[Len])
        ,length(MS, N).


learn_meta_reduction(_T,L,[Pos,Neg,BK,MS],[],time,S,Ps,D):-
        !
        ,evaluation:train_test_splits(S,Pos,Pos_Train,_Pos_Test)
        ,evaluation:train_test_splits(S,Neg,Neg_Train,_Neg_Test)
        ,learning_curve:learn_timing([Pos_Train,Neg_Train,BK,MS],L,Ps,D).
learn_meta_reduction(_T,L,[Pos,Neg,BK,MS],Hs,time,S,Ps,D):-
        !
        ,evaluation:train_test_splits(S,Pos,Pos_Train,_Pos_Test)
        ,evaluation:train_test_splits(S,Neg,Neg_Train,_Neg_Test)

        % The output of this will need to be combined with MS below.
        ,debug(progress,'Learning new metarules: ~w',[MS])
        ,learn_metarules(Pos,Neg,BK,Hs,MS_1)
        ,expanded_metarules(MS,MS_e)
        ,combined(MS_1,MS_e,MS_)
        ,learning_curve:learn_timing([Pos_Train,Neg_Train,BK,MS_],L,Ps,D).
learn_meta_reduction(T,L,[Pos,Neg,BK,MS],[],M,S,Ps,V):-
        !
        ,once(timed_train_and_test(T,S,L,[Pos,Neg,BK,MS],Ps,M,V)).
learn_meta_reduction(T,L,[Pos,Neg,BK,MS],Hs,M,S,Ps,V):-
        debug(progress,'Learning new metarules: ~w',[MS])
        ,learn_metarules(Pos,Neg,BK,Hs,MS_1)
        ,expanded_metarules(MS,MS_e)
        ,combined(MS_1,MS_e,MS_)
        ,once(timed_train_and_test(T,S,L,[Pos,Neg,BK,MS_],Ps,M,V)).


combined(MS_1,MS_2,MS):-
        flatten([MS_1,MS_2],MS_)
        ,debug_quantified_metarules(progress,'All metarules:',MS_)
        ,copy_term(MS_,MS_c)
        ,findall(H-B
                ,(member(H:-B,MS_c)
                 ,numbervars(H:-B)
                 )
                ,MS_sk)
        ,sort(2,@<,MS_sk,MS_s)
        ,varnumbers(MS_s,MS_v)
        ,findall(H:-B
                ,(member(H-B,MS_v)
                 )
                ,MS)
        ,debug_quantified_metarules(progress,'Combined metarules:',MS).


%!	log_experiment_setup(+Target,+Limit,+Metric,+Steps,+Samples) is
%!	det.
%
%	Log configuration options and experiment parameters.
%
log_experiment_setup(T,L,M,K,Ss,Pos,Neg,BK,MS,Hs):-
	learning_curve:metric_name(M,N)
%	,maplist(length,[Pos,Neg,BK,MS],[I,J,K,N])
        ,length(Pos,L1)
        ,length(Neg,L2)
        ,length(BK,L3)
        ,length(MS,L4)
        ,length(Hs,L5)
        ,debug(learning_curve,'Experiment parameters:',[])
	,debug(learning_curve,'Target:           ~w',[T])
	,debug(learning_curve,'Metric:           ~w',[N])
	,debug(learning_curve,'Steps:            ~w',[K])
	,debug(learning_curve,'Samples:          ~w',[Ss])
	,debug(learning_curve,'Time limit (sec): ~w',[L])
	,debug(learning_curve,'',[])
        ,debug(learning_curve,'MIL problem statistics:',[])
        ,debug(learning_curve,'Positive examples:      ~w', [L1])
        ,debug(learning_curve,'Negative examples:      ~w', [L2])
	,debug(learning_curve,'Background knowledge:   ~w ~w', [L3,BK])
	,debug(learning_curve,'Metarules:              ~w ~w',[L4,MS])
        ,debug(learning_curve,'Higher order metarules: ~w ~w',[L5,Hs])
	,debug(learning_curve,'',[])
	,debug(learning_curve,'Configuration options:',[])
	,print_config(debug,learning_curve,all).
