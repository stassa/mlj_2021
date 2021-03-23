:-module(run_experiments, [run_mtg_fragment/1
                          ,run_robots/1
                          ,run_coloured_graph/1
                          ]).

%:-use_module('../louise/data/scripts/learning_curve/learning_curve.pl').
:-use_module('learning_curve/learning_curve.pl').
:-[logging_configuration].

%!      load_config(+File) is det.
%
%       Load experiment-specific configuration options.
%
load_config(F):-
        load_files(F, [module(configuration)
                      ,redefine_module(true)
                      ]).

%!      run_mtg_fragment is det.
%
%       Run a learning rate experiment on the mtg_fragment.pl dataset.
%
run_mtg_fragment(M):-
        configuration:learner(L)
        ,load_config('../experiments/mtg_configuration.pl')
        ,T = ability/2
        ,debug_problem_statistics(learning_curve,T)
        %,K = 100
        %,float_interval(1,9,1,Ss)
        % Uncomment the following two lines to test experiment setup.
        % Comment the two lines above, first!
        ,K = 2 %5
        ,interval(1,1,1,Ss)
        ,debug(progress,'~w: Starting on mtg_fragment dataset',[L])
        ,learning_curve(T,M,K,Ss,_Ms,_SDs)
        ,debug(progress,'~w: Finished with mtg_fragment dataset',[L]).


%!      run_robots is det.
%
%       Run a learning rate experiment on the robots.pl dataset.
%
%       Note that the MIL problem in robots.pl is described as "grid
%       world navigation" in the paper.
%
run_robots(M):-
        configuration:learner(L)
        ,load_config('../experiments/robots_configuration.pl')
        ,moves_generator:write_dataset
        ,user:use_module(src(experiment_file))
        ,T = move/2
        ,debug_problem_statistics(learning_curve,T)
        %,K = 10
        %,float_interval(1,9,1,Ss)
        % Uncomment the following two lines to test experiment setup.
        % Comment the two lines above, first!
        ,K = 2 %5
        ,interval(1,1,1,Ss)
        ,debug(progress,'~w: Starting on robots dataset',[L])
        ,learning_curve(T,M,K,Ss,_Ms,_SDs)
        ,debug(progress,'~w: Finished with robots dataset',[L]).


%!      run_coloured_graph is det.
%
%       Run a learning rate experiment on the coloured_graph.pl dataset.
%
%       Note that the MIL problem in coloured_graph.pl is described as "grid
%       world navigation" in the paper.
%
run_coloured_graph(M):-
        configuration:learner(L)
        ,load_config('../experiments/coloured_graph_configuration.pl')
        %,graph_generator:write_dataset
        ,coloured_graph_target(T)
        ,debug_problem_statistics(learning_curve,T)
        %,K = 10
        %,float_interval(1,9,1,Ss)
        % Uncomment the following two lines to test experiment setup.
        % Comment the two lines above, first!
        ,K = 2 %5
        ,interval(1,1,1,Ss)
        ,debug(progress,'~w: Starting on coloured_graph dataset',[L])
        ,learning_curve(T,M,K,Ss,_Ms,_SDs)
        ,debug(progress,'~w: Finished with coloured_graph dataset',[L]).


%!      coloured_graph_target(-Target) is det.
%
%       Predicate symbol and arity of couloured graph target predicate.
%
coloured_graph_target(M/A):-
        configuration:mislabelling_type(T)
        ,configuration:target_prefix_arity(P,A)
        ,atomic_list_concat([P,T],'_',M).



%!	debug_problem_statistics(+Subject,+Target) is det.
%
%	Log statistics of the MIL problem for Target.
%
%	Currently this only lists the numbers of positive and negative
%	examples, background definitions and metarules in the initial
%	MIL problem for Target (i.e. before any automatic modifications
%	such as metarule extension).
%
debug_problem_statistics(S,T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,maplist(length,[Pos,Neg,BK,MS],[I,J,K,N])
        ,debug(S,'MIL problem statistics', [])
	,debug(S,'Positive examples:    ~w', [I])
	,debug(S,'Negative examples:    ~w', [J])
	,debug(S,'Background knowledge: ~w ~w', [K,BK])
	,debug(S,'Metarules:            ~w ~w~n', [N,MS]).
