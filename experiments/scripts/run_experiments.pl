:-module(run_experiments, [run_mtg_fragment/4
                          ,run_robots/4
                          ,run_coloured_graph/4
                          ]).

% Stops warnings that options set in the original configuration of an
% experiment file have been clobbered by the experiment configuration.
:- set_prolog_flag(warn_override_implicit_import, false).

:-use_module('../metarule_reduction/metarule_reduction.pl').
:-['../configuration/logging_configuration.pl'].

%:-edit(metarule_reduction/metarule_reduction).
:-edit('run_experiments.pl').

%!      load_config(+File) is det.
%
%       Load experiment-specific configuration options.
%
load_config(F):-
        load_files(F, [module(configuration)
                      ,redefine_module(true)
                      ]).

%!      run_mtg_fragment(+Metric,+Steps,+Samples,+Higher_Order) is det.
%
%       Run a learning rate experiment on the mtg_fragment.pl dataset.
%
%       Metric is an evaluation metric as defined in evaluation.pl (but
%       usually one of "time" or "acc" for accuracy). Steps is the
%       number of steps to train with each sub-set of the user-defined
%       metarules for this MIL problem. Samples is the sampling rate for
%       the training partition in each experiment step. Higher_Order is
%       a list of metarule identifiers of higher-order metarules.
%
run_mtg_fragment(M,K,S,Hs):-
        configuration:learner(L)
        % Path relative to louise/load_project.pl or louise/load_headless.pl.
        ,load_config('../experiments/configuration/mtg_configuration.pl')
        ,T = ability/2
        ,debug_problem_statistics(learning_curve,T)
        ,debug(progress,'~w: Starting on mtg_fragment dataset',[L])
        ,metarule_variation(T,M,K,S,Hs,_Ms,_SDs)
        ,debug(progress,'~w: Finished with mtg_fragment dataset',[L]).


%!      run_robots(+Metric,+Steps,+Samples,+Higher_Order) is det.
%
%       Run a learning rate experiment on the robots.pl dataset.
%
%       Note that the MIL problem in robots.pl is described as "grid
%       world navigation" in the paper.
%
run_robots(M,K,S,Hs):-
        configuration:learner(L)
        % Path relative to louise/load_project.pl or louise/load_headless.pl.
        ,load_config('../experiments/configuration/robots_configuration.pl')
        ,moves_generator:write_dataset
        ,user:use_module(src(experiment_file))
        ,T = move/2
        ,debug_problem_statistics(learning_curve,T)
        ,debug(progress,'~w: Starting on robots dataset',[L])
        ,metarule_variation(T,M,K,S,Hs,_Ms,_SDs)
        ,debug(progress,'~w: Finished with robots dataset',[L]).


%!      run_coloured_graph(+Metric,+Steps,+Samples,+Higher_Order) is det.
%
%       Run a learning rate experiment on the coloured_graph.pl dataset.
%
%       Note that the MIL problem in coloured_graph.pl is described as "grid
%       world navigation" in the paper.
%
run_coloured_graph(M,K,S,Hs):-
        configuration:learner(L)
        % Path relative to louise/load_project.pl or louise/load_headless.pl.
        ,load_config('../experiments/configuration/coloured_graph_configuration.pl')
        %,graph_generator:write_dataset
        ,coloured_graph_target(T)
        ,debug_problem_statistics(learning_curve,T)
        ,debug(progress,'~w: Starting on coloured_graph dataset',[L])
        ,metarule_variation(T,M,K,S,Hs,_Ms,_SDs)
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
%	,maplist(length,[Pos,Neg,BK,MS],[I,J,K,N])
        ,length(Pos,I)
        ,length(Neg,J)
        ,length(BK,K)
        ,length(MS,N)
        ,debug(S,'MIL problem statistics', [])
	,debug(S,'Positive examples:    ~w', [I])
	,debug(S,'Negative examples:    ~w', [J])
	,debug(S,'Background knowledge: ~w ~w', [K,BK])
	,debug(S,'Metarules:            ~w ~w~n', [N,MS]).

