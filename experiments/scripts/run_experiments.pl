:-module(run_experiments, [run_mtg_fragment/4
                          ,run_robots/4
                          ,run_coloured_graph/4
                          ]).

/** <module> Run metarule learning experiments.

Running the experiments
=======================

This module file must be loaded into Swi Prolog with the current
directory set to this file's root directory, i.e.:

mlj_2021_experiments/experiments/scripts/


Running experiments at the system command line
----------------------------------------------

To run the experiments on the linux command line or windows powershell
run a command like the following:

==
% bash:
& '<swipl>' -s $script -g 'run_coloured_graph(acc,2,1,[])' -t halt

% psh:
& '<swipl>' -s $script -g 'run_coloured_graph(acc,2,1,[])' -t halt
==

Where "<swipl>" is the path to the Swi-Prolog executable binary on your
system.

The two scripts run_experiments.sh and run_experiments.ps1 in the same
directory as this module file can be used to run all experiments in
succession.

Remember to change the current directory to the root directory of this
module file before running the bash or powershell scripts above.

Note that the windows script does not run in cmd.exe.


Running experiments in the Swi-Prolog IDE
-----------------------------------------

To run the experiments in the Swi-Prolog IDE instead of the system
command line first comment-out the following line in the source below:

==
:-['../../louise/load_headless'].
==

Then load Louise by loading its load_project.pl startup file under
the root louise/ directory.

==
% In louise/
?- [load_project].
==

Then, load this module at the Swi-Prolog command line:
==
?- use_module('../experiments/scripts/run_experiments.pl').
true.
==

Finally, change Swi-Prolog's current working directory to the root
directory of the run_experiments.pl module:

==
?- cd('../experiments/scripts').
==

Now you can call any of the run_<experiment> predicates defined
herewith in the Swi-Prolog command line, e.g.:

==
?- run_coloured_graph(acc,2,1,[]).
==

*/

:-['../../louise/load_headless'].

% Stops warnings that options set in the original configuration of an
% experiment file have been clobbered by the experiment configuration.
:- set_prolog_flag(warn_override_implicit_import, false).

% Defines debug subjects that will be logged to terminal.
% Logging to file is defined in metarule_reduction/learning_curve.pl
:-['../configuration/logging_configuration.pl'].

% Uncomment for development. Comment out to run experiments.
%:-edit('run_experiments.pl').
%:-edit(metarule_reduction/metarule_reduction).

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
        % Path relative to this file.
        ,load_config('../configuration/mtg_configuration.pl')
        % Must be loaded after loading experiment configuration
        % to avoid redefinition permission errors in learning_curve.pl
        ,use_module('../metarule_reduction/metarule_reduction.pl')
        ,T = ability/2
        ,debug(progress,'~w: Starting on mtg_fragment dataset',[L])
        ,metarule_reduction:metarule_variation(T,M,K,S,Hs,_Ms,_SDs)
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
        % Path relative to this file.
        ,load_config('../configuration/robots_configuration.pl')
        % Must be loaded after loading experiment configuration
        % to avoid redefinition permission errors in learning_curve.pl
        ,use_module('../metarule_reduction/metarule_reduction.pl')
        ,moves_generator:write_dataset
        ,user:use_module(src(experiment_file))
        ,T = move/2
        ,debug(progress,'~w: Starting on robots dataset',[L])
        ,metarule_reduction:metarule_variation(T,M,K,S,Hs,_Ms,_SDs)
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
        % Path relative to this file.
        ,load_config('../configuration/coloured_graph_configuration.pl')
        % Must be loaded after loading experiment configuration
        % to avoid redefinition permission errors in learning_curve.pl
        ,use_module('../metarule_reduction/metarule_reduction.pl')
        %,graph_generator:write_dataset
        ,coloured_graph_target(T)
        ,debug(progress,'~w: Starting on coloured_graph dataset',[L])
        ,metarule_reduction:metarule_variation(T,M,K,S,Hs,_Ms,_SDs)
        ,debug(progress,'~w: Finished with coloured_graph dataset',[L]).


%!      coloured_graph_target(-Target) is det.
%
%       Predicate symbol and arity of couloured graph target predicate.
%
coloured_graph_target(M/A):-
        configuration:mislabelling_type(T)
        ,configuration:target_prefix_arity(P,A)
        ,atomic_list_concat([P,T],'_',M).
