:-module(mtg_configuration, [experiment_file/2
                            ,example_clauses/1
                            ,generalised_examples/1
                            ,generalise_learned_metarules/1
                            ,learner/1
                            ,learned_metarules_printing/1
                            ,learning_predicate/1
                            ,max_invented/1
                            ,minimal_program_size/2
                            ,metarule/2
                            ,metarule_constraints/2
                            ,metarule_learning_limits/1
                            ,recursion_depth_limit/2
                            ,recursive_reduction/1
                            ,reduce_learned_metarules/1
                            ,reduction/1
                            ,resolutions/1
                            ,symbol_range/2
                            ,tautology/1
                            ,theorem_prover/1
                            ,unfold_invented/1
                            ,op(100,xfx,metarule)
                             % Learning curve script options
                            ,copy_plotting_scripts/1
                            ,logging_directory/1
                            ,plotting_directory/1
                            ,r_data_file/1
                            ,learning_curve_time_limit/1
                             % World generator options
                            ,experiment_world/1
                            ,world_dimensions/2
                            ,output_directory/1
                            ,output_to/1
                            ,symbol/3
                            ,exported_moves/2
                             % Experiment specific
                            ,metarules/1
                            ]).

% Must be loaded before experiment file to allow experiment files to
% use set_configuration_option/2 without errors.
:-use_module(src(auxiliaries), [set_configuration_option/2]).
:-user:use_module(src(experiment_file)).
:-reexport(lib(program_reduction/reduction_configuration),
	   except([resolutions/1])).
:-reexport(lib(evaluation/evaluation_configuration)).
:-reexport(lib/sampling/sampling_configuration).


% Dynamic configuration options can be manipulated
% by means of set_configuration_option/2 in module auxiliaries.
:- dynamic max_invented/1
          ,minimal_program_size/2
          ,recursion_depth_limit/2
	  ,recursive_reduction/1
	  ,reduction/1
	  ,resolutions/1
	  ,theorem_prover/1
          ,unfold_invented/1.

% Allows experiment files to define their own, special metarules.
% BUG: Actually, this doesn't work- module quantifiers, again.
% Needs fixing.
:-multifile metarule/2.

% ================================================================================
% Metarule learning configuration options
% ================================================================================

generalise_learned_metarules(true).
learned_metarules_printing(pretty).
%metarule_learning_limits(none).
%metarule_learning_limits(coverset).
%metarule_learning_limits(sampling(1)).
metarule_learning_limits(metasubstitutions(1)).
reduce_learned_metarules(false).

% ================================================================================
% Common configuration options
% ================================================================================

example_clauses(call).
generalised_examples(fully).
% Path relative to louise/load_project.pl or louise/load_headless.pl.
experiment_file('../experiments/datasets/robots/robots.pl',robots).
learner(louise).
:-dynamic learning_predicate/1.
:-multifile learning_predicate/1.
max_invented(1).
minimal_program_size(2,inf).
recursion_depth_limit(dynamic_learning,none).
recursive_reduction(false).
reduction(plotkins).
resolutions(5000).
symbol_range(predicate, ['P','Q','R','S','T']).
symbol_range(variable, ['X','Y','Z','U','V','W']).
tautology(H:-B):-
        copy_term(H:-B,C_)
        ,clause_literals(C_,Ls)
        ,numbervars(Ls)
        ,sort(Ls,[_]).
theorem_prover(resolution).
unfold_invented(false).

% ================================================================================
% Common metarules and metarule constraints
% ================================================================================

abduce metarule 'P(X,Y)'.
unit metarule 'P(x,y)'.
projection_21 metarule 'P(x,x):- Q(x)'.
projection_12 metarule 'P(x):- Q(x,x)'.
identity metarule 'P(x,y):- Q(x,y)'.
inverse metarule 'P(x,y):- Q(y,x)'.
chain metarule 'P(x,y):- Q(x,z), R(z,y)'.
tailrec metarule 'P(x,y):- Q(x,z), P(z,y)'.
precon metarule 'P(x,y):- Q(x), R(x,y)'.
postcon metarule 'P(x,y):- Q(x,y), R(y)'.
switch metarule 'P(x,y):- Q(x,z), R(y,z)'.
swap metarule 'P(x,y):- Q(z,x), R(z,y)'.
% Metarules with abductible first-order existentially quantified
% variables. Also see abduce metarule above.
chain_abduce_x metarule 'P(X,y):- Q(X,z), R(z,y)'.
chain_abduce_y metarule 'P(x,Y):- Q(x,z), R(z,Y)'.
chain_abduce_z metarule 'P(x,y):- Q(x,Z), R(Z,y)'.
projection_21_abduce metarule 'P(X,X):- Q(X)'.
projection_12_abduce metarule 'P(X):- Q(X,X)'.
precon_abduce metarule 'P(X,y):- Q(X), R(X,y)'.
postcon_abduce metarule 'P(x,Y):- Q(x,Y), R(Y)'.
% Meta-metarules. Use only with meta_learning.pl
% WARNING Comment these out when learing with [all] metarules!
meta_dyadic metarule 'P(x,y):- Q(z,u), R(v,w)'.
meta_monadic metarule 'P(x,y):- Q(z,u)'.
meta_precon metarule 'P(x,y):- Q(z),R(u,v)'.
meta_postcon metarule 'P(x,y):- Q(z,u),R(v)'.
meta_projection_21 metarule 'P(x,y):- Q(z)'.
meta_projection_12 metarule 'P(x):- Q(y,z)'.

:- dynamic metarule_constraints/2.
:- multifile metarule_constraints/2.

% ================================================================================
% Learning curve script configuration options
% ================================================================================

copy_plotting_scripts(false).
%copy_plotting_scripts(scripts(learning_curve/plotting)).
logging_directory('../experiments/output/robots/').
plotting_directory('../experiments/output/robots/').
r_data_file('learning_curve_data.r').
learning_curve_time_limit(300).


% ================================================================================
% Dataset-specific configuration options
% ================================================================================

%configuration:learning_predicate(learn_meta/1).

% Triadic metarules - allow higher-order moves to be used.
configuration:tri_chain_1 metarule 'P(x,y):- Q(M,x,z), R(z,y)'.
configuration:tri_chain_2 metarule 'P(x,y):- Q(x,z), R(M,z,y)'.
configuration:tri_chain_3 metarule 'P(x,y):- Q(M,x,z), R(N,z,y)'.

configuration:meta_tri_chain_1 metarule 'P(x,y):- Q(M,z,u), R(v,w)'.
configuration:meta_tri_chain_2 metarule 'P(x,y):- Q(z,u), R(M,v,w)'.
configuration:meta_tri_chain_3 metarule 'P(x,y):- Q(M,z,u), R(N,v,w)'.

% World generator configuration
experiment_world(empty_world).
world_dimensions(2,2).
output_directory(worlds).
output_to(console).
symbol(robot,false,#).
symbol(robot,true,$).
symbol(ball,_,@).
symbol(obstacle,_,o).
symbol(goal,_,x).
symbol(floor,_,.).
exported_moves(empty_world,[move_right/2,move_left/2,move_up/2,move_down/2]).
exported_moves(simple_world,[move_right/2,move_left/2,move_up/2,move_down/2
                            ,pick_up/2,put_down/2]).
exported_moves(obstacles_world,[move_right/2,move_left/2,move_up/2,move_down/2
                               ,pick_up/2,put_down/2]).

% ================================================================================
% Experiment-specific configuration options
% ================================================================================

% Overrides metarules/2 in datasets/robots/robots.pl
metarules([abduce
          ,unit
          ,identity
          ,inverse
          ,chain
          ,tailrec
          ,switch
          ,swap
          ,chain_abduce_x
          ,chain_abduce_y
          ,chain_abduce_y
          ]).

% This line ensures the experiment file set in the configuration option
% experiment_file/2 is always updated when the configuration module is
% changed and reloaded. Don't remove it.
%
% DO NOT REMOVE THIS LINE!
:-experiment_file:reload.
