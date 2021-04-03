:-module(utilities, [load_experiment_config/1
                    ,experiment_language_cardinality/3
                    ,metarule_language_cardinality/2
                    ,hypothesis_language_cardinality/2
                    ]).

:-use_module('../../louise/lib/term_utilities/term_utilities.pl').
:-use_module('../../louise/src/auxiliaries.pl').
:-use_module('../../louise/src/meta_learning.pl').
:-use_module('../../louise/src/mil_problem.pl').
:-use_module(run_experiments).

/** <module> Various utilities for inspecting and debugging experiments.

*/

%:- cd('../experiments/scripts').

% Switch to the scripts directory to facilitate running simulations
% Remember to comment-out the line that loads Louise in
% run_experiments.pl first. You'll see the errors anyway if you don't.
:- D = '../experiments/scripts'
  ,(   \+ working_directory(D,D)
   ->  cd(D)
   ;   true
   ).

:-debug(simulation).

%!      load_experiment_config(+Experiment) is det.
%
%       Load the configuration file of one Experiment.
%
%       Use this predicate to inspect configuration options for an
%       experiment. Useful to overcome the layers of indirection placed
%       between this script and the configuration of experiment
%       datasets.
%
load_experiment_config(mtg_fragment):-
        run_experiments:load_config('../configuration/mtg_configuration.pl').
load_experiment_config(robots):-
        run_experiments:load_config('../configuration/robots_configuration.pl').
load_experiment_config(coloured_graph):-
        run_experiments:load_config('../configuration/coloured_graph_configuration.pl').



%!      experiment_language_cardinality(+Experiment,+Length,-Card)
%!      is det.
%
%       Estimate the metarule language cardinality in an experiment.
%
%       This predicate simulates the specialisation of higher-order
%       metarules in an experiment to estimate the cardinality of the
%       set of specialisations of those higher order metarules.
%
experiment_language_cardinality(E,Hs,C):-
        load_experiment_config(E)
        ,learning_targets(Ts)
        ,higher_order_lengths(Hs,J,K)
        ,language_elements(Ts,J,K,C).


%!      higher_order_lengths(+Higer_Order,-Min,-Max) is det.
%
%       Extract the Min and Max lengths of Higher-Order metarules.
%
higher_order_lengths([higher_order(Min,Max)],Min,Max):-
        !.
higher_order_lengths(Hs,Min,Max):-
        expanded_metarules(Hs,MS)
        ,maplist(clause_literals,MS,Ls)
        ,maplist(length,Ls,Ns)
        ,min_list(Ns,J)
        ,max_list(Ns,K)
        % Account for encapsulated metasub atom
        ,Min is J - 1
        ,Max is K - 1.


%!      language_elements(+Targets,+J,+K,-Cardinality) is det.
%
%       Simulate specialisatiosn of third-order metarules of length K.
%
%       Targets is a set of learning targets defined in the currently
%       loaded experiment file.
%
%       J, K are natural numbers, the minimum and maximum number of
%       literals in third-order metarules and their specialisations.
%
%       Cardinality is a natural number, the number of fully ground
%       specialisations of third-order metarules of length J to K.
%
%       This predicate performs a simulated grounding of third-order
%       metarule literals to first-order clauses to estimate the
%       cardinality of a metarule language of third-order metarules of
%       length J to K and their specialisations.
%
language_elements(Ts,J,K,N):-
        auxiliaries:experiment_data(Ts,Pos,_Neg,_BK,_MS)
        ,examples_targets(Pos,Hs)
        ,predicate_signature(Ts,Ps)
        ,constant_signature(Pos,Cs)
        % Note that atoms in the Herbrand signature are ecnapsulated.
        ,meta_learning:herbrand_signature(Ts,Ss)
        ,maplist(length,[Hs,Ps,Cs,Ss],[T,P,C,A])
        ,debug(simulation,'Targets: ~w Bodies: ~w Constants: ~w Atoms: ~w',[T,P,C,A])
        ,metarules_specialisations(J,K,Hs,Ps,Cs,Ss,N).


%!      metarules_specialisations(+J,+K,+Hs,+Bs,+Cs,+Ss,-N) is det.
%
%       Generate specialisations of third-order metarules.
%
%       The "length" of a metarule is its number of (all) literals.
%
%       J, K are the minimum and maximum lengths of third-order
%       metarules.
%
%       Hs and Bs are the predicate symbols in the head and body
%       literals of instances of second-order metarules with K literals.
%
%       Cs is the set of constants in positive examples in the current
%       MIL problem.
%
%       Ss is the Herbrand Signature, the set of second-order metarule
%       literals that unify with the head literals of definitions of
%       background predicates and examples in the current MIL problem.
%
%       N is the number of fully-ground specialisations of each
%       third-order metarule of length in [J,K].
%
metarules_specialisations(J,K,Hs,Ps,Cs,Ss,N):-
        succ(K,K_)
        ,metarules_specialisations(J,K_,Hs,Ps,Cs,Ss,[],N).

metarules_specialisations(K,K,_Hs,_Ps,_Cs,_Ss,Acc,N):-
        sumlist(Acc,N)
        ,!.
metarules_specialisations(I,K,Hs,Ps,Cs,Ss,Acc,Bind):-
        debug(simulation,'Simulating specialisations of length ~w',[I])
        ,metarule_specialisations(I,Hs,Ps,Cs,Ss,N)
        ,succ(I,I_)
        ,metarules_specialisations(I_,K,Hs,Ps,Cs,Ss,[N|Acc],Bind).


%!      metarule_specialisations(+K,+Heads,+Bodies,+Consts,+Sig,-N)
%!      is det.
%
%       Generate specialisations of third-order metarules of length K.
%
%       The "length" of a metarule is its number of (all) literals.
%
%       K is the length of a third-order metarule.
%
%       Heads and Bodies are the predicate symbols of head and body
%       literals of instances of second-order metarules of length K.
%
%       Consts is the set of constants in the positive examples of the
%       target predicaets in the current MIL problem.
%
%       Sig is the Herbrand Signature, the set of all second-order
%       literals that univy with the heads of the definitions of
%       background predicaes and positive examples.
%
%       N is the number of second-order specialisations of length K of
%       the third-order metarule of length K.
%
metarule_specialisations(K,Hs,Ps,Cs,Ss,N):-
        literals_clauses(K,Ss,Ds)
        ,clauses_instances(Ds,Hs,Ps,Cs,Ms)
        ,debug_clauses(simulation_full,'Simulated specialisations:',Ms)
        ,length(Ms,N)
        ,debug(simulation,'Simulated ~w specialisations',[N]).


%!      literals_clauses(+K,+Signature,+Clauses) is det.
%
%       Generate clauses of second-order metarules.
%
%       K is the length of clauses to generate.
%
%       Signature is the Herbrand Signature, a list of second-order
%       literals from which second-order metarules can be constructed.
%
%       Clauses is a list of lists where each sub-list is a k-tuple of
%       literals in Signature. Each sub-list represents one second-order
%       metarule with the head of the list as the head literal.
%
literals_clauses(K,Ss,Cs):-
        debug(simulation,'Generating second-order clauses...',[])
        ,findall(Ls
               ,n_tuple(var,K,Ss,Ls)
               ,Cs)
        ,length(Cs,N)
        ,debug(simulation,'Generated ~w clauses.',[N]).


%!      clauses_instances(+Clauses,+Targets,+Symbols,+Constants,-Is)
%!      is det.
%
%       Instantiate a set of Clauses.
%
%       Clauses is a list-of-lists where each sub-list is a list of
%       second-order literals.
%
%       Targets is a list of predicate symbols of target predicates in
%       the current MIL problem.
%
%       Symbols is a list of predicate symbols of predicates in
%       background knowledge and examples (i.e. Targets is a subset of
%       Symbols).
%
%       Constants is the set of constants in positive examples of the
%       target predicates in the current MIL problem.
%
%       Is is a list of lists where each sub-list is a list of literals
%       in Clauses with existentially quantified variables instantiated
%       to symbols in Symbols and universally quantified variables in
%       Constants. The first existentially quantified variable in each
%       sub-list, representing the symbol of the head literal, is
%       instantiated only to predicates in Targets.
%
clauses_instances(Ds,Hs,Ps,Cs,Ms):-
        append(Hs,Ps,Bs)
        ,clauses_vars(Ds,Es,Us)
        ,debug(simulation,'Instantiating second-order clauses...',[])
        ,instantiations(Es,Us,Hs,Bs,Cs,Ms).


%!      clauses_vars(+Clauses,-Existential,-Universal) is det.
%
%       Extract variables from Clauses according to quantification.
%
%       Clauses is a list-of-lists where each sub-list is a list of
%       encapsulated second-order literals.
%
%       Existential and Universal are lists-of-lists where the i'th
%       sub-list of Existential is the list of existentially quantified
%       variables in the i'th set of literals in Clauses and the i'th
%       sub-list of Universal is the list of universally quantified
%       variables in the i'th set of literals in Clause.
%
clauses_vars(Ds,Es,Us):-
        clauses_vars(Ds,[],Es,[],Us).

clauses_vars([],Es,Es,Us,Us):-
        !.
clauses_vars([C|Ds],Acc_Es,Es,Acc_Us,Us):-
        clause_vars(C,Es_i,Us_i)
        ,clauses_vars(Ds,[Es_i|Acc_Es],Es,[Us_i|Acc_Us],Us).


%!      clause_vars(+Clause,-Existential,-Universal) is det.
%
%       Extract variables from a Clause according to quantification.
%
%       Clause is a list of encapsulated second-order literals.
%       Existential is the set of existentially quantified variables in
%       Clause and Universal is a list of the universally quantified
%       variables in Clause.
%
clause_vars(C,Es,Us):-
        clause_vars(C,[],Es,[],Us).

clause_vars([],Acc_Es,Es,Acc_Us,Us):-
        maplist(reverse,[Acc_Es,Acc_Us],[Es_r,Us_r])
        ,maplist(flatten,[Es_r,Us_r],[Es,Us])
        ,!.
clause_vars([L|Ls],Acc_Es,Es,Acc_Us,Us):-
        L =.. [m,E_i|Us_i]
        ,clause_vars(Ls,[E_i|Acc_Es],Es,[Us_i|Acc_Us],Us).


%!      instantiations(+Es,+Us,+Heads,+Bodies,+Constants,-Is) is det.
%
%       Generate all instantiations of sets of variables in clauses.
%
%       Es, Us are sets of existentially and universally quantified
%       variables in clauses of metarules
%
%       Heads, Bodies and Constants are predicate symbols and constants
%       that are to be used to instantiate the variables in Es and Us.
%
%       Is is a list of lists [Es_i,Us_i] where Es_i is a list of
%       existentially quantified variables instantiated to symbols
%       in Heads and Bodies and Us_i is a list of universally quantified
%       variables instantiated to constants in Constants.
%
%       The first element of each list Es_i is the existentially
%       quantified variable of a head literal and is instantiated only
%       to symbols in the list Heads, while remaining variables are
%       instantiated to Bodies. Bodies includes the symbols in BK and
%       the symbols in target predicates, so Heads is a subset of Bodies
%       and existentially quantified variables can be instantiated to
%       the predicate symbols of target predicates, representing
%       recursion.
%
instantiations(Es,Us,Hs,Bs,Cs,Is):-
        findall([[H|Es_i],Us_i]
               ,(nth1(I,Es,[H|Es_i])
                ,nth1(I,Us,Us_i)
                ,member(H,Hs)
                ,maplist(length,[[H|Es_i],Us_i],[N,M])
                ,maplist(n_tuple(ground),[N,M],[Bs,Cs],[[H|Es_i],Us_i])

                )
               ,Is_)
        ,sort(Is_, Is)
        ,debug(simulation,'Done instantiating second-order clauses',[]).


%!	n_tuple(+Inst,+N,+Xs,-Ys) is det.
%
%	Generate all N-tuples of elements in list Xs.
%
%	An n-tuple is a sequence of length n, with repetitions, drawn
%	from the elements in a set. For a set of cardinality k, there
%	are k^n n-tuples.
%
%       Inst is a constant denoting the instantiation of the terms in Xs
%       and is one of: [var,ground]. If Inst is "var", a copy of each
%       element of Xs is made before adding it to a new n-tuple, to
%       avoid having multiple copies of the same atom of the Herbrand
%       signature in each tuple. If Inst is "ground" no copy is made.
%
%       Suppose the Herbrand signature (passed as Xs) is a single atom:
%       [m(P,X,Y)]. Unless we make a fresh copy of this atom each time
%       we generate a tuple of atoms to represent a clause, we'd only
%       ever get tautologies of varying size:
%       ==
%       [m(P,X,Y),m(P,X,Y)]
%       [m(P,X,Y),m(P,X,Y),m(P,X,Y)]
%       [m(P,X,Y),m(P,X,Y),m(P,X,Y),m(P,X,Y)]
%       ...
%       ==
%
%       Copying makes for literals with fresh variables so we instead
%       generate clauses with literals with distinct variables (and no
%       tautologies):
%       ==
%       [m(P,X,Y),m(Q,Z,U)]
%       [m(P,X,Y),m(Q,Z,U),m(R,V,W)]
%       [m(P,X,Y),m(Q,Z,U),m(R,V,W),m(S,G,H)]
%       ...
%       ==
%
n_tuple(I,K, Xs, Ys):-
	length(Ys,K)
	,n_tuple(I,Xs,Ys).

%!	n_tuple(+Inst,+Xs,-Ys) is det.
%
%	Business end of n_tuple/3.
%
n_tuple(_,_,[]):-
	!.
n_tuple(var,Xs,[X_|Ys]):-
	member(X,Xs)
        ,copy_term(X,X_)
	,n_tuple(var,Xs,Ys).
n_tuple(ground,Xs,[X|Ys]):-
	member(X,Xs)
	,n_tuple(ground,Xs,Ys).


%!      constant_signature(+Examples,-Constants) is det.
%
%       Extract the Constants of a set of Examples.
%
%       This predicate tries to guess at the constant signature assuming
%       that a) all the constants in constant signature are to be found
%       in positive examples only (i.e. ignoring the BK) and b) that
%       examples are all ground.
%
%       Both big assumptions.
%
constant_signature(Es,Cs):-
        findall(As
               ,(member(E,Es)
                ,E =.. [_|As]
                )
               ,AS)
        ,flatten(AS,Fs)
        ,sort(Fs,Cs).


%!      metarule_language_cardinality(+Elements,-Cardinality) is det.
%
%       Calculate the Cardinality of a metarule language.
%
%       Elements is a list: [K,T,A,E,U,C,P], where:
%       * K is the maximum number of literals in third-order metarules.
%       * T is the number of target predicates in the current MIL
%       problem.
%       * A is the number of second-order atoms (i.e. the Herbrand
%       Signature).
%       * E is the number of existentially quantified first- and second-
%       order variables in second-order metarules with K literals.
%       * U is the number of universally quantified first-order
%       variables in second-order metarules with K literals.
%       * C is the number of constants (i.e. the Constants Signature).
%       * P is the number of background predicates in the current MIL
%       Problem.
%
metarule_language_cardinality([K,T,A,E,U,C,P], L):-
        findall(Li
               ,(between(1,K,I)
                ,Li is T * A^I * E^E * U^U * C^(E+U) * P^(I-1)
                )
               ,Ls)
        ,sumlist(Ls,L).



%!      hypothesis_language_cardinality(+Elements,-Cardinality)is det.
%
%       Calculate the Cardinality of a Hypothesis Language.
%
%       Elements is a list: [T,C,E,P,K], where:
%       * T is the number of target predicates in the current MIL
%       problem.
%       * C is the number of constants (i.e. the Constants Signature).
%       * E is the number of existentially quantified first- and second-
%       order variables in second-order metarules with K literals.
%       * P is the number of background predicates in the current MIL
%       Problem.
%       * K is the maximum number of literals in third-order metarules.
%
hypothesis_language_cardinality([T,C,E,P,K],N):-
        N is T * C^E * P^(K-1).
