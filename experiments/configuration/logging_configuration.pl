% Print coloured output to terminal if possible.
:- set_prolog_flag(color_term, true).

/* Debug levels
 * Note that some of the debug topics below emit identical messages.
 * In particular, 'learn' debugs learn/5 that calls top program
 * construction and reduction that are also debugged by 'top_program'
 * and 'reduction'.
*/
:-nodebug(_). % Clear all debug topics.
%:-debug(learn). % Debug learning steps.
%:-debug(metasubstitution). % Debug metasubstitutions.
%:-debug(top_program). % Debug Top program construction.
%:-debug(reduction). % Debug Top program reduction.
%:-debug(dynamic). % Debug dynamic learning.
%:-debug(predicate_invention). % Debug predicate invention.
%:-debug(learn_metarules). % Debug metarule learning
%:-debug(learned_metarules). % Debug new metarules
%:-debug(metarule_grounding). % Debug metarule template specialisation
%:-debug(examples_invention). % Debug examples invention.
%:-debug(remove_null). % Debug examples invention.
%:-debug(evaluation).
% Learning curve logging
%:-debug(progress).
%:-debug(learning_curve_full).
%:-debug(learning_curve_setup).
%:-debug(learning_curve).


% Colorise Swi debug messages to make them more readable in
% dark-coloured terminals (default colouring is too dark).
%user:message_property(debug(learn), color( [ fg(green) ]) ).
%user:message_property(debug(learned_metarules), color( [ fg(green) ]) ).
%user:message_property(debug(evaluation), color( [ fg(green) ]) ).
%user:message_property(debug(learn), color( [ fg(green) ]) ).

user:message_property(debug(learning_curve), color( [ fg(cyan) ]) ).
user:message_property(debug(learning_curve_setup), color( [ fg(cyan) ]) ).
user:message_property(debug(run_learning_curve_setup), color( [ fg(cyan) ]) ).
user:message_property(debug(progress), color( [ fg(bright_yellow) ]) ).

