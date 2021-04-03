:-module(log_parser, [copy_r_plotting_script/2
                     ,write_r_data/2
                     ,parse_logs_to_r_vectors/2
                     ]).

:- use_module(library(dcg/basics)).

/** <module> Parse experiment logs into R vectors for plotting.

*/

r_plotting_script('plot_experiment_results.r').


%!      copy_r_plotting_script(+Input_Dir,-Output_Dir) is det.
%
%       Copy the R plotting script to the Output directory.
%
copy_r_plotting_script(In_Dir,Out_Dir):-
        r_plotting_script(Fn)
        ,directory_file_path(In_Dir,Fn,P)
        ,copy_file(P,Out_Dir).


%!      write_r_data(+Directory) is det.
%
%       Write an R file holding expeiment data for plotting.
%
write_r_data(In_Dir, Out_Dir):-
        parse_logs_to_r_vectors(In_Dir,Vs)
        ,directory_file_path(Out_Dir,'experiment_data.r',P)
        ,O = open(P,write,S,[alias(experiment_data_file)
                        ,close_on_abort(true)
                        ])
        ,G = maplist(writeln(S),Vs)
        ,C = close(S)
        ,setup_call_cleanup(O,G,C).



%!      parse_logs_to_r_vectors(+Directory,-Vectors) is det.
%
%       Parse log files in a Directory to R Vectors for plotting
%
parse_logs_to_r_vectors(Dir,Vs_s):-
        directory_files(Dir,Fs)
        ,log_files(Dir,Fs,Ls)
        ,maplist(parse_log_to_r_vectors,Ls,Vs_)
        ,flatten(Vs_,Vs_f)
        ,sort(Vs_f,Vs_s).


%!      log_files(+Directory,+Files,-Logs) is det.
%
%       Collect paths of Log files in a Directory.
%
log_files(D,Fs,Ls):-
        findall(P
               ,(member(Fn,Fs)
                ,file_name_extension(Bn,log,Fn)
                ,atomic_list_concat([Bn,log],'.',L)
                ,directory_file_path(D,L,P)
                )
               ,Ls).


%!      parse_log_to_r_vectors(+File,-R_Vectors) is det.
%
%       Parse a log file to R Vectors for plotting.
%
parse_log_to_r_vectors(F,Rs):-
        read_lines(F,Ls)
        ,parse_lines(Ls,Ps)
        ,parsed_lines_to_r_vectors(Ps, Rs).


%!      read_lines(+File, -Lines) is det.
%
%       Read lines from a File until the end_of_file marker.
%
read_lines(F,Ls):-
        O = open(F,read,S,[alias(input_file)
                      ,close_on_abort(true)
                      ])
        ,R = read_lines(S,[],Ls)
        ,C = close(S)
        ,setup_call_cleanup(O,R,C).

%!      read_lines(+Stream,+Acc,-Lines) is det.
%
%       Business end of read_lines/2.
%
read_lines(S,Acc,Bind):-
        read_line_to_codes(S,Cs)
        ,is_list(Cs)
        ,!
        % For developing only
        ,atom_codes(A,Cs)
        ,read_lines(S,[A|Acc],Bind).
read_lines(S,Acc,Ls):-
        read_line_to_codes(S,end_of_file)
        ,reverse(Acc,Ls).


%!      parse_lines(+Lines, -Parsed) is det.
%
%       Parse Lines from a file to log info terms.
%
parse_lines(Ls, Ps):-
        parse_lines(Ls, [], Ps).

%!      parse_lines(+Lines, +Acc, -Parsed) is det.
%
%       Business end of parse_lines/2.
%
parse_lines([], Acc, Ls):-
        reverse(Acc, Ls)
        ,!.
parse_lines([L|Ls], Acc, Bind):-
        atom_codes(L, Cs)
        ,phrase(log_info(S), Cs)
        ,!
        ,parse_lines(Ls,[S|Acc],Bind).
parse_lines([_L|Ls], Acc, Bind):-
        parse_lines(Ls, Acc, Bind).


%!      log_info(?Info) is nondet.
%
%       Structure of interesting log information.
%
log_info(target(F/A)) --> target(Cs/A), { atom_codes(F,Cs) }.
log_info(metric(M)) --> metric(Cs), {atom_codes(M, Cs)}.
log_info(steps(N)) --> steps(N).
log_info(samples(N)) --> samples(N).
log_info(time_limit(L)) --> time_limit(L).
log_info(higher_order(Min,Max)) --> higher_order(punch,[Min,Max]).
log_info(higher_order(Hs)) --> higher_order(matrix,Cs), { atoms_codes(Hs, Cs) } .
log_info(means(Ms)) --> means(Ms).
log_info(sds(Ms)) --> sds(Ms).
log_info(pos(N)) --> positive_examples(N).
log_info(neg(N)) --> negative_examples(N).

target(T) --> info_line(`Target:`), predicate_symbol(T).
metric(M) --> info_line(`Metric:`), string(M).
steps(Ns) --> info_line(`Steps:`), integers(Ns).
samples(Ss) --> info_line(`Samples:`), listed(int,Ss), !.
samples(Ss) --> info_line(`Samples:`), listed(float,Ss).
time_limit(L) --> info_line(`Time limit (sec):`), integers(L).
higher_order(punch,Hs) -->
        info_line(`Higher order metarules:`)
        ,integers(_)
        ,blanks
        ,higher_order_limits(Hs)
        ,!.
higher_order(matrix,Hs) -->
        info_line(`Higher order metarules:`)
        ,integers(_)
        ,blanks
        ,listed(term,Hs).
means(Ms) --> info_line(`Mean`), string(_M), `:`, blanks, listed(nums,Ms).
sds(Ms) --> info_line(`Standard deviations:`), blanks, listed(nums,Ms).
positive_examples(N) --> info_line(`Positive examples:`), blanks, integers(N).
negative_examples(N) --> info_line(`Negative examples:`), blanks, integers(N).

info_line(S) --> start_of_line, string(S), blanks.

start_of_line --> comment, blank.
start_of_line --> comment.

comment --> `%`.

predicate_symbol(F/A) --> string(F), `/`, integer(A).

integers(N) --> integer(N).
integers([N|Ns]) --> integer(N), integers(Ns).

floats(N) --> float(N).
floats([N|Ns]) --> float(N), floats(Ns).

nums(N) --> number(N).
nums([N|Ns]) --> number(N), nums(Ns).

listed(_, []) --> `[]`.
listed(term,[S|As]) --> `[`, string(S), args(As), `]`, !.
listed(term,Ts) --> `[`, comma_string(Ts), `]`.
listed(int, Ns) --> `[`, integers(Ns), `]`.
listed(float, Ns) --> `[`, floats(Ns), `]`.
listed(floats, Ns) --> `[`, comma_floats(Ns), `]`.
listed(num, Ns) --> `[`, nums(Ns), `]`.
listed(nums, Ns) --> `[`, comma_nums(Ns), `]`.

comma_string([S|Ss]) --> string(S), comma, !, comma_string(Ss).
comma_string([S|Ss]) --> comma, !, string(S), comma_string(Ss).
comma_string([S]) --> string(S).

comma_ints([N|Ns]) --> integer(N), comma, !, comma_ints(Ns).
comma_ints([N|Ns]) --> comma, !, integer(N), comma_ints(Ns).
comma_ints([N])  --> integer(N).

comma_floats([N|Ns]) --> float(N), comma, !, comma_floats(Ns).
comma_floats([N|Ns]) --> comma, !, float(N), comma_floats(Ns).
comma_floats([N])  --> float(N).

comma_nums([N|Ns]) --> number(N), comma, !, comma_nums(Ns).
comma_nums([N|Ns]) --> comma, !, number(N), comma_nums(Ns).
comma_nums([N])  --> number(N).


comma --> `,`.

args(As) --> `(`, comma_string(As), `)`.

higher_order_limits([Min,Max]) --> `[`,`higher_order`,`(`,comma_ints([Min,Max]),`)]`.



%!      atoms_codes(?Atoms,?Codes) is det.
%
%       Maps atom_codes/2 to lists of Atoms and Codes.
%
atoms_codes(As, Cs):-
        maplist(atom_codes,As,Cs).



%!      numbers_codes(?Numbers,?Codes) is det.
%
%       Mapts number_codes/2 to lists of Numbers and Codes.
%
numbers_codes(Ns, Cs):-
        maplist(number_codes,Ns,Cs).



%!      parsed_lines_to_r_vectors(+Lines,-R_Data) is det.
%
%       Convert parsed log Lines to R data for plotting.
%
parsed_lines_to_r_vectors(Ls,Vs):-
        r_string(target,Ls,_,T_v)
        ,r_num(pos,Ls,_,Pos_v)
        ,r_num(neg,Ls,_,Neg_v)
        ,r_string(metric,Ls,_,M_v)
        ,r_num(steps,Ls,_,S_v)
        ,r_num(samples,Ls,_,Ss_v)
        ,r_num(time_limit,Ls,_,L_v)
        ,experiment_type(Ls,_,T,Et_v)
        ,r_vector(means,Ls,T,_,Ms_v)
        ,r_vector(sds,Ls,T,_,Sds_v)
        ,flatten([T_v,Pos_v,Neg_v,M_v,S_v,Ss_v,L_v,Ms_v,Sds_v,Et_v],Vs).


%!      r_string(+Datum,+Parsed,-Rest,-String) is det.
%
%       Convert a log file info datum to an R String for plotting.
%
r_string(F,Ls,Ls_,A):-
        functor(D,F,1)
        ,selectchk(D,Ls,Ls_)
        ,D =.. [_|[D_]]
        ,format(atom(A),'~w <- \'~w\'',[F,D_]).


%!      r_num(+Datum,+Parsed,-Rest,-Number) is det.
%
%       Convert a log file info datum to an R Number for plotting.
%
r_num(F,Ls,Ls_,A):-
        functor(D,F,1)
        ,selectchk(D,Ls,Ls_)
        ,D =.. [_|[D_]]
        ,r_ify_symbol(F,F_)
        ,format(atom(A),'~w <- ~w',[F_,D_]).


%!      r_vector(+Datum,+Parsed,-Rest,-Vector) is det.
%
%       Convert a log file info datum to an R Vector for plotting.
%
r_vector(F,Ls,T,Ls_,A):-
        functor(D,F,1)
        ,selectchk(D,Ls,Ls_)
        ,D =.. [F|[Vs]]
        ,V =.. [c|Vs]
        ,format(atom(A),'~w.~w <- ~w',[T,F,V]).


%!      experiment_type(+Datum,+Parsed,-Rest,-HO,-String) is det.
%
%       Convert a log file info datum to an R String for plotting.
%
%       HO is the type of higher-order metarules in the experiment, one
%       of: [none,punch,matrix]. HO is used as a prefix for r-vectors
%       holding the values of means and standard deviations of results
%       of the experiment for this type of higher-order metarule.
%
experiment_type(Ls,Ls_,sort,T):-
        selectchk(higher_order([]),Ls,Ls_)
        ,!
        ,format(atom(T),'higher.order <- \'none\'',[]).
experiment_type(Ls,Ls_,punch,T):-
        selectchk(higher_order(Min,Max),Ls,Ls_)
        ,maplist(number,[Min,Max])
        ,!
        ,format(atom(T),'higher.order <- c(min(~w),max(~w))',[Min,Max]).
experiment_type(Ls,Ls_,matrix,T):-
        selectchk(higher_order([H|Ts]),Ls,Ls_)
        ,!
        ,findall(Id_a
                ,(member(Id,[H|Ts])
                 ,format(atom(Id_a), '\'~w\'', [Id])
                 )
                ,IDs)
        %,V =.. [c|[H|Ts]]
        ,V =.. [c|IDs]
        ,format(atom(T),'higher.order <- ~w',[V]).


%!      r_ify_symbol(+Prolog,-R) is det.
%
%       Convert between Prolog and R naming convention.
%
%       Prolog is an atomic name in Prolog's notational convention for
%       atoms, possibly replacing spaces with the underscore. R is the
%       same atomic name but with the underscores replaced with '.', as
%       per R convention.
%
r_ify_symbol(S,S_):-
        atomic_list_concat(Ss,'_',S)
        ,!
        ,atomic_list_concat(Ss,'.',S_).
r_ify_symbol(S,S).
