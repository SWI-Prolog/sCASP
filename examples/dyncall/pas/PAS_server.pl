:- use_module(library(scasp)).
:- use_module(library(scasp/html)).
:- use_module(library(scasp/output)).

:- use_module('PAS_rules').
:- use_module('PAS_guide').
:- use_module('PAS_patient').

:- use_module(library(http/http_server)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/jquery)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/term_html)).

%!  server(+Port)
%
%   Start HTTP server at the indicated port.

server(Port) :-
    http_server([port(Port)]).

:- http_handler(root(.),     home,  []).
:- http_handler(root(solve), solve, [id(solve)]).

home(_Request) :-
    reply_html_page([ title('s(CASP) PAS demo')
                    ],
                    [ h1('s(CASP) PAS demo'),
                      \patient_form(1)
                    ]).

case(1,
     [ case_measurement(age, 76),
       case_evidence(african_american),
       case_evidence(male),

       %* Assessments *%
       case_evidence(nyha_class_4),
       case_evidence(accf_stage_c),

       %* Contraindications *%
       case_contraindication(continuous_positive_airway_pressure),

       %* Diagnoses *%
       case_diagnosis(ischemic_heart_disease),
       case_diagnosis(hypertension),
       case_diagnosis(diabetes),
       case_diagnosis(atrial_fibrillation),

       %* Dosages *%

       %* Evidence *%
       case_evidence(sleepApnea),
       case_evidence(angina),

       %* Illness History *%
       case_history(stroke),
       case_history(ischemic_attack),

       %* Measurements *%
       case_measurement(lvef, 0.35),
       case_measurement(heart_rate, 72),
       case_measurement(creatinine, 1.9),
       case_measurement(glomerular_filtration_rate, 55),
       case_measurement(potassium, 4.2),

       %* Medication History *%
       case_history(ace_inhibitors),
       case_history(beta_blockers)
     ]).


patient_form(Case) -->
    html_requires(jquery),
    html_requires(scasp),
    { case(Case, Data),
      length(Data, Length),
      Rows is max(20,Length+1),
      with_output_to(string(String),
                     forall(member(D, Data),
                            format('~q.~n', [D])))
    },
    html([ h4('Patient data'),
           textarea([id(data), rows(Rows), cols(60)],
                    String),
           h4('Query'),
           '?- ', input([id(query),
                         value('chose(ace_inhibitors)')]),
           h4(''),
           button(id(solve), 'Solve'),
           button(id(clear), 'Clear'),
           div(id(results), [])
         ]),
    button_actions.

button_actions -->
    { http_link_to_id(solve, [], SolveURL) },
    js_script({|javascript(SolveURL)||
$(function() {

$("#solve").on("click", function() {
  var data = $("#data").val();
  var query = $("#query").val();
  $.get(SolveURL,
        { data: data,
          query: query
        },
        function(reply) {
          var results = $("#results");

          results.html(reply);
          results.sCASP('swish_answer');
        });
});

$("#clear").on("click", function() {
  $("#results").empty();
});

});
              |}).


solve(Request) :-
    http_parameters(Request,
                    [ data(Data, []),
                      query(QueryS, [])
                    ]),
    setup_call_cleanup(
        open_string(Data, In),
        read_terms(In, Terms),
        close(In)),
    patient_data(Terms),
    term_string(Query, QueryS, [variable_names(VNames)]),
    call_time(findall(result(N, Time, VNames, Model, Justification),
                      call_nth(call_time(scasp(Query, Model, Justification), Time), N),
                      Results),
              TotalTime),
    reply_html_page([],
                    \results(Results, TotalTime)).

scasp(Query, Model, Justification) :-
    scasp(Query),
    scasp_model(Model),
    scasp_justification(Justification, []).

results([], Time) -->
    !,
    html(h3('No models (~3f sec)'-[Time.cpu])).
results(Results, _Time) -->
    sequence(result, Results).

result(result(N, Time, Bindings, Model, Justification)) -->
    { ovar_analyze_term(t(Bindings,Model,Justification))
    },
    html(div(class(result),
             [ h3('Result #~D (~3f sec)'-[N, Time.cpu]),
               \binding_section(Bindings),
               \html_model(Model, [class('collapsable-content')]),
               \html_justification_tree(Justification, [])
             ])).

read_terms(In, Terms) :-
    read_term(In, Term0, []),
    read_terms(Term0, In, Terms).

read_terms(end_of_file, _, []) :-
    !.
read_terms(Term, In, [Term|T]) :-
    read_term(In, Term1, []),
    read_terms(Term1, In, T).

%!  binding_section(+Bindings)//

binding_section([]) -->
    !.
binding_section(Bindings) -->
    html(div(class(bindings),
             [ h4('Bindings'),
               \bindings(Bindings)
             ])).

%!  bindings(+Bindings)//
%
%   Report on the bindings.

bindings([]) -->
    [].
bindings([H|T]) -->
    binding(H),
    bindings(T).

binding(Name=Value) -->
    html(div(class(binding),
             [ var(Name),
               ' = ',
               \term(Value, [])
             ])).
