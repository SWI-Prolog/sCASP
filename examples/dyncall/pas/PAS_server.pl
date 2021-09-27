:- use_module(library(scasp)).
:- use_module(library(scasp/html)).

:- use_module('PAS_rules').
:- use_module('PAS_guide').
:- use_module('PAS_patient').

:- use_module(library(http/http_server)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/jquery)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_dispatch)).

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
    { case(Case, Data),
      length(Data, Length),
      Rows is max(20,Length+1),
      with_output_to(string(String),
                     forall(member(D, Data),
                            format('~q.~n', [D])))
    },
    html([ h2('Patient data'),

           textarea([id(data), rows(Rows), cols(60)],
                    String),
           br([]),
           '?- ', textarea([id(query), rows(1), cols(57)],
                           'chose(ace_inhibitors)'),
           br([]),
           button(id(solve), 'Solve'),
           button(id(clear), 'Clear'),
           div(id(model), []),
           div(id(justification), [])
         ]),
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
            $("#justification").html(reply.justification);
        });
});

$("#clear").on("click", function() {
  $("#justification").empty();
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
    term_string(Query, QueryS),
    scasp(Query),
    scasp_justification(Tree, []),
    html_string(\html_justification_tree(Tree, []), Justification),
    reply_json(_{justification: Justification}).

read_terms(In, Terms) :-
    read_term(In, Term0, []),
    read_terms(Term0, In, Terms).

read_terms(end_of_file, _, []) :-
    !.
read_terms(Term, In, [Term|T]) :-
    read_term(In, Term1, []),
    read_terms(Term1, In, T).

:- html_meta
    html_string(html, -).

html_string(HTML, String) :-
    phrase(html(HTML), Tokens),
    with_output_to(string(String), print_html(Tokens)).
