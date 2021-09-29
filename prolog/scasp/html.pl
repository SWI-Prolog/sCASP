:- module(scasp_just_html,
          [ html_justification_tree//2,		% +Tree, +Options
            html_model//2,			% +Model, +Options
            tree_resources//0,
            tree_buttons//0
          ]).
:- use_module(common).
:- use_module(clp/disequality).
:- use_module(clp/clpq).
:- use_module(output).

:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/term_html)).
:- use_module(library(dcg/high_order)).

:- meta_predicate
    html_justification_tree(:, +, ?, ?).

/** <module> Render s(CASP) justification as HTML
*/

%!  html_justification_tree(:Tree, +Options)// is det.
%
%   Convert the tree to HTML. The  caller should use ovar_analyze_term/1
%   on Tree to name variables and identify  singletons. This is not done
%   in this predicate as the user may or  may not wish to combin the the
%   variable analysis with the bindings and/or model.
%
%   @see print_message/2.

:- det(html_justification_tree//2).

html_justification_tree(M:Tree, Options) -->
    html(ul(class(tree),
            \justification_tree(Tree,
                                [ depth(0),
                                  module(M)
                                | Options
                                ]))).

%!  justification_tree(+FilterChildren, +Options)//

justification_tree(query-[Query,o_nmr_check-[]], Options) -->
    !,
    justification_tree(Query, Options),
    full_stop(Options).
justification_tree(query-Children, Options) -->
    !,
    justification_tree_children(Children, Options),
    full_stop(Options).
justification_tree(o_nmr_check-[], _Options) -->
    !.
justification_tree(Term-[], Options) -->
    !,
    html(li([ \tree_atom(Term, Options),
              \connect(Options)
            ])).
justification_tree(Term-Children, Options) -->
    { incr_indent(Options, Options1) },
    html(li([ \tree_atom(Term, Options),
              \connector(implies, Options),
              ul(\justification_tree_children(Children, Options1))
            ])).

justification_tree_children([A,B|Rs], Options) -->
    justification_tree(A, [connect(and)|Options]),
    justification_tree_children([B|Rs], Options).
justification_tree_children([A], Options) -->
    justification_tree(A, Options).

connect(Options) -->
    { option(connect(Connector), Options) },
    !,
    connector(Connector, Options).
connect(_) -->
    [].

tree_atom(Atom, Options) -->
    { atom_classes(Atom, Classes),
      scasp_atom_string(Atom, String)
    },
    html(span([ class(['scasp-atom'|Classes]),
                title(String)
              ],
              \atom(Atom, Options))).

scasp_atom_string(Atom, String) :-
    with_output_to(string(String),
                   print_model_term(Atom, [])).

%!  html_model(+Model, +Options)// is det.
%
%   Emit the model as HTML terms.   We export the model as a dict with
%   nested model terms.

html_model(Model, Options) -->
    html(div(class('scasp-model'),
             \sequence(model_term_r(Options), Model))).

model_term_r(Options, Atom) -->
    { atom_classes(Atom, Classes),
      scasp_atom_string(Atom, String)
    },
    html(div([ class(['scasp-atom'|Classes]),
               title(String)
             ],
             \atom(Atom, Options))).


atom_classes(A, []) :-
    var(A),
    !.
atom_classes(not(A), ['scasp-not'|T]) :-
    !,
    atom_classes(A, T).
atom_classes(-(A), ['scasp-negation'|T]) :-
    !,
    atom_classes(A, T).
atom_classes(_, []).

%!  atom(+SCASPAtom, +Options)//
%
%   Emit an s(CASP) atom with annotations as   they  appear in the model
%   and justification.

atom(not(GlobalConstraint), Options) -->
    { is_global_constraint(GlobalConstraint, N)
    },
    !,
    utter(global_constraint(N), Options).
atom(not(Term), Options) -->
    !,
    utter(not(Term), Options).
atom(-Term, Options) -->
    !,
    utter(-(Term), Options).
atom(proved(Term), Options) -->
    !,
    utter(proved(Term), Options).
atom(chs(Term), Options) -->
    !,
    utter(chs(Term), Options).
atom(M:Term, Options) -->
    { atom(M) },
    !,
    atom(Term, [module(M)|Options]).
atom(Term, Options) -->            % #pred Term::Template
    { option(module(M), Options),       % Used existing translation
      human_expression(M:Term, Actions)
    },
    !,
    actions(Actions, Options).
atom(o_nmr_check, Options) -->
    !,
    utter(global_constraints_hold, Options).
atom(Term, Options) -->
    utter(holds(Term), Options).

%!  utter(+Exppression, +Options)

utter(global_constraints_hold, _Options) -->
    html('The global constraints hold').
utter(global_constraint(N), _Options) -->
    html('the global constraint number ~p holds'-[N]).
utter(not(Atom), Options) -->
    html('there is no evidence that '),
    atom(Atom, Options).
utter(-(Atom), Options) -->
    html('it is not the case that '),
    atom(Atom, Options).
utter(proved(Atom), Options) -->
    atom(Atom, Options),
    html(', justified above').
utter(chs(Atom), Options) -->
    html('it is assumed that '),
    atom(Atom, Options).
utter(holds(Atom), Options) -->
    (   { atom(Atom) }
    ->  html([span(class(atom), Atom), ' holds'])
    ;   { Atom =.. [Name|Args] }
    ->  html([span(class(atom), Name), ' holds for ']),
        list(Args, Options)
    ).

:- det(scasp_term//2).

scasp_term(Var, Options) -->
    { var(Var) },
    !,
    var(Var, Options).
scasp_term(@(Var:''), Options) -->
    { var(Var)
    },
    !,
    var(Var, Options).
scasp_term(@(Var:Type), Options) -->
    { var(Var)
    },
    !,
    var(Var, Type, Options).
scasp_term(@(Value:''), Options) -->
    !,
    scasp_term(Value, Options).
scasp_term(@(Value:Type), Options) -->
    html('the ~w '-[Type]),
    !,
    scasp_term(Value, Options).
scasp_term(Term, _Options) -->
    { var_number(Term, _) },
    !,
    [ '~p'-[Term] ].
scasp_term(Term, Options) -->
    term(Term, Options).

var(NegVar, Options) -->
    { get_neg_var(NegVar, List),
      ovar_is_singleton(NegVar)
    },
    !,
    (   {List = [One]}
    ->  html('anything except for '),
        scasp_term(One, Options)
    ;   html('anything except for '),
        list(List, [last_connector(or)|Options])
    ).
var(NegVar, Options) -->
    { get_neg_var(NegVar, List),
      ovar_var_name(NegVar, Name)
    },
    !,
    (   {List = [One]}
    ->  html([var(Name), ' other than ']),
        scasp_term(One, Options)
    ;   html([var(Name), ' not ']),
        list(List, [last_connector(or)|Options])
    ).
var(Var, Options) -->
    { is_clpq_var(Var),
      !,
      clpqr_dump_constraints([Var], [Var], Constraints)
    },
    clpq(Var, Constraints, Options).
var(Var, _Options) -->
    { ovar_var_name(Var, Name)
    },
    !,
    html(var(Name)).
var(_, _) -->
    html(anything).

%!  clpq(@Var, +Constraints, +Options)//

clpq(Var, [Constraint], Options) -->
    { compound(Constraint),
      Constraint =.. [Op,A,B],
      Var == A,
      cmp_op(Op, Text),
      (   ovar_var_name(Var, Name)
      ->  Id = var(Name)
      ;   Id = number
      )
    },
    html(['any ', Id, ' ', Text, ' ']),
    scasp_term(B, Options).

cmp_op(.>.,  'larger than').
cmp_op(.>=., 'larger than or equal to').
cmp_op(.<.,  'smaller than').
cmp_op(.=<., 'smaller than or equal to').
cmp_op(.=.,  'equal to').
cmp_op(.<>., 'not equal to').


%!  var(@Var, +Type, +Options)//

var(NegVar, Type, Options) -->
    { get_neg_var(NegVar, List),
      ovar_is_singleton(NegVar)
    },
    !,
    (   {List = [One]}
    ->  html(['any ', Type, ' except for ']),
        scasp_term(One, Options)
    ;   html(['any ', Type, ' except for ']),
        list(List, [last_connector(or)|Options])
    ).
var(NegVar, Type, Options) -->
    { get_neg_var(NegVar, List),
      ovar_var_name(NegVar, Name)
    },
    !,
    (   {List = [One]}
    ->  html([var(Name), ', a ', Type, ' other than ']),
        scasp_term(One, Options)
    ;   html([var(Name), ', a ', Type, ' not ']),
        list(List, [last_connector(or)|Options])
    ).
var(Var, _Type, Options) -->            % TBD: include type in NLP
    { is_clpq_var(Var),
      !,
      clpqr_dump_constraints([Var], [Var], Constraints)
    },
    clpq(Var, Constraints, Options).
var(Var, Type, _Options) -->
    { ovar_var_name(Var, Name)
    },
    !,
    html([var(Name), ', a ', Type]).
var(_, Type, _) -->
    html(['a ', Type]).

%!  list(+Elements) is det.
%
%   Emit a collection as "a, b, and c"

list([L1,L], Options) -->
    !,
    { option(last_connector(Conn), Options, 'and') },
    scasp_term(L1, Options),
    html(', ~w '-[Conn]),
    scasp_term(L, Options).
list([H|T], Options) -->
    scasp_term(H, Options),
    (   {T==[]}
    ->  []
    ;   html(', '),
        list(T, Options)
    ).

actions([], _) --> [].
actions([H|T], Options) -->
    action(H, Options),
    actions(T, Options).

action(text(S), _) -->
    html(S).
action(Term, Options) -->
    scasp_term(Term, Options).

%!  connector(+Meaning, +Options)//
%
%   Emit a logical connector.

connector(and, _Options) -->
    html(', and').
connector(not, _Options) -->
    html('there is no evidence that ').
connector(-, _Options) -->
    html('-').
connector(implies, _Options) -->
    html(', because').

full_stop(_Options) -->
    html('\u220e').

incr_indent(Options0, [depth(D)|Options2]) :-
    select_option(depth(D0), Options0, Options1),
    select_option(connect(_), Options1, Options2, _),
    D is D0+1.


		 /*******************************
		 *           RESOURCES		*
		 *******************************/

%!  tree_buttons//

tree_buttons -->
    html({|html||
<button class="btn-expand">Expand All</button>
<button class="btn-depth-incr">+1</button>
<button class="btn-depth-decr">-1</button>
<button class="btn-collapse">Collapse All</button>
|}).

%!  tree_resources//
%
%   Emit the JavaScript and HTML style resources for the collapsable
%   justification tree.

tree_resources -->
    tree_style,
    tree_script.

tree_style -->
    html({|html||
 <style>
.toggler { cursor: pointer; }
.toggler:before { display: inline-block; margin-right: 2pt; }

.treemenu li { list-style: none; }
li.tree-empty > .toggler { opacity: 0.3; cursor: default; }
li.tree-empty > .toggler:before { content: " "; }
li.tree-closed > .toggler:before {
    content: "";
    height: 0;
    width: 0;
    border-color: transparent blue;
    border-style: solid;
    border-width: 0.35em 0 0.35em 0.5em;
}
li.tree-opened > .toggler:before {
    content: "";
    height: 0;
    width: 0;
    border-color: blue transparent ;
    border-style: solid;
    border-width: 0.5em 0.35em 0 0.35em;
}

/* Open/close the model */

div.model-closed > .scasp-model {display:none;}

div.model-closed > h4 > .toggler:before {
    content: "";
    height: 0;
    width: 0;
    border-color: transparent blue;
    border-style: solid;
    border-width: 0.35em 0 0.35em 0.5em;
}
div.model-opened > h4 > .toggler:before {
    content: "";
    height: 0;
    width: 0;
    border-color: blue transparent ;
    border-style: solid;
    border-width: 0.5em 0.35em 0 0.35em;
}
</style>
         |}).

tree_script -->
    js_script({|javascript||
    (function($){

	function addButtons(tree) {
	    var just = tree.closest(".justification");

            function findTree(ev) {
              return $(ev.target).closest(".justification").find("> .tree");
            }

            just.find("button.btn-expand").on("click", function(ev) {
              var tree = findTree(ev);
              if ( tree.length > 0 )
                tree.expand({delay:0},0);
            });
            just.find("button.btn-depth-incr").on("click", function(ev) {
              var tree = findTree(ev);
              if ( tree.length > 0 )
                tree.depth({delay:500, step:1});
            });
            just.find("button.btn-depth-decr").on("click", function(ev) {
              var tree = findTree(ev);
              if ( tree.length > 0 )
                tree.depth({delay:500,step:-1});
            });
            just.find("button.btn-collapse").on("click", function(ev) {
              var tree = findTree(ev);
              if ( tree.length > 0 )
                tree.collapse({delay:0});
            });
	}

        $.fn.depth = function(options,depth) {
            options = options || {};
            options.delay = options.delay || 0;

            if ( options.step ) {
               var g_depth = this.data("g_depth") + options.step;
               var max_depth = this.data("max_depth");
               g_depth = g_depth < 0 ? 0 : g_depth;
               g_depth = g_depth > max_depth ? max_depth : g_depth;
               this.data("g_depth", g_depth);
               depth = g_depth;
               delete options.step;
            }

            this.find("> li").each(function() {
                e = $(this)
                var subtree = e.find('> ul');

                if (subtree.length > 0) {
                    if (depth > 0) {
                        e.addClass('tree-opened');
                        e.removeClass('tree-closed');

                        subtree.slideDown(options.delay);
                        subtree.show(options.delay);

                        subtree.depth(options,depth-1);
                    } else {
                        e.removeClass('tree-opened');
                        e.addClass('tree-closed');

                        subtree.slideUp(options.delay);
                        subtree.hide(options.delay);

                        subtree.collapse(options);
                    }
                }
            });
            return true;
        }

        /* Expand the tree and set g_depth to the depth of the deepest branch */

        $.fn.expand = function(options,depth) {
            options = options || {};
            options.delay = options.delay || 0;

	    this.each(function() {
                var root = false;
                var tree = $(this);

                if ( depth == 0 ) {
                    options.g_depth = 0;
                    root = true;
                }

                tree.find("> li").each(function() {
                    e = $(this)
                    var subtree = e.find('> ul');

                    if (subtree.length > 0) {
                        e.addClass('tree-opened');
                        e.removeClass('tree-closed');

                        subtree.slideDown(options.delay);
                        subtree.show(options.delay);

                        subtree.expand(options,depth+1);
                    }
                    options.g_depth=Math.max(options.g_depth,depth);
                });

                if ( root ) {
                    tree.data("g_depth", options.g_depth);
                    tree.data("max_depth", options.g_depth);
                }
            });

            return this;
        }

        $.fn.collapse = function(options) {
            options = options || {};
            options.delay = options.delay || 0;

            this.find("> li").each(function() {
                e = $(this)
                var subtree = e.find('> ul');

                if (subtree.length > 0) {
                    e.removeClass('tree-opened');
                    e.addClass('tree-closed');

                    subtree.slideUp(options.delay);
                    subtree.hide(options.delay);

                    subtree.collapse(options);
                }
            });

            this.data("g_depth", 0);

            return true;
        }

        $.fn.treemenu = function(options) {
            options = options || {};
            options.delay = options.delay || 0;
            options.openActive = options.openActive || false;
            options.closeOther = options.closeOther || false;
            options.activeSelector = options.activeSelector || ".active";

            this.addClass("treemenu");
            this.data("g_depth", 0);

            if (!options.nonroot) {
                this.addClass("treemenu-root");

                addButtons(this);

                this.on("click", ".toggler", function(ev) {
                    var li = $(ev.target).parent('li');

                    if (options.closeOther && li.hasClass('tree-closed')) {
                        var siblings = li.parent('ul').find("li:not(.tree-empty)");
                        siblings.removeClass("tree-opened");
                        siblings.addClass("tree-closed");
                        siblings.removeClass(options.activeSelector);
                        siblings.find('> ul').slideUp(options.delay);
                    }

                    li.find('> ul').slideToggle(options.delay);
                    li.toggleClass('tree-opened');
                    li.toggleClass('tree-closed');
                    li.toggleClass(options.activeSelector);
                });
            }

            options.nonroot = true;

            this.find("> li").each(function() {
                e = $(this);
                var subtree = e.find('> ul');
                var button = e.find('.toggler').eq(0);

                if(button.length == 0) {
                    // create toggler
                    var button = $('<span>');
                    button.addClass('toggler');
                    e.prepend(button);
                }

                if(subtree.length > 0) {
                    subtree.hide();

                    e.addClass('tree-closed');

                    $(this).find('> ul').treemenu(options);
                } else {
                    $(this).addClass('tree-empty');
                }
            });

            if (options.openActive) {
                var cls = this.attr("class");

                this.find(options.activeSelector).each(function(){
                    var el = $(this).parent();

                    while (el.attr("class") !== cls) {
                        el.find('> ul').show();
                        if(el.prop("tagName") === 'UL') {
                            el.show();
                        } else if (el.prop("tagName") === 'LI') {
                            el.removeClass('tree-closed');
                            el.addClass("tree-opened");
                            el.show();
                        }

                        el = el.parent();
                    }
                });
            }

            this.expand({delay:0},0);
            this.collapse({delay:0});

            return this;
        }

        $.fn.modelmenu = function(options) {
	    this.addClass("model-closed");

            this.each(function() {
                e = $(this);
                var button = e.find('.toggler').eq(0);

                if(button.length == 0) {
                    // create toggler
                    var button = $('<span>');
                    button.addClass('toggler');
                    e.find("h4").eq(0).prepend(button);
                }

                e.find(button).click(function(ev) {
                    var div = $(ev.target).closest("div.model");
                    div.toggleClass('model-opened');
                    div.toggleClass('model-closed');
                });
            });
        }
    })(jQuery);

/*
$(function(){
    $(".tree").treemenu({delay:0});
});
*/

|}).
