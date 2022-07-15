// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Set from "rescript/lib/es6/set.js";
import * as Caml from "rescript/lib/es6/caml.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Format from "rescript/lib/es6/format.js";
import * as Classic$RescriptOcamlgraph from "../Classic.bs.js";
import * as Fixpoint$RescriptOcamlgraph from "../Fixpoint.bs.js";
import * as Persistent$RescriptOcamlgraph from "../Persistent.bs.js";

function id(x) {
  return x;
}

var compare = Caml.caml_int_compare;

function equal(prim0, prim1) {
  return prim0 === prim1;
}

var IntOrdered = {
  compare: compare,
  hash: id,
  equal: equal
};

var IntSet = $$Set.Make(IntOrdered);

var G = Persistent$RescriptOcamlgraph.Digraph.Concrete(IntOrdered);

var Divisors = Classic$RescriptOcamlgraph.P(G);

function analyze(param) {
  return id;
}

var Analysis_join = IntSet.union;

var Analysis_equal = IntSet.equal;

var Analysis = {
  direction: /* Backward */1,
  join: Analysis_join,
  equal: Analysis_equal,
  analyze: analyze
};

var $$let = G.E;

var partial_arg_V = G.V;

var partial_arg_E = {
  dst: $$let.dst,
  src: $$let.src
};

var partial_arg_fold_vertex = G.fold_vertex;

var partial_arg_succ_e = G.succ_e;

var partial_arg_pred_e = G.pred_e;

var partial_arg_succ = G.succ;

var partial_arg_pred = G.pred;

var partial_arg = {
  V: partial_arg_V,
  E: partial_arg_E,
  fold_vertex: partial_arg_fold_vertex,
  succ_e: partial_arg_succ_e,
  pred_e: partial_arg_pred_e,
  succ: partial_arg_succ,
  pred: partial_arg_pred
};

var partial_arg$1 = Fixpoint$RescriptOcamlgraph.Make;

var Fixpoint = (function (param) {
      return partial_arg$1(partial_arg, param);
    })(Analysis);

function pp_int_set(pp, set) {
  var first = {
    contents: true
  };
  Format.fprintf(pp, /* Format */{
        _0: {
          TAG: /* Formatting_gen */18,
          _0: {
            TAG: /* Open_box */1,
            _0: /* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "<hov>",
                _1: /* End_of_format */0
              },
              _1: "<hov>"
            }
          },
          _1: /* End_of_format */0
        },
        _1: "@[<hov>"
      });
  Curry._2(IntSet.iter, (function (x) {
          if (first.contents) {
            first.contents = false;
          } else {
            Format.fprintf(pp, /* Format */{
                  _0: {
                    TAG: /* Char_literal */12,
                    _0: /* ',' */44,
                    _1: {
                      TAG: /* Formatting_lit */17,
                      _0: {
                        TAG: /* Break */0,
                        _0: "@ ",
                        _1: 1,
                        _2: 0
                      },
                      _1: /* End_of_format */0
                    }
                  },
                  _1: ",@ "
                });
          }
          return Format.pp_print_int(pp, x);
        }), set);
  return Format.fprintf(pp, /* Format */{
              _0: {
                TAG: /* Formatting_lit */17,
                _0: /* Close_box */0,
                _1: /* End_of_format */0
              },
              _1: "@]"
            });
}

var labels = Curry._2(Fixpoint.analyze, IntSet.singleton, Curry._1(Divisors.divisors, 15));

Format.open_vbox(0);

for(var i = 2; i <= 15; ++i){
  Curry._3(Format.printf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "Labels for ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* String_literal */11,
                  _0: ": ",
                  _1: {
                    TAG: /* Alpha */15,
                    _0: {
                      TAG: /* Formatting_lit */17,
                      _0: {
                        TAG: /* Break */0,
                        _0: "@,",
                        _1: 0,
                        _2: 0
                      },
                      _1: /* End_of_format */0
                    }
                  }
                }
              }
            },
            _1: "Labels for %d: %a@,"
          }), i, pp_int_set, Curry._1(labels, i));
}

Format.close_box(undefined);

Format.print_flush(undefined);

export {
  id ,
  IntOrdered ,
  IntSet ,
  G ,
  Divisors ,
  Analysis ,
  Fixpoint ,
  pp_int_set ,
  
}
/* IntSet Not a pure module */