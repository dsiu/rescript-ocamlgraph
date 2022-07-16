// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Strat$Graph from "../src/strat.bs.js";
import * as Persistent$Graph from "../src/persistent.bs.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

var compare = Caml_obj.caml_compare;

var equal = Caml_obj.caml_equal;

var V = {
  compare: compare,
  hash: Hashtbl.hash,
  equal: equal
};

var G = Persistent$Graph.Digraph.Concrete(V);

function get_initial(param) {
  return param[0];
}

function is_final(param, v) {
  return List.mem(v, param[1]);
}

function turn(param, v) {
  return Curry._1(param[2], v);
}

var P = {
  get_initial: get_initial,
  is_final: is_final,
  turn: turn
};

function empty(param) {
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Strategy definition",
        Error: new Error()
      };
}

function next(f, v) {
  return Curry._1(f, v);
}

function add(s, v, v$p, e) {
  if (Caml_obj.caml_equal(e, v)) {
    return v$p;
  } else {
    return Curry._1(s, e);
  }
}

var S = {
  empty: empty,
  next: next,
  add: add
};

var partial_arg_V = G.V;

var partial_arg_mem_vertex = G.mem_vertex;

var partial_arg_succ = G.succ;

var partial_arg_fold_vertex = G.fold_vertex;

var partial_arg_fold_succ = G.fold_succ;

var partial_arg = {
  V: partial_arg_V,
  mem_vertex: partial_arg_mem_vertex,
  succ: partial_arg_succ,
  fold_vertex: partial_arg_fold_vertex,
  fold_succ: partial_arg_fold_succ
};

var partial_arg$1 = Strat$Graph.Algo;

function partial_arg$2(param, param$1) {
  return partial_arg$1(partial_arg, param, param$1);
}

var A = partial_arg$2(P, {
      empty: empty,
      add: add,
      next: next
    });

function trans_aux(_g, _param) {
  while(true) {
    var param = _param;
    var g = _g;
    var n = param[1];
    if (n === 0) {
      return g;
    }
    var j = param[0];
    if (n === 1) {
      var g$1 = Curry._3(G.add_edge, g, [
            j,
            n
          ], [
            !j,
            n - 1 | 0
          ]);
      if (j) {
        _param = [
          !j,
          n
        ];
        _g = g$1;
        continue ;
      }
      _param = [
        !j,
        n - 1 | 0
      ];
      _g = g$1;
      continue ;
    }
    if (n === 2) {
      var g$2 = Curry._3(G.add_edge, g, [
            j,
            n
          ], [
            !j,
            n - 1 | 0
          ]);
      var g$3 = Curry._3(G.add_edge, g$2, [
            j,
            n
          ], [
            !j,
            n - 2 | 0
          ]);
      if (j) {
        _param = [
          !j,
          n
        ];
        _g = g$3;
        continue ;
      }
      _param = [
        !j,
        n - 1 | 0
      ];
      _g = g$3;
      continue ;
    }
    var g$4 = Curry._3(G.add_edge, g, [
          j,
          n
        ], [
          !j,
          n - 1 | 0
        ]);
    var g$5 = Curry._3(G.add_edge, g$4, [
          j,
          n
        ], [
          !j,
          n - 2 | 0
        ]);
    var g$6 = Curry._3(G.add_edge, g$5, [
          j,
          n
        ], [
          !j,
          n - 3 | 0
        ]);
    if (j) {
      _param = [
        !j,
        n
      ];
      _g = g$6;
      continue ;
    }
    _param = [
      !j,
      n - 1 | 0
    ];
    _g = g$6;
    continue ;
  };
}

function trans(n) {
  return trans_aux(G.empty, [
              true,
              n
            ]);
}

function p(n) {
  return [
          [
            true,
            n
          ],
          {
            hd: [
              true,
              0
            ],
            tl: /* [] */0
          },
          (function (param) {
              return param[0];
            })
        ];
}

function ex(n) {
  var t = trans(n);
  return [
          Curry._2(A.strategyA, t, p(n)),
          t
        ];
}

var ex1 = ex(15);

var ex2 = ex(13);

function couple_of_strat(g, s) {
  var f = function (v, l) {
    try {
      var v$p = Curry._1(s, v);
      return {
              hd: [
                v,
                v$p
              ],
              tl: l
            };
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Invalid_argument") {
        return l;
      }
      throw exn;
    }
  };
  return Curry._3(G.fold_vertex, f, g, /* [] */0);
}

function string_of_couple(param) {
  return "(" + (String(param[0][1]) + (", " + (String(param[1][1]) + ") ")));
}

function string_of_couple_list(l) {
  if (l) {
    return string_of_couple(l.hd) + string_of_couple_list(l.tl);
  } else {
    return "";
  }
}

Pervasives.print_newline(undefined);

Pervasives.print_string("For " + (String(15) + " matches, is there a winning strategy for the first player ?"));

Pervasives.print_newline(undefined);

Pervasives.print_string(Pervasives.string_of_bool(ex1[0][0]));

Pervasives.print_string(" --- ");

Pervasives.print_string(string_of_couple_list(couple_of_strat(ex1[1], ex1[0][1])));

Pervasives.print_newline(undefined);

Pervasives.print_newline(undefined);

Pervasives.print_string("For " + (String(13) + " matches, is there a winning strategy for the first player ?"));

Pervasives.print_newline(undefined);

Pervasives.print_string(Pervasives.string_of_bool(ex2[0][0]));

Pervasives.print_newline(undefined);

Pervasives.print_newline(undefined);

var n1 = 15;

var n2 = 13;

export {
  V ,
  G ,
  P ,
  S ,
  A ,
  trans_aux ,
  trans ,
  p ,
  ex ,
  n1 ,
  n2 ,
  ex1 ,
  ex2 ,
  couple_of_strat ,
  string_of_couple ,
  string_of_couple_list ,
  
}
/* G Not a pure module */