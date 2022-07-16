// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Persistent$Graph from "../src/persistent.bs.js";
import * as WeakTopological$Graph from "../src/weakTopological.bs.js";
import * as ChaoticIteration$Graph from "../src/chaoticIteration.bs.js";

var compare = Caml_obj.caml_compare;

var Operator_default = {
  TAG: /* Affect */0,
  _0: /* Var */0
};

var Operator = {
  compare: compare,
  $$default: Operator_default
};

function print_num(n) {
  if (typeof n === "number") {
    if (n !== 0) {
      return Pervasives.print_string("+\xe2\x88\x9e");
    } else {
      return Pervasives.print_string("-\xe2\x88\x9e");
    }
  } else {
    return Pervasives.print_int(n._0);
  }
}

function $less$percent(n1, n2) {
  if (typeof n1 === "number") {
    if (n1 !== 0) {
      if (typeof n2 === "number" && n2 !== 0) {
        return Pervasives.failwith("<%");
      } else {
        return false;
      }
    } else if (n2 === 0) {
      return Pervasives.failwith("<%");
    } else {
      return true;
    }
  } else if (typeof n2 === "number") {
    return n2 !== 0;
  } else {
    return n1._0 < n2._0;
  }
}

function $great$eq$percent(n1, n2) {
  return !$less$percent(n1, n2);
}

function $less$eq$percent(n1, n2) {
  if ($less$percent(n1, n2)) {
    return true;
  } else {
    return Caml_obj.caml_equal(n1, n2);
  }
}

function $great$percent(n1, n2) {
  if ($less$percent(n1, n2)) {
    return false;
  } else {
    return Caml_obj.caml_notequal(n1, n2);
  }
}

function min_(n1, n2) {
  if ($less$eq$percent(n1, n2)) {
    return n1;
  } else {
    return n2;
  }
}

function max_(n1, n2) {
  if ($less$percent(n1, n2)) {
    return n2;
  } else {
    return n1;
  }
}

var top = /* Bounded */{
  _0: /* MinusInfinity */0,
  _1: /* PlusInfinity */1
};

var equal = Caml_obj.caml_equal;

function print(param) {
  if (param) {
    Pervasives.print_string("[");
    print_num(param._0);
    Pervasives.print_string("; ");
    print_num(param._1);
    return Pervasives.print_string("]");
  } else {
    return Pervasives.print_string("\xe2\x8a\xa5");
  }
}

function join(i1, i2) {
  if (i1) {
    if (i2) {
      return /* Bounded */{
              _0: min_(i1._0, i2._0),
              _1: max_(i1._1, i2._1)
            };
    } else {
      return i1;
    }
  } else {
    return i2;
  }
}

function singleton(n) {
  return /* Bounded */{
          _0: /* Int */{
            _0: n
          },
          _1: /* Int */{
            _0: n
          }
        };
}

function $plus$percent(x, y) {
  if (typeof x === "number") {
    if (x === 0) {
      if (typeof y === "number" && y !== 0) {
        return Pervasives.failwith("+%");
      } else {
        return /* MinusInfinity */0;
      }
    }
    if (typeof y !== "number") {
      return /* PlusInfinity */1;
    }
    if (y === 0) {
      return Pervasives.failwith("+%");
    }
    
  } else if (typeof y !== "number") {
    return /* Int */{
            _0: x._0 + y._0 | 0
          };
  }
  if (typeof y === "number") {
    if (y === /* MinusInfinity */0) {
      return /* MinusInfinity */0;
    } else {
      return /* PlusInfinity */1;
    }
  }
  
}

function abstr_expr(interval, n) {
  if (typeof n === "number") {
    return interval;
  }
  if (n.TAG === /* Int */0) {
    return singleton(n._0);
  }
  var match = abstr_expr(interval, n._0);
  var match$1 = abstr_expr(interval, n._1);
  if (match && match$1) {
    return /* Bounded */{
            _0: $plus$percent(match._0, match$1._0),
            _1: $plus$percent(match._1, match$1._1)
          };
  } else {
    return /* Bottom */0;
  }
}

function abstr_test(interval, test, c) {
  if (!interval) {
    return /* Bottom */0;
  }
  var b = interval._1;
  var a = interval._0;
  if (test) {
    if ($less$percent(b, c)) {
      return /* Bottom */0;
    } else {
      return /* Bounded */{
              _0: max_(a, c),
              _1: b
            };
    }
  } else if ($great$percent(a, c)) {
    return /* Bottom */0;
  } else {
    return /* Bounded */{
            _0: a,
            _1: min_(b, c)
          };
  }
}

function analyze(param, interval) {
  var op = param[1];
  if (op.TAG === /* Affect */0) {
    return abstr_expr(interval, op._0);
  } else {
    return abstr_test(interval, op._0, /* Int */{
                _0: op._1
              });
  }
}

function widening(i1, i2) {
  if (!i1) {
    return i2;
  }
  var b = i1._1;
  var a = i1._0;
  if (i2) {
    return /* Bounded */{
            _0: $less$eq$percent(a, i2._0) ? a : /* MinusInfinity */0,
            _1: $less$percent(b, i2._1) ? /* PlusInfinity */1 : b
          };
  } else {
    return Pervasives.failwith("widening");
  }
}

var Interval = {
  print_num: print_num,
  $less$percent: $less$percent,
  $great$eq$percent: $great$eq$percent,
  $less$eq$percent: $less$eq$percent,
  $great$percent: $great$percent,
  min_: min_,
  max_: max_,
  top: top,
  equal: equal,
  print: print,
  join: join,
  singleton: singleton,
  $plus$percent: $plus$percent,
  abstr_expr: abstr_expr,
  abstr_test: abstr_test,
  analyze: analyze,
  widening: widening
};

var equal$1 = Caml_obj.caml_equal;

var Int = {
  compare: compare,
  hash: Hashtbl.hash,
  equal: equal$1
};

var Data = {
  print_num: print_num,
  $less$percent: $less$percent,
  $great$eq$percent: $great$eq$percent,
  $less$eq$percent: $less$eq$percent,
  $great$percent: $great$percent,
  min_: min_,
  max_: max_,
  top: top,
  equal: equal,
  print: print,
  join: join,
  singleton: singleton,
  $plus$percent: $plus$percent,
  abstr_expr: abstr_expr,
  abstr_test: abstr_test,
  analyze: analyze,
  widening: widening
};

var partial_arg = Persistent$Graph.Digraph.ConcreteLabeled;

var G = partial_arg(Int, Operator);

var Wto = WeakTopological$Graph.Make({
      V: G.V,
      iter_vertex: G.iter_vertex,
      iter_succ: G.iter_succ
    });

var partial_arg_V = G.V;

var partial_arg_E = {
  src: G.E.src
};

var partial_arg_fold_pred_e = G.fold_pred_e;

var partial_arg$1 = {
  V: partial_arg_V,
  E: partial_arg_E,
  fold_pred_e: partial_arg_fold_pred_e
};

var partial_arg$2 = ChaoticIteration$Graph.Make;

var Chaotic = (function (param) {
      return partial_arg$2(partial_arg$1, param);
    })({
      join: join,
      equal: equal,
      analyze: analyze,
      widening: widening
    });

var edges = {
  hd: [
    1,
    2,
    {
      TAG: /* Affect */0,
      _0: {
        TAG: /* Int */0,
        _0: 0
      }
    }
  ],
  tl: {
    hd: [
      2,
      3,
      {
        TAG: /* Test */1,
        _0: /* Le */0,
        _1: 39
      }
    ],
    tl: {
      hd: [
        3,
        2,
        {
          TAG: /* Affect */0,
          _0: {
            TAG: /* Sum */1,
            _0: /* Var */0,
            _1: {
              TAG: /* Int */0,
              _0: 1
            }
          }
        }
      ],
      tl: {
        hd: [
          2,
          4,
          {
            TAG: /* Test */1,
            _0: /* Ge */1,
            _1: 40
          }
        ],
        tl: /* [] */0
      }
    }
  }
};

var g = List.fold_left((function (acc, param) {
        return Curry._2(G.add_edge_e, acc, Curry._3(G.E.create, param[0], param[2], param[1]));
      }), G.empty, edges);

var strategy = Curry._2(Wto.recursive_scc, g, 1);

function print_vertex_data(vertex, interval) {
  Pervasives.print_int(vertex);
  Pervasives.print_string(" -> ");
  print(interval);
  return Pervasives.print_newline(undefined);
}

function init(v) {
  if (v === 1) {
    return top;
  } else {
    return /* Bottom */0;
  }
}

var widening_set2 = /* Predicate */{
  _0: (function (param) {
      return 3 === param;
    })
};

var result1 = Curry._5(Chaotic.recurse, g, strategy, init, /* FromWto */0, 40);

var result2 = Curry._5(Chaotic.recurse, g, strategy, init, widening_set2, 39);

var result3 = Curry._5(Chaotic.recurse, g, strategy, init, /* FromWto */0, 41);

console.log("W = WTO, delay=40:");

Curry._2(Chaotic.M.iter, print_vertex_data, result1);

Pervasives.print_newline(undefined);

console.log("W = {3}, delay=39:");

Curry._2(Chaotic.M.iter, print_vertex_data, result2);

Pervasives.print_newline(undefined);

console.log("W = WTO, delay=41:");

Curry._2(Chaotic.M.iter, print_vertex_data, result3);

Pervasives.print_newline(undefined);

var widening_delay1 = 40;

var widening_set1 = /* FromWto */0;

var widening_delay2 = 39;

var widening_delay3 = 41;

var widening_set3 = /* FromWto */0;

export {
  Operator ,
  Interval ,
  Int ,
  Data ,
  G ,
  Wto ,
  Chaotic ,
  edges ,
  g ,
  strategy ,
  print_vertex_data ,
  init ,
  widening_delay1 ,
  widening_set1 ,
  widening_delay2 ,
  widening_set2 ,
  widening_delay3 ,
  widening_set3 ,
  result1 ,
  result2 ,
  result3 ,
  
}
/* G Not a pure module */
