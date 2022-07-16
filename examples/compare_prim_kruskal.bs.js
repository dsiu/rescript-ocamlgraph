// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Arg from "rescript/lib/es6/arg.js";
import * as Caml from "rescript/lib/es6/caml.js";
import * as $$Array from "rescript/lib/es6/array.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Format from "rescript/lib/es6/format.js";
import * as Printf from "rescript/lib/es6/printf.js";
import * as Random from "rescript/lib/es6/random.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Prim$Graph from "../src/prim.bs.js";
import * as Rand$Graph from "../src/rand.bs.js";
import * as Kruskal$Graph from "../src/kruskal.bs.js";
import * as Imperative$Graph from "../src/imperative.bs.js";

var v_ = {
  contents: 30
};

var e_ = {
  contents: 50
};

var seed_ = {
  contents: undefined
};

var debug_ = {
  contents: false
};

var arg_spec_0 = [
  "-v",
  {
    TAG: /* Int */6,
    _0: (function (i) {
        v_.contents = i;
        
      })
  },
  " <int>  number of vertices"
];

var arg_spec_1 = {
  hd: [
    "-e",
    {
      TAG: /* Int */6,
      _0: (function (i) {
          e_.contents = i;
          
        })
    },
    " <int>  number of edges"
  ],
  tl: {
    hd: [
      "-seed",
      {
        TAG: /* Int */6,
        _0: (function (n) {
            seed_.contents = n;
            
          })
      },
      " <int>  random seed"
    ],
    tl: {
      hd: [
        "--debug",
        {
          TAG: /* Set */2,
          _0: debug_
        },
        "set the debug flag"
      ],
      tl: /* [] */0
    }
  }
};

var arg_spec = {
  hd: arg_spec_0,
  tl: arg_spec_1
};

Arg.parse(arg_spec, (function (param) {
        
      }), "usage: color <options>");

var v = v_.contents;

var e = e_.contents;

var debug = debug_.contents;

var s = seed_.contents;

var seed = s !== undefined ? s : (Random.self_init(undefined), Random.$$int(536870912));

Curry._1(Format.printf(/* Format */{
          _0: {
            TAG: /* String_literal */11,
            _0: "seed = ",
            _1: {
              TAG: /* Int */4,
              _0: /* Int_d */0,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* Formatting_lit */17,
                _0: /* Flush_newline */4,
                _1: /* End_of_format */0
              }
            }
          },
          _1: "seed = %d@."
        }), seed);

Random.init(seed);

var compare = Caml_obj.caml_compare;

var equal = Caml_obj.caml_equal;

var Int = {
  compare: compare,
  hash: Hashtbl.hash,
  equal: equal,
  $$default: 0
};

var partial_arg = {};

var partial_arg$1 = Imperative$Graph.Graph.AbstractLabeled;

var G = (function (param) {
      return partial_arg$1(partial_arg, param);
    })({
      compare: compare,
      $$default: 0
    });

var R = Rand$Graph.I(G);

function weight(x) {
  return Curry._1(G.E.label, x);
}

function add(prim0, prim1) {
  return prim0 + prim1 | 0;
}

var compare$1 = Caml_obj.caml_compare;

var W = {
  weight: weight,
  zero: 0,
  add: add,
  compare: compare$1
};

function utime(f, x) {
  var u = Date.now();
  var y = Curry._1(f, x);
  var ut = Date.now() - u;
  return [
          y,
          ut
        ];
}

function time5(f, x) {
  var t = $$Array.init(5, (function (param) {
          return utime(f, x)[1];
        }));
  if (debug) {
    $$Array.iter((function (x) {
            return Curry._1(Printf.printf(/* Format */{
                            _0: {
                              TAG: /* Float */8,
                              _0: /* Float_f */0,
                              _1: {
                                TAG: /* Lit_padding */0,
                                _0: /* Right */1,
                                _1: 2
                              },
                              _2: /* Lit_precision */{
                                _0: 2
                              },
                              _3: {
                                TAG: /* Char_literal */12,
                                _0: /* '\n' */10,
                                _1: /* End_of_format */0
                              }
                            },
                            _1: "%2.2f\n"
                          }), x);
          }), t);
  }
  $$Array.sort(Caml.caml_float_compare, t);
  return (Caml_array.get(t, 1) + Caml_array.get(t, 2) + Caml_array.get(t, 3)) / 3;
}

function print(f, x) {
  var match = utime(f, x);
  Curry._1(Printf.printf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "user time: ",
              _1: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: {
                  TAG: /* Lit_padding */0,
                  _0: /* Right */1,
                  _1: 2
                },
                _2: /* Lit_precision */{
                  _0: 2
                },
                _3: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Flush_newline */4,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "user time: %2.2f@."
          }), match[1]);
  return match[0];
}

var Time = {
  utime: utime,
  time5: time5,
  print: print
};

var $$let = G.E;

var partial_arg_V = G.V;

var partial_arg_E = {
  label: $$let.label,
  dst: $$let.dst,
  src: $$let.src
};

var partial_arg_fold_vertex = G.fold_vertex;

var partial_arg_iter_edges_e = G.iter_edges_e;

var partial_arg$2 = {
  V: partial_arg_V,
  E: partial_arg_E,
  fold_vertex: partial_arg_fold_vertex,
  iter_edges_e: partial_arg_iter_edges_e
};

var partial_arg$3 = Kruskal$Graph.Make;

var P1 = (function (param) {
      return partial_arg$3(partial_arg$2, param);
    })({
      compare: compare$1
    });

var $$let$1 = G.E;

var partial_arg_V$1 = G.V;

var partial_arg_E$1 = {
  label: $$let$1.label,
  dst: $$let$1.dst,
  src: $$let$1.src,
  compare: $$let$1.compare
};

var partial_arg_iter_vertex = G.iter_vertex;

var partial_arg_iter_edges_e$1 = G.iter_edges_e;

var partial_arg_iter_succ_e = G.iter_succ_e;

var partial_arg$4 = {
  V: partial_arg_V$1,
  E: partial_arg_E$1,
  iter_vertex: partial_arg_iter_vertex,
  iter_edges_e: partial_arg_iter_edges_e$1,
  iter_succ_e: partial_arg_iter_succ_e
};

var partial_arg$5 = Prim$Graph.Make;

var P2 = (function (param) {
      return partial_arg$5(partial_arg$4, param);
    })({
      weight: weight,
      compare: compare$1,
      add: add,
      zero: 0
    });

function testp(g) {
  return time5(P1.spanningtree, g);
}

function testk(g) {
  return time5(P2.spanningtree, g);
}

function test(nb_v, nb_e) {
  Curry._2(Printf.printf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "Execution time v=",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* String_literal */11,
                  _0: " - e=",
                  _1: {
                    TAG: /* Int */4,
                    _0: /* Int_d */0,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Char_literal */12,
                      _0: /* '\n' */10,
                      _1: /* End_of_format */0
                    }
                  }
                }
              }
            },
            _1: "Execution time v=%d - e=%d\n"
          }), nb_v, nb_e);
  var g = Curry._4(R.graph, undefined, nb_v, nb_e, undefined);
  var resp = time5(P1.spanningtree, g);
  Curry._1(Printf.printf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "PRIM : ",
              _1: {
                TAG: /* Float */8,
                _0: /* Float_f */0,
                _1: {
                  TAG: /* Lit_padding */0,
                  _0: /* Right */1,
                  _1: 2
                },
                _2: /* Lit_precision */{
                  _0: 2
                },
                _3: {
                  TAG: /* String_literal */11,
                  _0: "s\n",
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "PRIM : %2.2fs\n"
          }), resp);
  return Curry._1(Printf.printf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "KRUSKAL : ",
                    _1: {
                      TAG: /* Float */8,
                      _0: /* Float_f */0,
                      _1: {
                        TAG: /* Lit_padding */0,
                        _0: /* Right */1,
                        _1: 2
                      },
                      _2: /* Lit_precision */{
                        _0: 2
                      },
                      _3: {
                        TAG: /* String_literal */11,
                        _0: "s\n",
                        _1: {
                          TAG: /* Flush */10,
                          _0: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: "KRUSKAL : %2.2fs\n%!"
                }), time5(P2.spanningtree, g));
}

test(v, e);

var Stdlib;

export {
  Stdlib ,
  v_ ,
  e_ ,
  seed_ ,
  debug_ ,
  arg_spec ,
  v ,
  e ,
  debug ,
  seed ,
  Int ,
  G ,
  R ,
  W ,
  Time ,
  P1 ,
  P2 ,
  testp ,
  testk ,
  test ,
  
}
/*  Not a pure module */