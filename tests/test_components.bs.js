// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Format from "rescript/lib/es6/format.js";
import * as Random from "rescript/lib/es6/random.js";
import * as Pack$Graph from "../src/pack.bs.js";
import * as Components$Graph from "../src/components.bs.js";

var C = Components$Graph.Undirected({
      V: Pack$Graph.Graph.V,
      iter_vertex: Pack$Graph.Graph.iter_vertex,
      iter_edges: Pack$Graph.Graph.iter_edges
    });

Random.init(42);

var g = Curry._4(Pack$Graph.Graph.Rand.graph, undefined, 10, 3, undefined);

var match = Curry._1(C.components, g);

var f = match[1];

Curry._1(Format.printf(/* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_d */0,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: {
              TAG: /* String_literal */11,
              _0: " components",
              _1: {
                TAG: /* Formatting_lit */17,
                _0: /* Flush_newline */4,
                _1: /* End_of_format */0
              }
            }
          },
          _1: "%d components@."
        }), match[0]);

Curry._2(Pack$Graph.Graph.iter_vertex, (function (v) {
        return Curry._2(Format.printf(/* Format */{
                        _0: {
                          TAG: /* Int */4,
                          _0: /* Int_d */0,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            TAG: /* String_literal */11,
                            _0: " -> ",
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
                          }
                        },
                        _1: "%d -> %d@."
                      }), Curry._1(Pack$Graph.Graph.V.label, v), Curry._1(f, v));
      }), g);

export {
  C ,
  
}
/* C Not a pure module */