// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Format from "rescript/lib/es6/format.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Graphviz$RescriptOcamlgraph from "../src/graphviz.bs.js";
import * as Traverse$RescriptOcamlgraph from "../src/traverse.bs.js";
import * as Persistent$RescriptOcamlgraph from "../src/persistent.bs.js";

var compare = Caml_obj.caml_compare;

var equal = Caml_obj.caml_equal;

var partial_arg = {
  compare: compare,
  hash: Hashtbl.hash,
  equal: equal
};

var partial_arg$1 = Persistent$RescriptOcamlgraph.Digraph.ConcreteBidirectionalLabeled;

var G = (function (param) {
      return partial_arg$1(partial_arg, param);
    })({
      compare: compare,
      $$default: ""
    });

Traverse$RescriptOcamlgraph.Dfs({
      is_directed: G.is_directed,
      V: G.V,
      iter_vertex: G.iter_vertex,
      fold_vertex: G.fold_vertex,
      iter_succ: G.iter_succ,
      fold_succ: G.fold_succ
    });

var g = Belt_List.reduce({
      hd: [
        "u",
        {
          hd: "v",
          tl: {
            hd: "x",
            tl: /* [] */0
          }
        }
      ],
      tl: {
        hd: [
          "v",
          {
            hd: "y",
            tl: /* [] */0
          }
        ],
        tl: {
          hd: [
            "w",
            {
              hd: "z",
              tl: {
                hd: "y",
                tl: /* [] */0
              }
            }
          ],
          tl: {
            hd: [
              "x",
              {
                hd: "v",
                tl: /* [] */0
              }
            ],
            tl: {
              hd: [
                "y",
                {
                  hd: "x",
                  tl: /* [] */0
                }
              ],
              tl: {
                hd: [
                  "z",
                  {
                    hd: "z",
                    tl: /* [] */0
                  }
                ],
                tl: /* [] */0
              }
            }
          }
        }
      }
    }, G.empty, (function (g, param) {
        var v = param[0];
        return Belt_List.reduce(param[1], g, (function (g, y) {
                      return Curry._3(G.add_edge, g, v, y);
                    }));
      }));

var Dfs = Traverse$RescriptOcamlgraph.Dfs({
      is_directed: G.is_directed,
      V: G.V,
      iter_vertex: G.iter_vertex,
      fold_vertex: G.fold_vertex,
      iter_succ: G.iter_succ,
      fold_succ: G.fold_succ
    });

console.log(Curry._1(Dfs.has_cycle, g));

function pre(v) {
  console.log(" pre " + Curry._1(G.V.label, v) + ".");
  
}

function post(v) {
  console.log("post " + Curry._1(G.V.label, v) + ".");
  
}

console.log("iter: ");

Curry._4(Dfs.iter_component, pre, post, g, "w");

console.log("prefix: ");

Curry._3(Dfs.prefix_component, pre, g, "w");

var E = G.E;

function vertex_name(v) {
  return Curry._1(G.V.label, v);
}

function graph_attributes(param) {
  return /* [] */0;
}

function default_vertex_attributes(param) {
  return /* [] */0;
}

function vertex_attributes(param) {
  return /* [] */0;
}

function default_edge_attributes(param) {
  return /* [] */0;
}

function edge_attributes(param) {
  return /* [] */0;
}

function get_subgraph(param) {
  
}

var Gv = Graphviz$RescriptOcamlgraph.Dot({
      V: {},
      E: {
        src: E.src,
        dst: E.dst
      },
      iter_vertex: G.iter_vertex,
      iter_edges_e: G.iter_edges_e,
      graph_attributes: graph_attributes,
      default_vertex_attributes: default_vertex_attributes,
      vertex_name: vertex_name,
      vertex_attributes: vertex_attributes,
      get_subgraph: get_subgraph,
      default_edge_attributes: default_edge_attributes,
      edge_attributes: edge_attributes
    });

Curry._2(Gv.fprint_graph, Format.str_formatter, g);

var s = Format.flush_str_formatter(undefined);

console.log("s=");

console.log(s);

export {
  
}
/* G Not a pure module */
