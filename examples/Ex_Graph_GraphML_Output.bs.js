// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Format from "rescript/lib/es6/format.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Graphml$Graph from "../src/graphml.bs.js";
import * as Ex_Graph_Samples$Graph from "./Ex_Graph_Samples.bs.js";

function map_edge(e) {
  return {
          hd: [
            "ed",
            Curry._1(Ex_Graph_Samples$Graph.G.E.dst, e)
          ],
          tl: /* [] */0
        };
}

function map_vertex(v) {
  return {
          hd: [
            "id1",
            v
          ],
          tl: /* [] */0
        };
}

var vertex_uid = Ex_Graph_Samples$Graph.G.V.hash;

function edge_uid(e) {
  return Hashtbl.hash([
              Curry._1(vertex_uid, Curry._1(Ex_Graph_Samples$Graph.G.E.src, e)),
              Curry._1(Ex_Graph_Samples$Graph.G.E.label, e),
              Curry._1(vertex_uid, Curry._1(Ex_Graph_Samples$Graph.G.E.dst, e))
            ]);
}

var $$let = Ex_Graph_Samples$Graph.G.E;

var partial_arg_E = {
  src: $$let.src,
  dst: $$let.dst
};

var partial_arg_is_directed = Ex_Graph_Samples$Graph.G.is_directed;

var partial_arg_iter_vertex = Ex_Graph_Samples$Graph.G.iter_vertex;

var partial_arg_iter_edges_e = Ex_Graph_Samples$Graph.G.iter_edges_e;

var partial_arg = {
  E: partial_arg_E,
  is_directed: partial_arg_is_directed,
  iter_vertex: partial_arg_iter_vertex,
  iter_edges_e: partial_arg_iter_edges_e
};

var partial_arg$1 = Graphml$Graph.Print;

var GraphPrinter = (function (param) {
      return partial_arg$1(partial_arg, param);
    })({
      vertex_properties: {
        hd: [
          "id1",
          "string",
          undefined
        ],
        tl: /* [] */0
      },
      edge_properties: {
        hd: [
          "ed",
          "string",
          "3"
        ],
        tl: /* [] */0
      },
      map_vertex: map_vertex,
      map_edge: map_edge,
      vertex_uid: vertex_uid,
      edge_uid: edge_uid
    });

Curry._2(GraphPrinter.print, Format.std_formatter, Ex_Graph_Samples$Graph.sample1);

export {
  
}
/* GraphPrinter Not a pure module */
