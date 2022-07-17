// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Format from "rescript/lib/es6/format.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Graphml$Graph from "../src/graphml.bs.js";
import * as Imperative$Graph from "../src/imperative.bs.js";

var compare = Caml_obj.caml_compare;

function hash(i) {
  return i;
}

var equal = Caml_obj.caml_equal;

var V = {
  compare: compare,
  hash: hash,
  equal: equal
};

var G = Imperative$Graph.Digraph.ConcreteBidirectional(V);

var vertex_properties = {
  hd: [
    "id1",
    "string",
    undefined
  ],
  tl: {
    hd: [
      "id2",
      "string",
      "2"
    ],
    tl: /* [] */0
  }
};

var edge_properties = {
  hd: [
    "ed",
    "string",
    "3"
  ],
  tl: /* [] */0
};

function map_edge(e) {
  return {
          hd: [
            "ed",
            String(Curry._1(G.E.dst, e))
          ],
          tl: /* [] */0
        };
}

function map_vertex(v) {
  return {
          hd: [
            "id1",
            String(v)
          ],
          tl: {
            hd: [
              "id2",
              String(v)
            ],
            tl: /* [] */0
          }
        };
}

var vertex_uid = G.V.hash;

function edge_uid(e) {
  return Hashtbl.hash([
              Curry._1(vertex_uid, Curry._1(G.E.src, e)),
              Curry._1(G.E.label, e),
              Curry._1(vertex_uid, Curry._1(G.E.dst, e))
            ]);
}

var Gr_V = G.V;

var Gr_E = G.E;

var Gr_is_directed = G.is_directed;

var Gr_is_empty = G.is_empty;

var Gr_nb_vertex = G.nb_vertex;

var Gr_nb_edges = G.nb_edges;

var Gr_out_degree = G.out_degree;

var Gr_in_degree = G.in_degree;

var Gr_mem_vertex = G.mem_vertex;

var Gr_mem_edge = G.mem_edge;

var Gr_mem_edge_e = G.mem_edge_e;

var Gr_find_edge = G.find_edge;

var Gr_find_all_edges = G.find_all_edges;

var Gr_succ = G.succ;

var Gr_pred = G.pred;

var Gr_succ_e = G.succ_e;

var Gr_pred_e = G.pred_e;

var Gr_iter_vertex = G.iter_vertex;

var Gr_fold_vertex = G.fold_vertex;

var Gr_iter_edges = G.iter_edges;

var Gr_fold_edges = G.fold_edges;

var Gr_iter_edges_e = G.iter_edges_e;

var Gr_fold_edges_e = G.fold_edges_e;

var Gr_iter_succ = G.iter_succ;

var Gr_iter_pred = G.iter_pred;

var Gr_fold_succ = G.fold_succ;

var Gr_fold_pred = G.fold_pred;

var Gr_iter_succ_e = G.iter_succ_e;

var Gr_fold_succ_e = G.fold_succ_e;

var Gr_iter_pred_e = G.iter_pred_e;

var Gr_fold_pred_e = G.fold_pred_e;

var Gr_create = G.create;

var Gr_clear = G.clear;

var Gr_copy = G.copy;

var Gr_add_vertex = G.add_vertex;

var Gr_remove_vertex = G.remove_vertex;

var Gr_add_edge = G.add_edge;

var Gr_add_edge_e = G.add_edge_e;

var Gr_remove_edge = G.remove_edge;

var Gr_remove_edge_e = G.remove_edge_e;

var Gr = {
  V: Gr_V,
  E: Gr_E,
  is_directed: Gr_is_directed,
  is_empty: Gr_is_empty,
  nb_vertex: Gr_nb_vertex,
  nb_edges: Gr_nb_edges,
  out_degree: Gr_out_degree,
  in_degree: Gr_in_degree,
  mem_vertex: Gr_mem_vertex,
  mem_edge: Gr_mem_edge,
  mem_edge_e: Gr_mem_edge_e,
  find_edge: Gr_find_edge,
  find_all_edges: Gr_find_all_edges,
  succ: Gr_succ,
  pred: Gr_pred,
  succ_e: Gr_succ_e,
  pred_e: Gr_pred_e,
  iter_vertex: Gr_iter_vertex,
  fold_vertex: Gr_fold_vertex,
  iter_edges: Gr_iter_edges,
  fold_edges: Gr_fold_edges,
  iter_edges_e: Gr_iter_edges_e,
  fold_edges_e: Gr_fold_edges_e,
  iter_succ: Gr_iter_succ,
  iter_pred: Gr_iter_pred,
  fold_succ: Gr_fold_succ,
  fold_pred: Gr_fold_pred,
  iter_succ_e: Gr_iter_succ_e,
  fold_succ_e: Gr_fold_succ_e,
  iter_pred_e: Gr_iter_pred_e,
  fold_pred_e: Gr_fold_pred_e,
  create: Gr_create,
  clear: Gr_clear,
  copy: Gr_copy,
  add_vertex: Gr_add_vertex,
  remove_vertex: Gr_remove_vertex,
  add_edge: Gr_add_edge,
  add_edge_e: Gr_add_edge_e,
  remove_edge: Gr_remove_edge,
  remove_edge_e: Gr_remove_edge_e,
  vertex_properties: vertex_properties,
  edge_properties: edge_properties,
  map_edge: map_edge,
  map_vertex: map_vertex,
  vertex_uid: vertex_uid,
  edge_uid: edge_uid
};

var $$let = G.E;

var partial_arg_E = {
  src: $$let.src,
  dst: $$let.dst
};

var partial_arg_is_directed = G.is_directed;

var partial_arg_iter_vertex = G.iter_vertex;

var partial_arg_iter_edges_e = G.iter_edges_e;

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
      vertex_properties: vertex_properties,
      edge_properties: edge_properties,
      map_vertex: map_vertex,
      map_edge: map_edge,
      vertex_uid: vertex_uid,
      edge_uid: edge_uid
    });

function print(g) {
  return Curry._2(GraphPrinter.print, Format.std_formatter, g);
}

var g = Curry._2(G.create, undefined, undefined);

Curry._2(G.add_vertex, g, 1);

Curry._2(G.add_vertex, g, 2);

Curry._2(G.add_vertex, g, 3);

Curry._3(G.add_edge, g, 1, 2);

Curry._3(G.add_edge, g, 1, 3);

Curry._2(GraphPrinter.print, Format.std_formatter, g);

export {
  V ,
  G ,
  Gr ,
  GraphPrinter ,
  print ,
  
}
/* G Not a pure module */