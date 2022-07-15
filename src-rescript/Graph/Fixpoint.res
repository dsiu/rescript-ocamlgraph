/* ************************************************************************ */
/*  */
/* Ocamlgraph: a generic graph library for OCaml */
/* Copyright (C) 2004-2010 */
/* Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles */
/*  */
/* This software is free software; you can redistribute it and/or */
/* modify it under the terms of the GNU Library General Public */
/* License version 2.1, with the special exception on linking */
/* described in file LICENSE. */
/*  */
/* This software is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. */
/*  */
/* ************************************************************************ */

/* Copyright (c) 2010 - 2012 Technische Universitaet Muenchen
 * Markus W. Weissmann <markus.weissmann@in.tum.de>
 * All rights reserved. */

/* maximum fixpoint point calculation with the work list algorithm;
   to implement a concrete analysis, implement a module that satisfies
   the Rules signature. Such a module in the Analysis functor gives a
   complete analysis/optimization module that works on a CFG.
*/

type direction = Forward | Backward

module type Analysis = {
  type data
  type edge
  type vertex
  type g

  let direction: direction
  let join: (data, data) => data
  let equal: (data, data) => bool
  let analyze: (edge, data) => data
}

@ocaml.doc(" Minimal graph signature for work list algorithm ")
module type G = {
  type t
  module V: Sig.COMPARABLE
  module E: {
    type t
    let dst: t => V.t
    let src: t => V.t
  }
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let succ_e: (t, V.t) => list<E.t>
  let pred_e: (t, V.t) => list<E.t>
  let succ: (t, V.t) => list<V.t>
  let pred: (t, V.t) => list<V.t>
}

module Make = (
  G: G,
  A: ((Analysis with type g = G.t) with type edge = G.E.t) with type vertex = G.V.t,
) => {
  module M = Map.Make(G.V)
  module N = Set.Make(G.V)

  let analyze = (initial, g) => {
    let (nodes, data) = G.fold_vertex(
      (vertex, (n, m)) => (N.add(vertex, n), M.add(vertex, initial(vertex), m)),
      g,
      (N.empty, M.empty),
    )

    /* generate an associative map to quickly find the incoming
     * (outgoing) edges of a node during the anaysis store a pair of
     * a partially applied analysis function and the corresponding
     * 'partner' node */
    let nodemap: M.t<list<(A.data => A.data, G.V.t)>> = {
      let add = switch A.direction {
      | Forward =>
        n => {
          let preds = G.pred_e(g, n)
          List.map(edge => (A.analyze(edge), G.E.src(edge)), preds)
        }
      | Backward =>
        n => {
          let succs = G.succ_e(g, n)
          List.map(edge => (A.analyze(edge), G.E.dst(edge)), succs)
        }
      }

      G.fold_vertex((vertex, m) => M.add(vertex, add(vertex), m), g, M.empty)
    }

    let rec worklist = (data: M.t<A.data>, wl: N.t) => {
      /* 'meet' an arbitrary number of data-sets */
      let meet = (initial, xs) => List.fold_left(A.join, initial, xs)

      /* analyze one node, creating a new data-set and node-worklist
       as necessary */
      let analyze_node = (analysis, n, d, wl) =>
        switch analysis(d, n) {
        | None => (d, wl)
        | Some(d') => (d', N.add(n, wl))
        }

      /* get some node from the node-set -- this will eventually trigger
       an exception */
      switch try Some(N.choose(wl)) catch {
      | Not_found => None
      } {
      | None => data
      | Some(n) =>
        /* remove the chosen node from the set */
        let wl = N.remove(n, wl)

        let (f, ns) = switch A.direction {
        /* analyze all INCOMING edges of all SUCCESSOR nodes of the
         node to be processed */
        | Forward =>
          /* process one node: analyze all it's incoming edges
               and merge the resulting data;
               if the result is different to the previously stored data
               for this node, return a new tuple, else None */
          let new_node_data = (data: M.t<A.data>, node) => {
            let edges = M.find(node, nodemap)
            let analysis = List.map(((f, src)) => f(M.find(src, data)), edges)

            let node_data = M.find(node, data)
            let node_data' = meet(initial(node), analysis)
            if A.equal(node_data, node_data') {
              None
            } else {
              Some(M.add(node, node_data', data))
            }
          }

          (new_node_data, G.succ(g, n))
        /* analyze all OUTGOING edges of all PREDECESSOR nodes
         of the node to be processed */
        | Backward =>
          let new_node_data = (data: M.t<A.data>, node) => {
            let edges = M.find(node, nodemap)
            let analysis = List.map(((f, dst)) => f(M.find(dst, data)), edges)

            let node_data = M.find(node, data)
            let node_data' = meet(initial(node), analysis)
            if A.equal(node_data, node_data') {
              None
            } else {
              Some(M.add(node, node_data', data))
            }
          }

          (new_node_data, G.pred(g, n))
        }

        /* analyze all successor nodes by analyzing all of their
         predecessor edges */
        let (data, wl) = List.fold_left(((d, wl), n) => analyze_node(f, n, d, wl), (data, wl), ns)

        /* do a recursive call: the recursion will eventually end with a
         * Not_found exception when no nodes are left in the work list */
        worklist(data, wl)
      }
    }

    let data = worklist(data, nodes)
    n => M.find(n, data)
  }
}
