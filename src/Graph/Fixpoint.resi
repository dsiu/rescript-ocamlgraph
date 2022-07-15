@@ocaml.text(/* ************************************************************************ */
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

" Fixpoint computation implemented using the work list algorithm.
    This module makes writing data-flow analysis easy.

    One of the simplest fixpoint analysis is that of reachability.
    Given a directed graph module [G], its analysis can be implemented
    as follows:

    {[
      module Reachability = Graph.Fixpoint.Make(G)
          (struct
            type vertex = G.E.vertex
            type edge = G.E.t
            type g = G.t
            type data = bool
            let direction = Graph.Fixpoint.Forward
            let equal = (=)
            let join = (||)
            let analyze _ = (fun x -> x)
          end)
    ]}

    The types for [vertex], [edge] and [g] are those of the graph to be
    analyzed.  The [data] type is [bool]: It will tell if the
    vertex is reachable from the start vertex.  The [equal] operation
    for [bool] is simply structural equality; the [join] operation is
    logical or.  The [analyze] function is very simple, too: If the
    predecessor vertex is reachable, so is the successor vertex of the
    edge.

    To use the analysis, an instance of a graph [g] is required.  For
    this analysis a predicate [is_root_vertex : G.E.vertex -> bool] is
    required to initialize the reachability of the root vertex to
    [true] and of all other vertices to [false].

    {[
      let g = ...
        let result = Reachability.analyze is_root_vertex g
    ]}

    The [result] is a map of type [G.E.vertex -> bool] that can be
    queried for every vertex to tell if the vertex is reachable from
    the root vertex.

    @author Markus W. Weissmann
    @see \"Introduction to Lattices and Order\" B. A. Davey and H. A. Priestley, Cambridge University Press, 2002
    @see \"Fixed Point Theory\" Andrzej Granas and James Dugundji, Springer, 2003
    @see \"Principles of Program Analysis\" Flemming Nielson, Hanne Riis Nielson and Chris Hankin, Springer, 2005
    @see \"Ubersetzerbau 3: Analyse und Transformation\" Reinhard Wilhelm and Helmut Seidl, Springer, 2010
")

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

type direction = Forward | @ocaml.doc(" Type of an analysis ") Backward

module type Analysis = {
  @ocaml.doc(" information stored at each vertex ")
  type data

  @ocaml.doc(" type of edges of the underlying graph ")
  type edge

  @ocaml.doc(" type of vertices of the underlying graph ")
  type vertex

  @ocaml.doc(" type of the underlying graph ")
  type g

  @ocaml.doc(" the direction of the analysis ")
  let direction: direction

  @ocaml.doc(" operation how to join data when paths meet ")
  let join: (data, data) => data

  @ocaml.doc(" predicate to determine the fixpoint ")
  let equal: (data, data) => bool

  @ocaml.doc(" the actual analysis of one edge; provided the edge and the incoming
      data, it needs to compute the outgoing data ")
  let analyze: (edge, data) => data
}

module Make: (
  G: G,
  A: ((Analysis with type g = G.t) with type edge = G.E.t) with type vertex = G.V.t,
) =>
{
  @ocaml.doc(" [analyze f g] computes the fixpoint on the given graph using the
      work list algorithm. Beware that a misconstructed Analysis will
      not terminate! [f] is used to create the initial analysis
      data. The function returned is a map to see what data was computed
      for which node.

      Beware of applying function [analyze] partially, to arguments
      [f] and [g] only. The result is a function that is to be used to query
      the result of the analysis. ")
  let analyze: (G.V.t => A.data, A.g, G.V.t) => A.data
}
