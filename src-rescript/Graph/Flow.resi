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

" Algorithms on flows

    The following flow algorithms only apply to networks, that are
    directed graphs together with a source (a 0 in-degree vertex) and a
    terminal (a 0 out-degree vertex). ")

@@ocaml.text(" {1 Maximum flow algorithms} ")

@ocaml.doc(" Signature for edges' flow. ")
module type FLOW = {
  @ocaml.doc(" Type of edges. ")
  type t

  @ocaml.doc(" Type of labels on edges. ")
  type label

  @@ocaml.text(" Maximum and minimum capacities for a label on an edge. ")

  let max_capacity: label => t

  @@ocaml.text(" Current flow for a label on an edge. ")

  let flow: label => t

  @@ocaml.text(" [+] and [-] on flows. ")

  let add: (t, t) => t
  let sub: (t, t) => t

  @@ocaml.text(" Neutral element for [add] and [sub]. ")

  let zero: t

  @@ocaml.text(" A total ordering over flows. ")

  let compare: (t, t) => int
}

@@ocaml.text("  {2 Goldberg-Tarjan maximal flow algorithm} ")

@ocaml.doc(" Minimal graph signature for Goldberg-Tarjan.
    Sub-signature of {!Sig.G}. ")
module type G_GOLDBERG_TARJAN = {
  type t
  module V: Sig.COMPARABLE
  module E: Sig.EDGE with type vertex = V.t
  let nb_vertex: t => int
  let nb_edges: t => int
  let fold_edges_e: ((E.t, 'a) => 'a, t, 'a) => 'a
  let fold_succ_e: ((E.t, 'a) => 'a, t, V.t, 'a) => 'a
  let fold_pred_e: ((E.t, 'a) => 'a, t, V.t, 'a) => 'a
}

module Goldberg_Tarjan: (G: G_GOLDBERG_TARJAN, F: FLOW with type label = G.E.label) =>
{
  @ocaml.doc(" [maxflow g v1 v2] searchs the maximal flow from source [v1] to
      terminal [v2] using Goldberg-Tarjan algorithm (with gap detection
      heuristic).
      It returns the flow on each edge and the growth of the flow. ")
  let maxflow: (G.t, G.V.t, G.V.t) => (G.E.t => F.t, F.t)
}

@@ocaml.text("  {2 Ford-Fulkerson maximal flow algorithm} ")

@ocaml.doc(" Minimal digraph signature for Ford-Fulkerson.
    Sub-signature of {!Sig.G}. ")
module type G_FORD_FULKERSON = {
  type t
  module V: Sig.HASHABLE
  module E: {
    type t
    type label
    let src: t => V.t
    let dst: t => V.t
    let label: t => label
  }
  let iter_succ_e: (E.t => unit, t, V.t) => unit
  let iter_pred_e: (E.t => unit, t, V.t) => unit
}

module type FLOWMIN = {
  include FLOW
  let min_capacity: label => t
}

module Ford_Fulkerson: (G: G_FORD_FULKERSON, F: FLOWMIN with type label = G.E.label) =>
{
  @ocaml.doc(" [maxflow g v1 v2] searchs the maximal flow from source [v1]
      to terminal [v2] using the Ford-Fulkerson algorithm. It
      returns the new flows on each edges and the growth of the
      flow. ")
  let maxflow: (G.t, G.V.t, G.V.t) => (G.E.t => F.t, F.t)
}
