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

"
   Minimal separators of a graph

   Based on the article:
   Generating all the minimal separators of a graph.
   by A. Berry, J.-P. Bordat and O.Cogis
   http://www.isima.fr/berry/generating.html

   A set [S] of vertices is a minimal separator if it exists 2 distinct
   connected components [C] and [D] in [G \ S] such that each vertex of [S] has
   a successor in [C] and [D]. ")

@ocaml.doc(" Minimal signature for computing the minimal separators ")
module type G = {
  type t
  module V: Sig.COMPARABLE
  let succ: (t, V.t) => list<V.t>
  let iter_succ: (V.t => unit, t, V.t) => unit
  let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
  let iter_vertex: (V.t => unit, t) => unit
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
}

module type MINSEP = {
  @ocaml.doc(" Implementation of a graph ")
  module G: G

  @ocaml.doc(" Implementation of a set of vertex ")
  module Vertex_Set: Set.S with type elt = G.V.t

  @ocaml.doc(" Implementation of a set of [Vertex_Set] ")
  module VSetset: Set.S with type elt = Vertex_Set.t

  @ocaml.doc(" [allminsep g] computes the list of all minimal separators of g. ")
  let allminsep: G.t => list<Vertex_Set.t>

  @ocaml.doc(" Less efficient that [allminsep] ")
  let list_of_allminsep: G.t => list<list<G.V.t>>

  @ocaml.doc(" Less efficient that [allminsep] ")
  let set_of_allminsep: G.t => VSetset.t
}

@ocaml.doc(" Implementation for a persistent graph ")
module P: (
  G: {
    include G
    let remove_vertex: (t, V.t) => t
  },
) => (MINSEP with module G = G)

@ocaml.doc(" Implementation for an imperative graph.
    Less efficient that the implementation for a persistent graph ")
module I: (
  G: {
    include G
    module Mark: Sig.MARK with type graph = t and type vertex = V.t
  },
) => (MINSEP with module G = G)
