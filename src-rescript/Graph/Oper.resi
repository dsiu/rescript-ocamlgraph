@@ocaml.text(
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

  " Basic operations over graphs "
)

@@ocaml.text(" {2 Basic operations over graphs} ")

module type S = {
  type g

  @ocaml.doc(" [transitive_closure ?reflexive g] returns the transitive closure
      of [g] (as a new graph). Loops (i.e. edges from a vertex to itself)
      are added only if [reflexive] is [true] (default is [false]). ")
  let transitive_closure: (~reflexive: bool=?, g) => g

  @ocaml.doc(" [add_transitive_closure ?reflexive g] replaces [g] by its
      transitive closure. Meaningless for persistent implementations
      (then acts as [transitive_closure]). ")
  let add_transitive_closure: (~reflexive: bool=?, g) => g

  @ocaml.doc(" [transitive_reduction ?reflexive g] returns the transitive reduction
      of [g] (as a new graph). Loops (i.e. edges from a vertex to itself)
      are removed only if [reflexive] is [true] (default is [false]). ")
  let transitive_reduction: (~reflexive: bool=?, g) => g

  @ocaml.doc(" [replace_by_transitive_reduction ?reflexive g] replaces [g] by its
      transitive reduction. Meaningless for persistent implementations
      (then acts as [transitive_reduction]). ")
  let replace_by_transitive_reduction: (~reflexive: bool=?, g) => g

  @ocaml.doc(" [mirror g] returns a new graph which is the mirror image of [g]:
      each edge from [u] to [v] has been replaced by an edge from [v] to [u].
      For undirected graphs, it simply returns [g].
      Note: Vertices are shared between [g] and [mirror g]; you may need to
      make a copy of [g] before using [mirror] ")
  let mirror: g => g

  @ocaml.doc(" [complement g] returns a new graph which is the complement of [g]:
      each edge present in [g] is not present in the resulting graph and
      vice-versa. Edges of the returned graph are unlabeled. ")
  let complement: g => g

  @ocaml.doc(" [intersect g1 g2] returns a new graph which is the intersection of [g1]
      and [g2]: each vertex and edge present in [g1] *and* [g2] is present
      in the resulting graph. ")
  let intersect: (g, g) => g

  @ocaml.doc(" [union g1 g2] returns a new graph which is the union of [g1] and [g2]:
      each vertex and edge present in [g1] *or* [g2] is present in the
      resulting graph. ")
  let union: (g, g) => g
}

@ocaml.doc(" Basic operations over graphs ")
module Make: (B: Builder.S) => (S with type g = B.G.t)

@ocaml.doc(" Basic operations over persistent graphs ")
module P: (G: Sig.P) => (S with type g = G.t)

@ocaml.doc(" Basic operations over imperative graphs ")
module I: (G: Sig.I) => (S with type g = G.t)

@@ocaml.text(" {2 Choose} ")

@ocaml.doc(" Choose an element in a graph ")
module Choose: (
  G: {
    type t
    type vertex
    type edge
    let iter_vertex: (vertex => unit, t) => unit
    let iter_edges_e: (edge => unit, t) => unit
  },
) =>
{
  @ocaml.doc(" [choose_vertex g] returns a vertex from the graph.
      @raise Invalid_argument if the graph is empty. ")
  let choose_vertex: G.t => G.vertex

  @ocaml.doc(" [choose_edge g] returns an edge from the graph.
      @raise Invalid_argument if the graph has no edge. ")
  let choose_edge: G.t => G.edge
}

@@ocaml.text(" {2 Neighbourhood} ")

@ocaml.doc(" Neighbourhood of vertex / vertices ")
module Neighbourhood: (
  G: {
    type t
    module V: Sig.COMPARABLE
    let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
    let succ: (t, V.t) => list<V.t>
  },
) =>
{
  module Vertex_Set: Set.S with type elt = G.V.t

  @@ocaml.text(" The neighbourhood of a vertex [v] is
      \{ v' | (succ g v) and (v <> v') \} ")

  @ocaml.doc(" Neighbourhood of a vertex as a list. ")
  let list_from_vertex: (G.t, G.V.t) => list<G.V.t>

  @ocaml.doc(" Neighbourhood of a vertex as a set.
      Less efficient that [list_from_vertex]. ")
  let set_from_vertex: (G.t, G.V.t) => Vertex_Set.t

  @@ocaml.text(" The neighbourhood of a set [S] of vertices is [U \ S] where
      [U] is the union of neighbourhoods of each vertex of [S]. ")

  @ocaml.doc(" Neighbourhood of a list of vertices as a list. ")
  let list_from_vertices: (G.t, list<G.V.t>) => list<G.V.t>

  @ocaml.doc(" Neighbourhood of a list of vertices as a set.
      More efficient that [list_from_vertices]. ")
  let set_from_vertices: (G.t, list<G.V.t>) => Vertex_Set.t
}
