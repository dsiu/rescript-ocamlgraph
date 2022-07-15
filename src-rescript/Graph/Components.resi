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

  " Strongly connected components. "
)

@ocaml.doc(" Minimal graph signature required by {!Make}.
    Sub-signature of {!Sig.G}. ")
module type G = {
  type t
  module V: Sig.COMPARABLE
  let iter_vertex: (V.t => unit, t) => unit
  let iter_succ: (V.t => unit, t, V.t) => unit
}

@ocaml.doc(" Functor providing functions to compute strongly connected components of a
    graph. ")
module Make: (G: G) =>
{
  @ocaml.doc(" [scc g] computes the strongly connected components of [g].
      The result is a pair [(n,f)] where [n] is the number of
      components. Components are numbered from [0] to [n-1], and
      [f] is a function mapping each vertex to its component
      number. In particular, [f u = f v] if and only if [u] and
      [v] are in the same component. Another property of the
      numbering is that components are numbered in a topological
      order: if there is an arc from [u] to [v], then [f u >= f v]

      Not tail-recursive.
      Complexity: O(V+E)
      The function returned has complexity O(1) ")
  let scc: G.t => (int, G.V.t => int)

  @ocaml.doc(" [scc_array g] computes the strongly connected components of [g].
      Components are stored in the resulting array, indexed with a
      numbering with the same properties as for [scc] above. ")
  let scc_array: G.t => array<list<G.V.t>>

  @ocaml.doc(" [scc_list g] computes the strongly connected components of [g].
      The result is a partition of the set of the vertices of [g].
      The [n]-th components is [(scc_array g).(n-1)]. ")
  let scc_list: G.t => list<list<G.V.t>>
}

@@ocaml.text(" Connectivity in strongly connected directed graphs ")

module Connectivity: (GB: Builder.S) =>
{
  module S: Set.S with type elt = GB.G.vertex

  @ocaml.doc(" Computes the strong articulation points of the given
      strongly connected directed graph. The result is undefined if the
      input graph is not directed and strongly connected.

      A strong articulation point is a vertex that when removed from the
      original graph disconnects that graph into two or more components.

      The implementation involves constructing the mirror image of the
      graph; for bidirectional graphs prefer {!module:BiConnectivity}.

      Implements the algorithm from Italiano, Laura, and Santaroni,
      TCS 447 (2012), \"Finding strong bridges and strong articulation points
      in linear time\".
      Complexity: O(V + E) ")
  let strong_articulation_points: GB.G.t => list<GB.G.vertex>

  @ocaml.doc(" As for [strong_articulation_points] but returns a set. ")
  let sstrong_articulation_points: GB.G.t => S.t
}

module BiConnectivity: (G: Sig.G) =>
{
  module S: Set.S with type elt = G.vertex

  @ocaml.doc(" Computes the strong articulation points of the given
      strongly connected directed  graph. The result is undefined if the
      input graph is not directed and strongly connected.

      A strong articulation point is a vertex that when removed from the
      original graph disconnects that graph into two or more components.

      The implementation traverses the graph by iterating over predecessors;
      for unidirectional graphs prefer {!module:Connectivity}.

      Implements the algorithm from Italiano, Laura, and Santaroni,
      TCS 447 (2012), \"Finding strong bridges and strong articulation points
      in linear time\".
      Complexity: O(V + E) ")
  let strong_articulation_points: G.t => list<G.vertex>

  @ocaml.doc(" As for [strong_articulation_points] but returns a set. ")
  let sstrong_articulation_points: G.t => S.t
}

@@ocaml.text(" Connected components (for undirected graphs).
    The implementation uses union-find. Time complexity is (quasi) O(V+E).
    Space complexity is O(V). ")

module type U = {
  type t
  module V: Sig.COMPARABLE
  let iter_vertex: (V.t => unit, t) => unit
  let iter_edges: ((V.t, V.t) => unit, t) => unit
}

module Undirected: (G: U) =>
{
  let components: G.t => (int, G.V.t => int)
  let components_array: G.t => array<list<G.V.t>>
  let components_list: G.t => list<list<G.V.t>>
}
