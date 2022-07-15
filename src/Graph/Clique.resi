@@ocaml.text(
  /* ************************************************************************ */
  /*  */
  /* Ocamlgraph: a generic graph library for OCaml */
  /* Copyright (C) 2014-2015 */
  /* Giselle Reis */
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

  " Graph cliques "
)

@@ocaml.text(" {1 Clique algorithms} ")

@@ocaml.text(" {2 Bron-Kerbosch Algorithm}

    This algorithm will find and return all maximal cliques in an undirected graph. ")

@ocaml.doc(" Minimal graph signature for Bron-Kerbosch.
    Sub-signature of {!Sig.G}. ")
module type G = {
  type t
  module V: Sig.COMPARABLE
  let succ: (t, V.t) => list<V.t>
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
}

module Bron_Kerbosch: (G: G) =>
{
  @ocaml.doc(" [maximalcliques g] computes all the maximal cliques of [g] using the
      Bron-Kerbosch algorithm. It returns the sets of vertices belonging to the
      same maximal clique. ")
  let maximalcliques: G.t => list<list<G.V.t>>
}
