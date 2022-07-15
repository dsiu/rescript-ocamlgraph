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

" Graph builders in order to persistent/imperative graphs sharing a same
    signature. ")

@@ocaml.text(" {2 Common interface for graph builders}.

    Note: the following functions always return graphs but this is meaningless
    for imperative implementations (the graph is modified in-place).
    This is just to provide a common interface. ")

module type S = {
  module G: Sig.G
  let empty: unit => G.t
  let copy: G.t => G.t
  let add_vertex: (G.t, G.V.t) => G.t
  let add_edge: (G.t, G.V.t, G.V.t) => G.t
  let add_edge_e: (G.t, G.E.t) => G.t
  let remove_vertex: (G.t, G.V.t) => G.t
  let remove_edge: (G.t, G.V.t, G.V.t) => G.t
  let remove_edge_e: (G.t, G.E.t) => G.t
}

module type INT = S with type G.V.label = int

@@ocaml.text(" {1 Builders for the various graph implementations} ")

@ocaml.doc(" Persistent Graphs Builders. ")
module P: (G: Sig.P) => (S with module G = G)

@ocaml.doc(" Imperative Graphs Builders. ")
module I: (G: Sig.I) => (S with module G = G)

/*
Local Variables:
compile-command: "make -C .."
End:
*/
