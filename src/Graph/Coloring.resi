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

" [k]-coloring of undirected graphs.

    A [k]-coloring of a graph [g] is a mapping [c] from nodes to [\{1,...,k\}]
    such that [c(u) <> c(v)] for any edge [u-v] in [g]. ")

exception NoColoring

@@ocaml.text(" {2 Graph coloring for graphs with integer marks} ")

@ocaml.doc(" Minimal graph signature for {!Mark}.
    Sub-signature of {!Sig.IM}. ")
module type GM = {
  let is_directed: bool
  type t
  let nb_vertex: t => int
  module V: Sig.COMPARABLE
  let out_degree: (t, V.t) => int
  let iter_vertex: (V.t => unit, t) => unit
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let iter_succ: (V.t => unit, t, V.t) => unit
  let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
  module Mark: {
    let get: V.t => int
    let set: (V.t, int) => unit
  }
}

@ocaml.doc(" Provide a function for [k]-coloring a graph with integer marks.
    The provided function is more efficient that the one provided by functor
    {!Make} above. ")
module Mark: (G: GM) =>
{
  @ocaml.doc(" [coloring g k] colors the nodes of graph [g] using [k] colors,
      assigning the marks integer values between 1 and [k].

      The graph marks may be partially set before starting; the meaning of
      initial values is as follows:
      - 0: a node to be colored
      - any value between 1 and [k]: a color already assigned
      - any value greater than [k]: a node to be ignored

      @raise NoColoring if [g] cannot be [k]-colored.

      Worst-case time complexity is exponential. Space complexity is O(V). ")
  let coloring: (G.t, int) => unit

  @ocaml.doc(" [two_color g] attemps to color [g] with colors 1 and 2.
      @raise NoColoring if this is not possible (i.e., if the graph
      is not bipartite). Runs in O(V+E). ")
  let two_color: G.t => unit
}

@@ocaml.text(" {2 Graph coloring for graphs without marks} ")

@ocaml.doc(" Minimal graph signature for {!Make}.
    Sub-signature of {!Sig.G}. ")
module type G = {
  let is_directed: bool
  type t
  let nb_vertex: t => int
  module V: Sig.COMPARABLE
  let out_degree: (t, V.t) => int
  let iter_vertex: (V.t => unit, t) => unit
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let iter_succ: (V.t => unit, t, V.t) => unit
  let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
}

@@ocaml.doc(" Provide a function for [k]-coloring a graph. ")

module Make: (G: G) =>
{
  @@ocaml.text(" Hash tables used to store the coloring ")
  module H: Hashtbl.S

  let coloring: (G.t, int) => H.t<int>
  @@ocaml.text(" [coloring g k] colors the graph [g] with [k] colors and returns the
      coloring as a hash table mapping nodes to their colors.
      Colors are integers from 1 to [k].

      @raise NoColoring if [g] cannot be [k]-colored.

      Worst-case time complexity is exponential. Space complexity is O(V). ")

  let two_color: G.t => H.t<int>
}
