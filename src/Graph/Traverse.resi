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

  " Graph traversal. "
)

@@ocaml.text(" {2 Dfs and Bfs}

   In the modules below, the most meaningful functions are the
   [iter/fold_component] functions, where the starting point of the
   traversal is user-provided.

   Functions [iter/fold] to traverse the whole graph are also
   provided, for convenience, and they proceed as follows: they run
   the user-provided [iter/fold_vertex] functions (from input module
   [G]) and, for each vertex not yet visited, start a new traversal
   from this vertex. In particular, each traversal is not necessarily
   started from a vertex without predecessors. Said otherwise, it is
   up to you to come up with an [iter_vertex] function that will
   identify suitable roots, e.g. vertices with no predecessors, if
   this is really what you want.
")

@ocaml.doc(" Minimal graph signature for {!Dfs} and {!Bfs}.
    Sub-signature of {!Sig.G}. ")
module type G = {
  let is_directed: bool
  type t
  module V: Sig.COMPARABLE
  @ocaml.doc(" It is enough to iter over all the roots (vertices without predecessor) of
      the graph, even if iterating over the other vertices is correct.
      (See the comment above.) ")
  let iter_vertex: (V.t => unit, t) => unit

  @ocaml.doc(" It is enough to fold over all the roots (vertices without predecessor) of
      the graph, even if folding over the other vertices is correct.
      (See the comment above.) ")
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a

  let iter_succ: (V.t => unit, t, V.t) => unit
  let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
}

@ocaml.doc(" Depth-first search ")
module Dfs: (G: G) =>
{
  @@ocaml.text(" {2 Classical big-step iterators} ")

  @ocaml.doc(" [iter pre post g] visits all nodes of [g] in depth-first search,
      applying [pre] to each visited node before its successors,
      and [post] after them. Each node is visited exactly once.
      Not tail-recursive. ")
  let iter: (~pre: G.V.t => unit=?, ~post: G.V.t => unit=?, G.t) => unit

  @ocaml.doc(" applies only a prefix function; note that this function is more
      efficient than [iter] and is tail-recursive. ")
  let prefix: (G.V.t => unit, G.t) => unit

  @ocaml.doc(" applies only a postfix function. Not tail-recursive. ")
  let postfix: (G.V.t => unit, G.t) => unit

  @@ocaml.text(" Same thing, but for a single connected component
      (only [prefix_component] is tail-recursive) ")

  let iter_component: (~pre: G.V.t => unit=?, ~post: G.V.t => unit=?, G.t, G.V.t) => unit
  let prefix_component: (G.V.t => unit, G.t, G.V.t) => unit
  let postfix_component: (G.V.t => unit, G.t, G.V.t) => unit

  @@ocaml.text(" {2 Classical folds} ")

  @ocaml.doc(" The function is applied each time a node is reached for the first time,
      before iterating over its successors. Tail-recursive. ")
  let fold: ((G.V.t, 'a) => 'a, 'a, G.t) => 'a

  @ocaml.doc(" Idem, but limited to a single root vertex. ")
  let fold_component: ((G.V.t, 'a) => 'a, 'a, G.t, G.V.t) => 'a

  @@ocaml.text(" {2 Step-by-step iterator}

      This is a variant of the iterators above where you can move on
      step by step. The abstract type [iterator] represents the current
      state of the iteration. The [step] function returns the next state.
      In each state, function [get] returns the currently visited vertex.
      On the final state both [get] and [step] raises exception [Exit].

      Note: the iterator type is persistent (i.e. is not modified by the
      [step] function) and thus can be used in backtracking algorithms. ")

  type iterator
  let start: G.t => iterator
  let step: iterator => iterator
  let get: iterator => G.V.t

  @@ocaml.text(" {2 Cycle detection} ")

  @ocaml.doc(" [has_cycle g] checks for a cycle in [g]. Linear in time and space. ")
  let has_cycle: G.t => bool
}

@ocaml.doc(" Breadth-first search ")
module Bfs: (G: G) =>
{
  @@ocaml.text(" {2 Classical big-step iterators} ")

  @ocaml.doc(" The function is applied each time a node is reached for the first time.
      Not tail-recursive. ")
  let iter: (G.V.t => unit, G.t) => unit

  @ocaml.doc(" Idem, but limited to a single root vertex. ")
  let iter_component: (G.V.t => unit, G.t, G.V.t) => unit

  @@ocaml.text(" {2 Classical folds} ")

  @ocaml.doc(" The function is applied each time a node is reached for the first time.
      Not tail-recursive. ")
  let fold: ((G.V.t, 'a) => 'a, 'a, G.t) => 'a

  @ocaml.doc(" Idem, but limited to a single root vertex. ")
  let fold_component: ((G.V.t, 'a) => 'a, 'a, G.t, G.V.t) => 'a

  @@ocaml.text(" {2 Step-by-step iterator}
      See module [Dfs] ")

  type iterator
  let start: G.t => iterator
  let step: iterator => iterator
  let get: iterator => G.V.t
}

@@ocaml.text(" {2 Traversal with marking}

    Provide a more efficient version of depth-first algorithm when graph
    vertices are marked. ")

@ocaml.doc(" Minimal graph signature for graph traversal with marking.
    Sub-signature of {!Sig.IM}. ")
module type GM = {
  type t
  module V: {
    type t
  }
  let iter_vertex: (V.t => unit, t) => unit
  let iter_succ: (V.t => unit, t, V.t) => unit
  module Mark: {
    let clear: t => unit
    let get: V.t => int
    let set: (V.t, int) => unit
  }
}

@ocaml.doc(" Graph traversal with marking.
    Only applies to imperative graphs with marks. ")
module Mark: (G: GM) =>
{
  @ocaml.doc(" [dfs g] traverses [g] in depth-first search, marking all nodes. ")
  let dfs: G.t => unit

  @ocaml.doc(" [has_cycle g] checks for a cycle in [g]. Modifies the marks.
      Linear time, constant space. ")
  let has_cycle: G.t => bool
}
