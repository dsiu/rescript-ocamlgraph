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

/*
  Copyright © 2009 Carnegie-Mellon University, David Brumley, and Ivan Jager.
  From the BAP library; see http://bap.ece.cmu.edu

  Modified by OCamlGraph's authors.
*/

" Dominators

    All of the functions in this module assume that the graph is not modified
    between calling one of these functions and using the returned functions.
    Such mutation results in undefined behavior.
    @author Ivan Jager
")

exception Unreachable

module type G = {
  type t
  module V: Sig.COMPARABLE
  let pred: (t, V.t) => list<V.t>
  let succ: (t, V.t) => list<V.t>
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let iter_vertex: (V.t => unit, t) => unit
  let iter_succ: (V.t => unit, t, V.t) => unit
  let nb_vertex: t => int
}

module type S = {
  @ocaml.doc(" type of graphs ")
  type t

  @ocaml.doc(" type of vertices ")
  type vertex

  module S: Set.S with type elt = vertex

  @ocaml.doc(" function from [n] to [n]'s immediate dominator ")
  type idom = vertex => vertex

  @ocaml.doc(" [idoms x y] is true when [x] is [y]'s immediate dominator ")
  type idoms = (vertex, vertex) => bool

  @ocaml.doc(" function from [x] to a list of nodes immediately dominated by [x] ")
  type dom_tree = vertex => list<vertex>

  @ocaml.doc(" function from node to a list of nodes that dominate it. ")
  type dominators = vertex => list<vertex>

  @ocaml.doc(" [dom x y] returns true iff [x] dominates [y] ")
  type dom = (vertex, vertex) => bool

  @ocaml.doc(" [sdom x y] returns true iff [x] strictly dominates [y]. ")
  type sdom = (vertex, vertex) => bool

  @ocaml.doc(" function from [x] to a list of nodes not dominated by [x], but with
      predecessors which are dominated by [x] ")
  type dom_frontier = vertex => list<vertex>

  @ocaml.doc(" Computes the dominator tree, using the Lengauer-Tarjan algorithm.
      [compute_idom cfg s0] returns a function [idom : V.t -> V.t] s.t.
      [idom x] returns the immediate dominator of [x]. ")
  let compute_idom: (t, vertex, vertex) => vertex

  @ocaml.doc(" Given a function from a node to it's dominators, returns a function
      [dom : V.t -> V.t -> bool] s.t. [dom x y] returns true when
      [x] dominates [y]. ")
  let dominators_to_dom: ('a => S.t, vertex, 'a) => bool

  @ocaml.doc(" Given a function from a node to it's dominators, returns a function
      [sdom : V.t -> V.t -> bool] s.t. [sdom x y] returns true when
      [x] strictly dominates [y]. ")
  let dominators_to_sdom: (vertex => S.t, vertex, vertex) => bool
  let dom_to_sdom: ((vertex, vertex) => bool, vertex, vertex) => bool

  @ocaml.doc(" Given a a function from a node to it's dominators, returns a function
      from a node to it's strict dominators. ")
  let dominators_to_sdominators: (vertex => S.t, vertex) => S.t

  @ocaml.doc(" Given a function from a node to it's dominators, returns a function
      [idoms : vertex -> vertex -> bool] s.t. [idoms x y] returns true when
      [x] is the immediate dominator of [y]. ")
  let dominators_to_idoms: (vertex => S.t, vertex, vertex) => bool

  @ocaml.doc(" Computes a dominator tree (function from x to a list of nodes immediately
      dominated by x) for the given CFG and dominator function.
      Note: The dominator tree is also called [IDom] by Muchnick.
      Note: If you are computing a post-dominator tree, then the
      optional argument pred should be G.succ. ")
  let dominators_to_dom_tree: (
    t,
    ~pred: (t, vertex) => list<vertex>=?,
    vertex => S.t,
    vertex,
  ) => S.t

  @ocaml.doc(" Computes a dominator tree (function from x to a list of nodes immediately
      dominated by x) for the given CFG and idom function. ")
  let idom_to_dom_tree: (t, vertex => vertex, vertex) => list<vertex>

  let idom_to_idoms: (idom, vertex, vertex) => bool

  @ocaml.doc(" Computes the dominance frontier.
      As specified in section 19.1 of Modern Compiler Implementation in ML
      by Andrew Appel. ")
  let compute_dom_frontier: (t, dom_tree, idom, vertex) => list<vertex>

  let idom_to_dominators: ('a => 'a, 'a) => list<'a>

  let idom_to_dom: (vertex => vertex, vertex, vertex) => bool

  @ocaml.doc(" Returns a list of the non-trivial dominators of a flowgraph G(v) given
      the start vertex [v] and the corresponding dominator tree. E.g.,
      [dom_tree_to_nontrivial_dom v (idom_to_dom_tree g (compute_idom g v))].
      A vertex u is a non-trivial dominator of G(v) if it dominates some
      vertex w other than v and u. ")
  let dom_tree_to_nontrivial_dom: (vertex, dom_tree) => list<vertex>

  @ocaml.doc(" As for [dom_tree_to_nontrivial_dom] but returns a set. ")
  let dom_tree_to_snontrivial_dom: (vertex, dom_tree) => S.t
}

module Make: (G: G) => (S with type t = G.t and type vertex = G.V.t)

module type I = {
  include G
  let empty: unit => t
  let add_edge: (t, V.t, V.t) => t
}

module Make_graph: (G: I) =>
{
  include S with type t = G.t and type vertex = G.V.t

  type dom_graph = unit => t

  type dom_functions = {
    idom: idom,
    idoms: idoms,
    dom_tree: dom_tree,
    dominators: dominators,
    dom: dom,
    sdom: sdom,
    dom_frontier: dom_frontier,
    dom_graph: dom_graph,
  }

  let compute_dom_graph: (G.t, dom_tree) => G.t

  @ocaml.doc(" Computes all dominance functions.

      This function computes some things eagerly and some lazily, so don't
      worry about it doing extra work to compute functions you don't need,
      but also don't call it if you aren't going to use anything it returns.

      @return a record containing all dominance functions for the given graph
      and entry node.
  ")
  let compute_all: (G.t, vertex) => dom_functions
}
