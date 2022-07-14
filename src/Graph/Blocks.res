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

  " Common implementation to persistent and imperative graphs. "
)

open Sig
open Util

let first_value_for_cpt_vertex = 0
let cpt_vertex = ref(first_value_for_cpt_vertex)
/* global counter for abstract vertex */

/* [max_cpt t1 t2] returns the maximum of [t1] and [t2] wrt the total ordering
   induced by tags creation. This ordering is defined as follow:
   forall tags t1 t2,
   t1 <= t2 iff
   t1 is before t2 in the finite sequence
   [0; 1; ..; max_int; min_int; min_int-1; -1] */
let max_cpt = (c1, c2) => max(c1 + min_int, c2 + min_int) - min_int

/* This function must be called after the unserialisation of any abstract
 vertex if you want to create new vertices. */
let after_unserialization = serialized_cpt_vertex =>
  cpt_vertex := max_cpt(serialized_cpt_vertex, cpt_vertex.contents)

@@ocaml.text(
  /* ************************************************************************* */
  " {2 Association table builder} "
)
/* ************************************************************************* */

@ocaml.doc(" Common signature to an imperative/persistent association table ")
module type HM = {
  type return<'a>
  type t<'a>
  type key
  let create: (~size: int=?, unit) => t<'a>
  let create_from: t<'a> => t<'a>
  let empty: return<'a>
  let clear: t<'a> => unit
  let is_empty: t<'a> => bool
  let add: (key, 'a, t<'a>) => t<'a>
  let remove: (key, t<'a>) => t<'a>
  let mem: (key, t<'a>) => bool
  let find: (key, t<'a>) => 'a
  @ocaml.doc(" [find_and_raise k t s] is equivalent to [find k t] but
      raises [Invalid_argument s] when [find k t] raises [Not_found] ")
  let find_and_raise: (key, t<'a>, string) => 'a

  let iter: ((key, 'a) => unit, t<'a>) => unit
  let map: ((key, 'a) => (key, 'a), t<'a>) => t<'a>
  let fold: ((key, 'a, 'b) => 'b, t<'a>, 'b) => 'b
  let copy: t<'a> => t<'a>
}

module type TBL_BUILDER = (X: COMPARABLE) => (HM with type key = X.t)

@ocaml.doc(" [HM] implementation using hashtbl. ")
module Make_Hashtbl = (X: COMPARABLE) => {
  include Hashtbl.Make(X)

  type return<'a> = unit
  let empty = ()
  /* never call and not visible for the user thank's to signature
   constraints */

  let create_from = h => create(length(h))
  let create = (~size=97, ()) => create(size)

  let is_empty = h => length(h) == 0

  let find_and_raise = (k, h, s) =>
    try find(h, k) catch {
    | Not_found => invalid_arg(s)
    }

  let map = (f, h) => {
    let h' = create_from(h)
    iter((k, v) => {
      let (k, v) = f(k, v)
      add(h', k, v)
    }, h)
    h'
  }

  let add = (k, v, h) => {
    replace(h, k, v)
    h
  }
  let remove = (k, h) => {
    remove(h, k)
    h
  }
  let mem = (k, h) => mem(h, k)
  let find = (k, h) => find(h, k)
}

@ocaml.doc(" [HM] implementation using map ")
module Make_Map = (X: COMPARABLE) => {
  include Map.Make(X)
  type return<'a> = t<'a>
  let create = (~size as _=?, ()) => assert false
  /* never call and not visible for the user thank's to
   signature constraints */
  let create_from = _ => empty
  let copy = m => m
  let map = (f, m) => fold((k, v, m) => {
      let (k, v) = f(k, v)
      add(k, v, m)
    }, m, empty)
  let find_and_raise = (k, h, s) =>
    try find(k, h) catch {
    | Not_found => invalid_arg(s)
    }
  let clear = _ => assert false
  /* never call and not visible for the user thank's to
   signature constraints */
}

@@ocaml.text(
  /* ************************************************************************* */
  " {2 Blocks builder} "
)
/* ************************************************************************* */

@ocaml.doc(" Common implementation to all (directed) graph implementations. ")
module Minimal = (S: Set.S, HM: HM) => {
  type vertex = HM.key

  let is_directed = true
  let empty = HM.empty
  let create = HM.create
  let is_empty = HM.is_empty
  let copy = HM.copy
  let clear = HM.clear

  let nb_vertex = g => HM.fold((_, _) => succ, g, 0)
  let nb_edges = g => HM.fold((_, s, n) => n + S.cardinal(s), g, 0)
  let out_degree = (g, v) =>
    S.cardinal(
      try HM.find(v, g) catch {
      | Not_found => invalid_arg("[ocamlgraph] out_degree")
      },
    )

  let mem_vertex = (g, v) => HM.mem(v, g)

  let unsafe_add_vertex = (g, v) => HM.add(v, S.empty, g)
  let unsafe_add_edge = (g, v1, v2) => HM.add(v1, S.add(v2, HM.find(v1, g)), g)

  let add_vertex = (g, v) =>
    if HM.mem(v, g) {
      g
    } else {
      unsafe_add_vertex(g, v)
    }

  let iter_vertex = f => HM.iter((v, _) => f(v))
  let fold_vertex = f => HM.fold((v, _) => f(v))
}

@ocaml.doc(" All the predecessor operations from the iterators on the edges ")
module Pred = (
  S: {
    module PV: COMPARABLE
    module PE: EDGE with type vertex = PV.t
    type t
    let mem_vertex: (PV.t, t) => bool
    let iter_edges: ((PV.t, PV.t) => unit, t) => unit
    let fold_edges: ((PV.t, PV.t, 'a) => 'a, t, 'a) => 'a
    let iter_edges_e: (PE.t => unit, t) => unit
    let fold_edges_e: ((PE.t, 'a) => 'a, t, 'a) => 'a
  },
) => {
  open S

  let iter_pred = (f, g, v) => {
    if !mem_vertex(v, g) {
      invalid_arg("[ocamlgraph] iter_pred")
    }
    iter_edges((v1, v2) =>
      if PV.equal(v, v2) {
        f(v1)
      }
    , g)
  }

  let fold_pred = (f, g, v) => {
    if !mem_vertex(v, g) {
      invalid_arg("[ocamlgraph] fold_pred")
    }
    fold_edges((v1, v2, a) =>
      if PV.equal(v, v2) {
        f(v1, a)
      } else {
        a
      }
    , g)
  }

  let pred = (g, v) => fold_pred((v, l) => list{v, ...l}, g, v, list{})

  let in_degree = (g, v) => {
    if !mem_vertex(v, g) {
      invalid_arg("[ocamlgraph] in_degree")
    }
    fold_pred((_, n) => n + 1, g, v, 0)
  }

  let iter_pred_e = (f, g, v) => {
    if !mem_vertex(v, g) {
      invalid_arg("[ocamlgraph] iter_pred_e")
    }
    iter_edges_e(e =>
      if PV.equal(v, PE.dst(e)) {
        f(e)
      }
    , g)
  }

  let fold_pred_e = (f, g, v) => {
    if !mem_vertex(v, g) {
      invalid_arg("[ocamlgraph] fold_pred_e")
    }
    fold_edges_e((e, a) =>
      if PV.equal(v, PE.dst(e)) {
        f(e, a)
      } else {
        a
      }
    , g)
  }

  let pred_e = (g, v) => fold_pred_e((v, l) => list{v, ...l}, g, v, list{})
}

@ocaml.doc(" Common implementation to all the unlabeled (directed) graphs. ")
module Unlabeled = (V: COMPARABLE, HM: HM with type key = V.t) => {
  module S = Set.Make(V)

  module E = {
    type vertex = V.t
    include OTProduct(V, V)
    let src = fst
    let dst = snd
    type label = unit
    let label = _ => ()
    let create = (v1, (), v2) => (v1, v2)
  }
  type edge = E.t

  let mem_edge = (g, v1, v2) =>
    try S.mem(v2, HM.find(v1, g)) catch {
    | Not_found => false
    }

  let mem_edge_e = (g, (v1, v2)) => mem_edge(g, v1, v2)

  let find_edge = (g, v1, v2) =>
    if mem_edge(g, v1, v2) {
      (v1, v2)
    } else {
      raise(Not_found)
    }
  let find_all_edges = (g, v1, v2) =>
    try list{find_edge(g, v1, v2)} catch {
    | Not_found => list{}
    }

  let unsafe_remove_edge = (g, v1, v2) => HM.add(v1, S.remove(v2, HM.find(v1, g)), g)
  let unsafe_remove_edge_e = (g, (v1, v2)) => unsafe_remove_edge(g, v1, v2)

  let remove_edge = (g, v1, v2) => {
    if !HM.mem(v2, g) {
      invalid_arg("[ocamlgraph] remove_edge")
    }
    HM.add(v1, S.remove(v2, HM.find_and_raise(v1, g, "[ocamlgraph] remove_edge")), g)
  }

  let remove_edge_e = (g, (v1, v2)) => remove_edge(g, v1, v2)

  let iter_succ = (f, g, v) => S.iter(f, HM.find_and_raise(v, g, "[ocamlgraph] iter_succ"))

  let fold_succ = (f, g, v) => S.fold(f, HM.find_and_raise(v, g, "[ocamlgraph] fold_succ"))

  let iter_succ_e = (f, g, v) => iter_succ(v2 => f((v, v2)), g, v)
  let fold_succ_e = (f, g, v) => fold_succ(v2 => f((v, v2)), g, v)

  let succ = (g, v) => S.elements(HM.find_and_raise(v, g, "[ocamlgraph] succ"))
  let succ_e = (g, v) => fold_succ_e((e, l) => list{e, ...l}, g, v, list{})

  let map_vertex = (f, g) => {
    module MV = Util.Memo(V)
    let f = MV.memo(f)
    HM.map((v, s) => (f(v), S.fold((v, s) => S.add(f(v), s), s, S.empty)), g)
  }

  module I = {
    type t = HM.t<S.t>
    module PV = V
    module PE = E
    let iter_edges = f => HM.iter(v => S.iter(f(v)))
    let fold_edges = f => HM.fold(v => S.fold(f(v)))
    let iter_edges_e = f => iter_edges((v1, v2) => f((v1, v2)))
    let fold_edges_e = f => fold_edges((v1, v2, a) => f((v1, v2), a))
  }
  include I

  include Pred({
    include I
    let mem_vertex = HM.mem
  })
}

@ocaml.doc(" Common implementation to all the labeled (directed) graphs. ")
module Labeled = (V: COMPARABLE, E: ORDERED_TYPE, HM: HM with type key = V.t) => {
  module VE = OTProduct(V, E)
  module S = Set.Make(VE)

  module E = {
    type vertex = V.t
    type label = E.t
    type t = (vertex, label, vertex)
    let src = ((v, _, _)) => v
    let dst = ((_, _, v)) => v
    let label = ((_, l, _)) => l
    let create = (v1, l, v2) => (v1, l, v2)
    module C = OTProduct(V, VE)
    let compare = ((x1, x2, x3), (y1, y2, y3)) => C.compare((x1, (x3, x2)), (y1, (y3, y2)))
  }
  type edge = E.t

  let mem_edge = (g, v1, v2) =>
    try S.exists(((v2', _)) => V.equal(v2, v2'), HM.find(v1, g)) catch {
    | Not_found => false
    }

  let mem_edge_e = (g, (v1, l, v2)) =>
    try {
      let ve = (v2, l)
      S.exists(ve' => VE.compare(ve, ve') == 0, HM.find(v1, g))
    } catch {
    | Not_found => false
    }

  exception Found(edge)
  let find_edge = (g, v1, v2) =>
    try {
      S.iter(((v2', l)) =>
        if V.equal(v2, v2') {
          raise(Found(v1, l, v2'))
        }
      , HM.find(v1, g))
      raise(Not_found)
    } catch {
    | Found(e) => e
    }

  let find_all_edges = (g, v1, v2) =>
    try S.fold(((v2', l), acc) =>
      if V.equal(v2, v2') {
        list{(v1, l, v2'), ...acc}
      } else {
        acc
      }
    , HM.find(v1, g), list{}) catch {
    | Not_found => list{}
    }

  let unsafe_remove_edge = (g, v1, v2) =>
    HM.add(v1, S.filter(((v2', _)) => !V.equal(v2, v2'), HM.find(v1, g)), g)

  let unsafe_remove_edge_e = (g, (v1, l, v2)) => HM.add(v1, S.remove((v2, l), HM.find(v1, g)), g)

  let remove_edge = (g, v1, v2) => {
    if !HM.mem(v2, g) {
      invalid_arg("[ocamlgraph] remove_edge")
    }
    HM.add(
      v1,
      S.filter(
        ((v2', _)) => !V.equal(v2, v2'),
        HM.find_and_raise(v1, g, "[ocamlgraph] remove_edge"),
      ),
      g,
    )
  }

  let remove_edge_e = (g, (v1, l, v2)) => {
    if !HM.mem(v2, g) {
      invalid_arg("[ocamlgraph] remove_edge_e")
    }
    HM.add(v1, S.remove((v2, l), HM.find_and_raise(v1, g, "[ocamlgraph] remove_edge_e")), g)
  }

  let iter_succ = (f, g, v) =>
    S.iter(((w, _)) => f(w), HM.find_and_raise(v, g, "[ocamlgraph] iter_succ"))
  let fold_succ = (f, g, v) =>
    S.fold(((w, _)) => f(w), HM.find_and_raise(v, g, "[ocamlgraph] fold_succ"))

  let iter_succ_e = (f, g, v) =>
    S.iter(((w, l)) => f((v, l, w)), HM.find_and_raise(v, g, "[ocamlgraph] iter_succ_e"))

  let fold_succ_e = (f, g, v) =>
    S.fold(((w, l)) => f((v, l, w)), HM.find_and_raise(v, g, "[ocamlgraph] fold_succ_e"))

  let succ = (g, v) => fold_succ((w, l) => list{w, ...l}, g, v, list{})
  let succ_e = (g, v) => fold_succ_e((e, l) => list{e, ...l}, g, v, list{})

  let map_vertex = (f, g) => {
    module MV = Util.Memo(V)
    let f = MV.memo(f)
    HM.map((v, s) => (f(v), S.fold(((v, l), s) => S.add((f(v), l), s), s, S.empty)), g)
  }

  module I = {
    type t = HM.t<S.t>
    module PV = V
    module PE = E
    let iter_edges = f => HM.iter(v => S.iter(((w, _)) => f(v, w)))
    let fold_edges = f => HM.fold(v => S.fold(((w, _)) => f(v, w)))
    let iter_edges_e = f => HM.iter(v => S.iter(((w, l)) => f((v, l, w))))
    let fold_edges_e = f => HM.fold(v => S.fold(((w, l)) => f((v, l, w))))
  }
  include I

  include Pred({
    include I
    let mem_vertex = HM.mem
  })
}

@ocaml.doc(" The vertex module and the vertex table for the concrete graphs. ")
module ConcreteVertex = (F: TBL_BUILDER, V: COMPARABLE) => {
  module V = {
    include V
    type label = t
    let label = v => v
    let create = v => v
  }
  module HM = F(V)
}

module Make_Abstract = (
  G: {
    module HM: HM
    module S: Set.S
    include G with type t = HM.t<S.t> and type V.t = HM.key
    let remove_edge: (t, vertex, vertex) => t
    let remove_edge_e: (t, edge) => t
    /* val unsafe_add_vertex: t -> vertex -> t */ /* Was unused */
    let unsafe_add_edge: (t, vertex, S.elt) => t
    let unsafe_remove_edge: (t, vertex, vertex) => t
    let unsafe_remove_edge_e: (t, edge) => t
    let create: (~size: int=?, unit) => t
    let clear: t => unit
  },
) => {
  module I = {
    type t = {edges: G.t, mutable size: int}
    /* BE CAREFUL: [size] is only mutable in the imperative version. As
       there is no extensible records in current ocaml version, and for
       genericity purpose, [size] is mutable in both imperative and
       persistent implementations.
       Do not modify size in the persistent implementation! */

    type vertex = G.vertex
    type edge = G.edge

    module PV = G.V
    module PE = G.E

    let iter_edges = (f, g) => G.iter_edges(f, g.edges)
    let fold_edges = (f, g) => G.fold_edges(f, g.edges)
    let iter_edges_e = (f, g) => G.iter_edges_e(f, g.edges)
    let fold_edges_e = (f, g) => G.fold_edges_e(f, g.edges)
    let mem_vertex = (v, g) => G.mem_vertex(g.edges, v)
    let create = (~size=?, ()) => {edges: G.create(~size?, ()), size: 0}
    let clear = g => {
      G.clear(g.edges)
      g.size = 0
    }
  }
  include I

  include Pred(I)

  /* optimisations */

  let is_empty = g => g.size == 0
  let nb_vertex = g => g.size

  /* redefinitions */
  module V = G.V
  module E = G.E
  module HM = G.HM
  module S = G.S

  let unsafe_add_edge = G.unsafe_add_edge
  let unsafe_remove_edge = G.unsafe_remove_edge
  let unsafe_remove_edge_e = G.unsafe_remove_edge_e
  let is_directed = G.is_directed

  let remove_edge = g => G.remove_edge(g.edges)
  let remove_edge_e = g => G.remove_edge_e(g.edges)

  let out_degree = g => G.out_degree(g.edges)
  let in_degree = g => G.in_degree(g.edges)

  let nb_edges = g => G.nb_edges(g.edges)
  let succ = g => G.succ(g.edges)
  let mem_vertex = g => G.mem_vertex(g.edges)
  let mem_edge = g => G.mem_edge(g.edges)
  let mem_edge_e = g => G.mem_edge_e(g.edges)
  let find_edge = g => G.find_edge(g.edges)
  let find_all_edges = g => G.find_all_edges(g.edges)

  let iter_vertex = (f, g) => G.iter_vertex(f, g.edges)
  let fold_vertex = (f, g) => G.fold_vertex(f, g.edges)
  let iter_succ = (f, g) => G.iter_succ(f, g.edges)
  let fold_succ = (f, g) => G.fold_succ(f, g.edges)
  let succ_e = g => G.succ_e(g.edges)
  let iter_succ_e = (f, g) => G.iter_succ_e(f, g.edges)
  let fold_succ_e = (f, g) => G.fold_succ_e(f, g.edges)
  let map_vertex = (f, g) => {...g, edges: G.map_vertex(f, g.edges)}

  /* reimplementation */

  let copy = g => {
    let h = HM.create()
    let vertex = v =>
      try HM.find(v, h) catch {
      | Not_found =>
        let v' = V.create(V.label(v))
        let h' = HM.add(v, v', h)
        assert (h === h')
        v'
      }

    map_vertex(vertex, g)
  }
}

@@ocaml.text(" Support for explicitly maintaining edge set of
    predecessors.  Crucial for algorithms that do a lot of backwards
    traversal. ")

module BidirectionalMinimal = (S: Set.S, HM: HM) => {
  type vertex = HM.key

  let is_directed = true
  let empty = HM.empty
  let create = HM.create
  let clear = HM.clear
  let is_empty = HM.is_empty
  let copy = HM.copy

  let nb_vertex = g => HM.fold((_, _) => succ, g, 0)
  let nb_edges = g => HM.fold((_, (_, s), n) => n + S.cardinal(s), g, 0)
  let out_degree = (g, v) =>
    S.cardinal(
      snd(
        try HM.find(v, g) catch {
        | Not_found => invalid_arg("[ocamlgraph] out_degree")
        },
      ),
    )

  let mem_vertex = (g, v) => HM.mem(v, g)

  let unsafe_add_vertex = (g, v) => HM.add(v, (S.empty, S.empty), g)
  let add_vertex = (g, v) =>
    if HM.mem(v, g) {
      g
    } else {
      unsafe_add_vertex(g, v)
    }

  let iter_vertex = f => HM.iter((v, _) => f(v))
  let fold_vertex = f => HM.fold((v, _) => f(v))
}

module BidirectionalUnlabeled = (V: COMPARABLE, HM: HM with type key = V.t) => {
  module S = Set.Make(V)

  module E = {
    type vertex = V.t
    include OTProduct(V, V)
    let src = fst
    let dst = snd
    type label = unit
    let label = _ => ()
    let create = (v1, (), v2) => (v1, v2)
  }
  type edge = E.t

  let mem_edge = (g, v1, v2) =>
    try S.mem(v2, snd(HM.find(v1, g))) catch {
    | Not_found => false
    }

  let mem_edge_e = (g, (v1, v2)) => mem_edge(g, v1, v2)

  let find_edge = (g, v1, v2) =>
    if mem_edge(g, v1, v2) {
      (v1, v2)
    } else {
      raise(Not_found)
    }
  let find_all_edges = (g, v1, v2) =>
    try list{find_edge(g, v1, v2)} catch {
    | Not_found => list{}
    }

  let unsafe_remove_edge = (g, v1, v2) => {
    let (in_set, out_set) = HM.find(v1, g)
    let g = HM.add(v1, (in_set, S.remove(v2, out_set)), g)
    let (in_set, out_set) = HM.find(v2, g)
    HM.add(v2, (S.remove(v1, in_set), out_set), g)
  }

  let unsafe_remove_edge_e = (g, (v1, v2)) => unsafe_remove_edge(g, v1, v2)

  let remove_edge = (g, v1, v2) => {
    if !(HM.mem(v2, g) && HM.mem(v1, g)) {
      invalid_arg("[ocamlgraph] remove_edge")
    }
    unsafe_remove_edge(g, v1, v2)
  }

  let remove_edge_e = (g, (v1, v2)) => remove_edge(g, v1, v2)

  let iter_succ = (f, g, v) => S.iter(f, snd(HM.find_and_raise(v, g, "[ocamlgraph] iter_succ")))

  let fold_succ = (f, g, v) => S.fold(f, snd(HM.find_and_raise(v, g, "[ocamlgraph] fold_succ")))

  let iter_succ_e = (f, g, v) => iter_succ(v2 => f((v, v2)), g, v)
  let fold_succ_e = (f, g, v) => fold_succ(v2 => f((v, v2)), g, v)

  let succ = (g, v) => S.elements(snd(HM.find_and_raise(v, g, "[ocamlgraph] succ")))
  let succ_e = (g, v) => fold_succ_e((e, l) => list{e, ...l}, g, v, list{})

  let map_vertex = (f, g) => {
    module MV = Util.Memo(V)
    let f = MV.memo(f)
    HM.map(
      (v, (s1, s2)) => (
        f(v),
        (
          S.fold((v, s) => S.add(f(v), s), s1, S.empty),
          S.fold((v, s) => S.add(f(v), s), s2, S.empty),
        ),
      ),
      g,
    )
  }

  module I = {
    /* we keep sets for both incoming and outgoing edges */
    type t = /* incoming */ /* outgoing */ HM.t<(S.t /* incoming */, S.t)>
    module PV = V
    module PE = E
    let iter_edges = f => HM.iter((v, (_, outset)) => S.iter(f(v), outset))
    let fold_edges = f => HM.fold((v, (_, outset)) => S.fold(f(v), outset))
    let iter_edges_e = f => iter_edges((v1, v2) => f((v1, v2)))
    let fold_edges_e = f => fold_edges((v1, v2, a) => f((v1, v2), a))
  }
  include I

  let iter_pred = (f, g, v) => S.iter(f, fst(HM.find_and_raise(v, g, "[ocamlgraph] iter_pred")))

  let fold_pred = (f, g, v) => S.fold(f, fst(HM.find_and_raise(v, g, "[ocamlgraph] fold_pred")))

  let pred = (g, v) => S.elements(fst(HM.find_and_raise(v, g, "[ocamlgraph] pred")))

  let in_degree = (g, v) =>
    S.cardinal(
      fst(
        try HM.find(v, g) catch {
        | Not_found => invalid_arg("[ocamlgraph] in_degree")
        },
      ),
    )

  let iter_pred_e = (f, g, v) => iter_pred(v2 => f((v2, v)), g, v)
  let fold_pred_e = (f, g, v) => fold_pred(v2 => f((v2, v)), g, v)

  let pred_e = (g, v) => fold_pred_e((e, l) => list{e, ...l}, g, v, list{})
}

module BidirectionalLabeled = (V: COMPARABLE, E: ORDERED_TYPE, HM: HM with type key = V.t) => {
  module VE = OTProduct(V, E)
  module S = Set.Make(VE)

  module E = {
    type vertex = V.t
    type label = E.t
    type t = (vertex, label, vertex)
    let src = ((v, _, _)) => v
    let dst = ((_, _, v)) => v
    let label = ((_, l, _)) => l
    let create = (v1, l, v2) => (v1, l, v2)
    module C = OTProduct(V, VE)
    let compare = ((x1, x2, x3), (y1, y2, y3)) => C.compare((x1, (x3, x2)), (y1, (y3, y2)))
  }
  type edge = E.t

  let mem_edge = (g, v1, v2) =>
    try S.exists(((v2', _)) => V.equal(v2, v2'), snd(HM.find(v1, g))) catch {
    | Not_found => false
    }

  let mem_edge_e = (g, (v1, l, v2)) =>
    try {
      let ve = (v2, l)
      S.exists(ve' => VE.compare(ve, ve') == 0, snd(HM.find(v1, g)))
    } catch {
    | Not_found => false
    }

  exception Found(edge)
  let find_edge = (g, v1, v2) =>
    try {
      S.iter(((v2', l)) =>
        if V.equal(v2, v2') {
          raise(Found(v1, l, v2'))
        }
      , snd(HM.find(v1, g)))
      raise(Not_found)
    } catch {
    | Found(e) => e
    }

  let find_all_edges = (g, v1, v2) =>
    try S.fold(((v2', l), acc) =>
      if V.equal(v2, v2') {
        list{(v1, l, v2'), ...acc}
      } else {
        acc
      }
    , snd(HM.find(v1, g)), list{}) catch {
    | Not_found => list{}
    }

  let unsafe_remove_edge = (g, v1, v2) => {
    let (in_set, out_set) = HM.find(v1, g)
    let del = (v, set) => S.filter(((v', _)) => !V.equal(v, v'), set)
    let g = HM.add(v1, (in_set, del(v2, out_set)), g)
    let (in_set, out_set) = HM.find(v2, g)
    HM.add(v2, (del(v1, in_set), out_set), g)
  }

  let unsafe_remove_edge_e = (g, (v1, l, v2)) => {
    let (in_set, out_set) = HM.find(v1, g)
    let g = HM.add(v1, (in_set, S.remove((v2, l), out_set)), g)
    let (in_set, out_set) = HM.find(v2, g)
    HM.add(v2, (S.remove((v1, l), in_set), out_set), g)
  }

  let remove_edge = (g, v1, v2) => {
    /* if not (HM.mem v2 g) then invalid_arg "[ocamlgraph] remove_edge"; */
    let (in_set, out_set) = HM.find_and_raise(v1, g, "[ocamlgraph] remove_edge")
    let del = (v, set) => S.filter(((v', _)) => !V.equal(v, v'), set)
    let g = HM.add(v1, (in_set, del(v2, out_set)), g)
    let (in_set, out_set) = HM.find_and_raise(v2, g, "[ocamlgraph] remove_edge")
    HM.add(v2, (del(v1, in_set), out_set), g)
  }

  let remove_edge_e = (g, (v1, l, v2)) => {
    /* if not (HM.mem v2 g) then invalid_arg "[ocamlgraph] remove_edge_e"; */
    let (in_set, out_set) = HM.find_and_raise(v1, g, "[ocamlgraph] remove_edge_e")
    let g = HM.add(v1, (in_set, S.remove((v2, l), out_set)), g)
    let (in_set, out_set) = HM.find_and_raise(v2, g, "[ocamlgraph] remove_edge_e")
    HM.add(v2, (S.remove((v1, l), in_set), out_set), g)
  }

  let iter_succ = (f, g, v) =>
    S.iter(((w, _)) => f(w), snd(HM.find_and_raise(v, g, "[ocamlgraph] iter_succ")))

  let fold_succ = (f, g, v) =>
    S.fold(((w, _)) => f(w), snd(HM.find_and_raise(v, g, "[ocamlgraph] fold_succ")))

  let iter_succ_e = (f, g, v) =>
    S.iter(((w, l)) => f((v, l, w)), snd(HM.find_and_raise(v, g, "[ocamlgraph] iter_succ_e")))

  let fold_succ_e = (f, g, v) =>
    S.fold(((w, l)) => f((v, l, w)), snd(HM.find_and_raise(v, g, "[ocamlgraph] fold_succ_e")))

  let succ = (g, v) => fold_succ((w, l) => list{w, ...l}, g, v, list{})
  let succ_e = (g, v) => fold_succ_e((e, l) => list{e, ...l}, g, v, list{})

  let map_vertex = (f, g) => {
    module MV = Util.Memo(V)
    let f = MV.memo(f)
    HM.map(
      (v, (s1, s2)) => (
        f(v),
        (
          S.fold(((v, l), s) => S.add((f(v), l), s), s1, S.empty),
          S.fold(((v, l), s) => S.add((f(v), l), s), s2, S.empty),
        ),
      ),
      g,
    )
  }

  module I = {
    type t = HM.t<(S.t, S.t)>
    module PV = V
    module PE = E
    let iter_edges = f => HM.iter((v, (_, outset)) => S.iter(((w, _)) => f(v, w), outset))
    let fold_edges = f => HM.fold((v, (_, outset)) => S.fold(((w, _)) => f(v, w), outset))
    let iter_edges_e = f => HM.iter((v, (_, outset)) => S.iter(((w, l)) => f((v, l, w)), outset))
    let fold_edges_e = f => HM.fold((v, (_, outset)) => S.fold(((w, l)) => f((v, l, w)), outset))
  }
  include I

  let iter_pred = (f, g, v) =>
    S.iter(((w, _)) => f(w), fst(HM.find_and_raise(v, g, "[ocamlgraph] iter_pred")))

  let fold_pred = (f, g, v) =>
    S.fold(((w, _)) => f(w), fst(HM.find_and_raise(v, g, "[ocamlgraph] fold_pred")))

  let in_degree = (g, v) =>
    S.cardinal(
      fst(
        try HM.find(v, g) catch {
        | Not_found => invalid_arg("[ocamlgraph] in_degree")
        },
      ),
    )

  let iter_pred_e = (f, g, v) =>
    S.iter(((w, l)) => f((w, l, v)), fst(HM.find_and_raise(v, g, "[ocamlgraph] iter_pred_e")))

  let fold_pred_e = (f, g, v) =>
    S.fold(((w, l)) => f((w, l, v)), fst(HM.find_and_raise(v, g, "[ocamlgraph] fold_pred_e")))

  let pred = (g, v) => fold_pred((w, l) => list{w, ...l}, g, v, list{})
  let pred_e = (g, v) => fold_pred_e((e, l) => list{e, ...l}, g, v, list{})
}

@ocaml.doc(" Build persistent (resp. imperative) graphs from a persistent (resp.
    imperative) association table ")
module Make = (F: TBL_BUILDER) => {
  module Digraph = {
    module Concrete = (V: COMPARABLE) => {
      include ConcreteVertex(F, V)
      include Unlabeled(V, HM)
      include Minimal(S, HM)

      let add_edge = (g, v1, v2) =>
        if mem_edge(g, v1, v2) {
          g
        } else {
          let g = add_vertex(g, v1)
          let g = add_vertex(g, v2)
          unsafe_add_edge(g, v1, v2)
        }

      let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)
    }

    module ConcreteBidirectional = (V: COMPARABLE) => {
      include ConcreteVertex(F, V)
      include BidirectionalUnlabeled(V, HM)
      include BidirectionalMinimal(S, HM)

      let unsafe_add_edge = (g, v1, v2) => {
        let find = (v, g) =>
          try HM.find(v, g) catch {
          | Not_found => (S.empty, S.empty)
          }
        let (in_set, out_set) = find(v1, g)
        let g = HM.add(v1, (in_set, S.add(v2, out_set)), g)
        let (in_set, out_set) = find(v2, g)
        HM.add(v2, (S.add(v1, in_set), out_set), g)
      }

      let add_edge = (g, v1, v2) =>
        if mem_edge(g, v1, v2) {
          g
        } else {
          unsafe_add_edge(g, v1, v2)
        }

      let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)
    }

    module ConcreteLabeled = (V: COMPARABLE, Edge: ORDERED_TYPE_DFT) => {
      include ConcreteVertex(F, V)
      include Labeled(V, Edge, HM)
      include Minimal(S, HM)

      let add_edge_e = (g, (v1, l, v2) as e) =>
        if mem_edge_e(g, e) {
          g
        } else {
          let g = add_vertex(g, v1)
          let g = add_vertex(g, v2)
          unsafe_add_edge(g, v1, (v2, l))
        }

      let add_edge = (g, v1, v2) => add_edge_e(g, (v1, Edge.default, v2))
    }

    module ConcreteBidirectionalLabeled = (V: COMPARABLE, Edge: ORDERED_TYPE_DFT) => {
      include ConcreteVertex(F, V)
      include BidirectionalLabeled(V, Edge, HM)
      include BidirectionalMinimal(S, HM)

      let unsafe_add_edge_e = (g, (v1, l, v2)) => {
        let find = (v, g) =>
          try HM.find(v, g) catch {
          | Not_found => (S.empty, S.empty)
          }
        let (in_set, out_set) = find(v1, g)
        let g = HM.add(v1, (in_set, S.add((v2, l), out_set)), g)
        let (in_set, out_set) = find(v2, g)
        HM.add(v2, (S.add((v1, l), in_set), out_set), g)
      }

      let add_edge_e = (g, e) =>
        if mem_edge_e(g, e) {
          g
        } else {
          unsafe_add_edge_e(g, e)
        }

      let add_edge = (g, v1, v2) => add_edge_e(g, (v1, Edge.default, v2))
    }

    module Abstract = (V: VERTEX) => {
      module G = {
        module V = V
        module HM = F(V)
        include Unlabeled(V, HM)
        include Minimal(S, HM)
      }
      include Make_Abstract(G)
    }

    module AbstractLabeled = (V: VERTEX, E: ORDERED_TYPE_DFT) => {
      module G = {
        module V = V
        module HM = F(V)
        include Labeled(V, E, HM)
        include Minimal(S, HM)
      }
      include Make_Abstract(G)
    }
  }
}

@ocaml.doc(" Implementation of undirected graphs from implementation of directed
    graphs. ")
module Graph = (
  G: {
    include Sig.G
    let create: (~size: int=?, unit) => t
    let clear: t => unit
    let copy: t => t
    type return
    let add_vertex: (t, vertex) => return
    let remove_vertex: (t, vertex) => return
  },
) => {
  include G

  let is_directed = false

  /* Redefine iterators and [nb_edges]. */

  let iter_edges = f =>
    iter_edges((v1, v2) =>
      if V.compare(v1, v2) >= 0 {
        f(v1, v2)
      }
    )

  let fold_edges = f =>
    fold_edges((v1, v2, acc) =>
      if V.compare(v1, v2) >= 0 {
        f(v1, v2, acc)
      } else {
        acc
      }
    )

  let iter_edges_e = f =>
    iter_edges_e(e =>
      if V.compare(E.src(e), E.dst(e)) >= 0 {
        f(e)
      }
    )

  let fold_edges_e = f =>
    fold_edges_e((e, acc) =>
      if V.compare(E.src(e), E.dst(e)) >= 0 {
        f(e, acc)
      } else {
        acc
      }
    )

  let nb_edges = g => fold_edges_e(_ => \"+"(1), g, 0)

  /* Redefine operations on predecessors:
   predecessors are successors in an undirected graph. */

  let pred = succ
  let in_degree = out_degree
  let iter_pred = iter_succ
  let fold_pred = fold_succ
  let pred_e = succ_e
  let iter_pred_e = iter_succ_e
  let fold_pred_e = fold_succ_e
}

/*
Local Variables:
compile-command: "make -C .."
End:
*/
