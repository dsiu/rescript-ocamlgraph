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

/* Basic operations over graphs */

module type S = {
  type g
  let transitive_closure: (~reflexive: bool=?, g) => g
  let add_transitive_closure: (~reflexive: bool=?, g) => g
  let transitive_reduction: (~reflexive: bool=?, g) => g
  let replace_by_transitive_reduction: (~reflexive: bool=?, g) => g
  let mirror: g => g
  let complement: g => g
  let intersect: (g, g) => g
  let union: (g, g) => g
}

module Make = (B: Builder.S) => {
  open B

  /* Roy-Warshall's algorithm */

  type g = G.t

  let add_transitive_closure = (~reflexive=false, g0) => {
    let phi = (v, g) => {
      let g = if reflexive {
        B.add_edge(g, v, v)
      } else {
        g
      }
      G.fold_succ((sv, g) => G.fold_pred((pv, g) => B.add_edge(g, pv, sv), g, v, g), g, v, g)
    }

    G.fold_vertex(phi, g0, g0)
  }

  let transitive_closure = (~reflexive=false, g0) => add_transitive_closure(~reflexive, B.copy(g0))

  let mirror = g =>
    if G.is_directed {
      let g' = G.fold_vertex((v, g') => B.add_vertex(g', v), g, B.empty())

      G.fold_edges_e((e, g') => {
        let v1 = G.E.src(e)
        let v2 = G.E.dst(e)
        B.add_edge_e(g', G.E.create(v2, G.E.label(e), v1))
      }, g, g')
    } else {
      g
    }

  let complement = g => G.fold_vertex((v, g') => G.fold_vertex((w, g') =>
        if G.mem_edge(g, v, w) {
          g'
        } else {
          B.add_edge(g', v, w)
        }
      , g, g'), g, B.empty())

  let intersect = (g1, g2) => G.fold_vertex((v, g) =>
      try {
        let succ = G.succ_e(g2, v)
        G.fold_succ_e((e, g) =>
          if List.exists(e' => G.E.compare(e, e') == 0, succ) {
            B.add_edge_e(g, e)
          } else {
            g
          }
        , g1, v, B.add_vertex(g, v))
      } catch {
      | Invalid_argument(_) => /* [v] not in [g2] */
        g
      }
    , g1, B.empty())

  let union = (g1, g2) => {
    let add = (g1, g2) =>
      /* add the graph [g1] in [g2] */
      G.fold_vertex(
        (v, g) => G.fold_succ_e((e, g) => B.add_edge_e(g, e), g1, v, B.add_vertex(g, v)),
        g1,
        g2,
      )

    add(g1, B.copy(g2))
  }

  let replace_by_transitive_reduction = (~reflexive=false, g0) => {
    /* first compute reachability in g0 using a DFS from each vertex */
    module H = Hashtbl.Make(G.V)
    module D = Traverse.Dfs(G)
    let reachable = H.create(G.nb_vertex(g0))
    let path_from = v => {
      let s = H.create(8)
      H.add(reachable, v, s)
      D.prefix_component(w => H.add(s, w, ()), g0, v)
    }
    G.iter_vertex(path_from, g0)
    let path = (u, v) => H.mem(H.find(reachable, u), v)
    /* then remove redundant edges */
    let phi = (v, g) => {
      let g = if reflexive {
        B.remove_edge(g, v, v)
      } else {
        g
      }
      G.fold_succ((sv, g) => G.fold_succ((sv', g) =>
          if !G.V.equal(sv, sv') && path(sv, sv') {
            B.remove_edge(g, v, sv')
          } else {
            g
          }
        , g, v, g), g, v, g)
    }

    G.fold_vertex(phi, g0, g0)
  }

  let transitive_reduction = (~reflexive=false, g0) =>
    replace_by_transitive_reduction(~reflexive, B.copy(g0))
}

module P = (G: Sig.P) => Make(Builder.P(G))
module I = (G: Sig.I) => Make(Builder.I(G))

module Choose = (
  G: {
    type t
    type vertex
    type edge
    let iter_vertex: (vertex => unit, t) => unit
    let iter_edges_e: (edge => unit, t) => unit
  },
) => {
  exception Found_Vertex(G.vertex)
  let choose_vertex = g =>
    try {
      G.iter_vertex(v => raise(Found_Vertex(v)), g)
      invalid_arg("choose_vertex")
    } catch {
    | Found_Vertex(v) => v
    }

  exception Found_Edge(G.edge)
  let choose_edge = g =>
    try {
      G.iter_edges_e(v => raise(Found_Edge(v)), g)
      invalid_arg("choose_vertex")
    } catch {
    | Found_Edge(v) => v
    }
}

module Neighbourhood = (
  G: {
    type t
    module V: Sig.COMPARABLE
    let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
    let succ: (t, V.t) => list<V.t>
  },
) => {
  module Vertex_Set = Set.Make(G.V)

  let set_from_vertex = (g, v) => G.fold_succ((v', s) =>
      if G.V.equal(v, v') {
        s
      } else {
        Vertex_Set.add(v', s)
      }
    , g, v, Vertex_Set.empty)

  let list_from_vertex = (g, v) => {
    let rec aux = x =>
      switch x {
      | list{} => list{}
      | list{v', ...l} =>
        if G.V.equal(v, v') {
          assert !List.exists(G.V.equal(v), l)
          l
        } else {
          list{v', ...aux(l)}
        }
      }

    aux(G.succ(g, v))
  }

  let set_from_vertices = (g, l) => {
    let fold_left = f => List.fold_left(f, Vertex_Set.empty, l)
    let env_init = fold_left((s, v) => Vertex_Set.add(v, s))
    let add = (x, s) =>
      if Vertex_Set.mem(x, env_init) {
        s
      } else {
        Vertex_Set.add(x, s)
      }

    fold_left((s, v) => G.fold_succ(add, g, v, s))
  }

  let list_from_vertices = (g, l) => Vertex_Set.elements(set_from_vertices(g, l))
}
