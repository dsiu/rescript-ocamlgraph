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
  module G: G
  module Vertex_Set: Set.S with type elt = G.V.t
  module VSetset: Set.S with type elt = Vertex_Set.t
  let allminsep: G.t => list<Vertex_Set.t>
  let list_of_allminsep: G.t => list<list<G.V.t>>
  let set_of_allminsep: G.t => VSetset.t
}

module Make = (
  G: {
    include G
    @ocaml.doc(" compute the set of connected components of G(V \ l) ")
    let cc: (t, list<V.t>) => list<list<V.t>>
  },
) => {
  module N = Oper.Neighbourhood(G)
  module Vertex_Set: Set.S with type t = N.Vertex_Set.t and type elt = G.V.t = N.Vertex_Set
  module VSetset = Set.Make(N.Vertex_Set)
  /* Use [N.Vertex_Set] instead of [Vertex_Set] in order to avoid an error with
     ocamldoc 4.02. However this change requires to add extra type annotations
     to module [Vertex_Set] above in order to prevent compilation error with
     OCaml <= 4.0 :-(. */

  let initialisation = g => {
    let cc = G.cc(g)
    let neighbourhood = N.list_from_vertex(g)
    let neighbourhoods = N.set_from_vertices(g)
    G.fold_vertex(
      (v, s) =>
        List.fold_left(
          (s, l) => list{neighbourhoods(l), ...s},
          s,
          cc(list{v, ...neighbourhood(v)}),
        ),
      g,
      list{},
    )
  }

  let generation = g => {
    let neighbourhood = N.list_from_vertex(g)
    let neighbourhoods = N.set_from_vertices(g)
    let cc = G.cc(g)
    let rec gen_aux = (seen, bigs, x) =>
      switch x {
      | list{} => bigs
      | list{s, ...tl} =>
        let l = Vertex_Set.elements(s)
        let seen = VSetset.add(s, seen)
        let (bigs, tl) = Vertex_Set.fold((v, _) => {
          let add_neighbourhoods = ((bigs, tl), l) => {
            let s = neighbourhoods(l)
            (
              list{s, ...bigs},
              if VSetset.mem(s, seen) {
                tl
              } else {
                list{s, ...tl}
              },
            )
          }

          List.fold_left(add_neighbourhoods, (bigs, tl), cc(\"@"(l, neighbourhood(v))))
        }, s, (bigs, tl))

        gen_aux(seen, bigs, tl)
      }

    bigs => gen_aux(VSetset.empty, bigs, bigs)
  }

  let allminsep = g => generation(g, initialisation(g))

  let set_of_allminsep = g =>
    List.fold_left((bigs, s) => VSetset.add(s, bigs), VSetset.empty, allminsep(g))

  let list_of_allminsep = g => List.map(Vertex_Set.elements, allminsep(g))
}

module P = (
  G: {
    include G
    let remove_vertex: (t, V.t) => t
  },
) => {
  module G = G
  include Make({
    include G
    let cc = {
      module CC = Components.Make(G)
      (g, l) => {
        let g = List.fold_left(remove_vertex, g, l)
        CC.scc_list(g)
      }
    }
  })
}

module I = (
  G: {
    include G
    module Mark: Sig.MARK with type graph = t and type vertex = V.t
  },
) => {
  module G = G
  include Make({
    include G
    let cc = {
      module CC = Components.Make({
        include G
        let iter_vertex = f =>
          iter_vertex(v =>
            if Mark.get(v) == 0 {
              f(v)
            }
          )
      })

      (g, l) => {
        G.Mark.clear(g)
        List.iter(v => G.Mark.set(v, 1), l)
        CC.scc_list(g)
      }
    }
  })
}
