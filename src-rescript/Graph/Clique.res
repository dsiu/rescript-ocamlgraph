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

module type G = {
  type t
  module V: Sig.COMPARABLE
  let succ: (t, V.t) => list<V.t>
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
}

module Bron_Kerbosch = (G: G) => {
  let rec bron_kerbosch = (cliquelst, graph, clique, candidates, used) =>
    switch (candidates, used) {
    | (list{}, list{}) => list{clique, ...cliquelst}
    | (list{}, _) => cliquelst
    | (c, u) =>
      let (_, _, cliques) = List.fold_left(((c, u, acc), v) => {
        /* Get the neighbors ignoring self-loops */
        let n = List.filter(nb => !G.V.equal(nb, v), G.succ(graph, v))
        let c' = List.filter(cv => List.exists(v => G.V.equal(v, cv), n), c)
        let u' = List.filter(cv => List.exists(v => G.V.equal(v, cv), n), u)
        let c_minus_v = List.filter(cv => !G.V.equal(cv, v), c)

        (c_minus_v, list{v, ...u}, bron_kerbosch(acc, graph, list{v, ...clique}, c', u'))
      }, (c, u, list{}), c)
      \"@"(cliques, cliquelst)
    }

  let maximalcliques = g => {
    let vertices = G.fold_vertex((v, acc) => list{v, ...acc}, g, list{})
    bron_kerbosch(list{}, g, list{}, vertices, list{})
  }
}
