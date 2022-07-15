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
  let iter_vertex: (V.t => unit, t) => unit
  let iter_succ: (V.t => unit, t, V.t) => unit
}

module Make = (G: G) => {
  module H = Hashtbl.Make(G.V)

  /* iterative code using a stack (variable [cont] below) */

  type action =
    | Finish(G.V.t, int)
    | Visit(G.V.t, G.V.t)
    | Test(G.V.t, G.V.t)

  let scc = g => {
    let root = H.create(997)
    let hashcomp = H.create(997)
    let stack = ref(list{})
    let numdfs = ref(0)
    let numcomp = ref(0)
    let rec pop = (x, ll) =>
      switch ll {
      | list{(y: int, w), ...l} if y > x =>
        H.add(hashcomp, w, numcomp.contents)
        pop(x, l)
      | l => l
      }

    let cont = ref(list{})
    let visit = v =>
      if !H.mem(root, v) {
        let n = {
          incr(numdfs)
          numdfs.contents
        }
        H.add(root, v, n)
        cont := list{Finish(v, n), ...cont.contents}
        G.iter_succ(w => cont := list{Visit(v, w), Test(v, w), ...cont.contents}, g, v)
      }

    let rec finish = () =>
      switch cont.contents {
      | list{} => ()
      | list{action, ...tail} =>
        cont := tail
        switch action {
        | Finish(v, n) =>
          if H.find(root, v) == n {
            H.add(hashcomp, v, numcomp.contents)
            let s = pop(n, stack.contents)
            stack := s
            incr(numcomp)
          } else {
            stack := list{(n, v), ...stack.contents}
          }
        | Visit(_, w) => visit(w)
        | Test(v, w) =>
          if !H.mem(hashcomp, w) {
            H.replace(root, v, min(H.find(root, v), H.find(root, w)))
          }
        }
        finish()
      }

    let visit_and_finish = v => {
      visit(v)
      finish()
    }

    G.iter_vertex(visit_and_finish, g)
    (numcomp.contents, v => H.find(hashcomp, v))
  }

  let scc_array = g => {
    let (n, f) = scc(g)
    let t = Array.make(n, list{})
    G.iter_vertex(v => {
      let i = f(v)
      t[i] = list{v, ...t[i]}
    }, g)
    t
  }

  let scc_list = g => {
    let a = scc_array(g)
    Array.fold_right((l, acc) => list{l, ...acc}, a, list{})
  }
}

@@ocaml.text(" Connectivity in strongly connected directed graphs ")

module Connectivity = (GB: Builder.S) => {
  module MOper = Oper.Make(GB)
  module Choose = Oper.Choose(GB.G)
  module Dom = Dominator.Make(GB.G)

  module S = Dom.S

  let sstrong_articulation_points = g => {
    let s = Choose.choose_vertex(g)
    module SCC = Make({
      include GB.G
      let iter_vertex = f =>
        GB.G.iter_vertex(v =>
          if !V.equal(s, v) {
            f(v)
          }
        )
      let iter_succ = f =>
        GB.G.iter_succ(v =>
          if !V.equal(s, v) {
            f(v)
          }
        )
    })

    let s_is_sap = fst(SCC.scc(g)) > 1
    let dt_s = {
      open Dom
      idom_to_dom_tree(g, compute_idom(g, s))
    }
    let d_s = Dom.dom_tree_to_snontrivial_dom(s, dt_s)
    let g_r = MOper.mirror(g)
    let dtr_s = {
      open Dom
      idom_to_dom_tree(g_r, compute_idom(g_r, s))
    }
    let dr_s = Dom.dom_tree_to_snontrivial_dom(s, dtr_s)
    let d = Dom.S.union(d_s, dr_s)
    if s_is_sap {
      Dom.S.add(s, d)
    } else {
      d
    }
  }

  let strong_articulation_points = g => S.elements(sstrong_articulation_points(g))
}

module BiConnectivity = (G: Sig.G) => {
  module Choose = Oper.Choose(G)
  module Dom = Dominator.Make(G)
  module RDom = Dominator.Make({
    type t = G.t
    module V = G.V
    let pred = G.succ
    let succ = G.pred
    let fold_vertex = G.fold_vertex
    let iter_vertex = G.iter_vertex
    let iter_succ = G.iter_pred
    let nb_vertex = G.nb_vertex
  })

  module S = Dom.S

  let sstrong_articulation_points = g => {
    let s = Choose.choose_vertex(g)
    module SCC = Make({
      include G
      let iter_vertex = f =>
        G.iter_vertex(v =>
          if !V.equal(s, v) {
            f(v)
          }
        )
      let iter_succ = f =>
        G.iter_succ(v =>
          if !V.equal(s, v) {
            f(v)
          }
        )
    })

    let s_is_sap = fst(SCC.scc(g)) > 1
    let dt_s = {
      open Dom
      idom_to_dom_tree(g, compute_idom(g, s))
    }
    let d_s = Dom.dom_tree_to_snontrivial_dom(s, dt_s)
    let dtr_s = {
      open RDom
      idom_to_dom_tree(g, compute_idom(g, s))
    }
    let dr_s = Dom.dom_tree_to_snontrivial_dom(s, dtr_s)
    let d = Dom.S.union(d_s, dr_s)
    if s_is_sap {
      Dom.S.add(s, d)
    } else {
      d
    }
  }

  let strong_articulation_points = g => S.elements(sstrong_articulation_points(g))
}

@@ocaml.text(" Connected components (for undirected graphs) ")

module type U = {
  type t
  module V: Sig.COMPARABLE
  let iter_vertex: (V.t => unit, t) => unit
  let iter_edges: ((V.t, V.t) => unit, t) => unit
}

module Undirected = (G: U) => {
  module UF = Unionfind.Make(G.V)
  module H = Hashtbl.Make(G.V)

  let components = g => {
    let vertices = ref(list{})
    G.iter_vertex(v => vertices := list{v, ...vertices.contents}, g)
    let uf = UF.init(vertices.contents)
    let visit = (u, v) => UF.union(u, v, uf)
    G.iter_edges(visit, g)
    let count = ref(0)
    let comp = H.create(5003)
    let visit = v => {
      let v = UF.find(v, uf)
      if !H.mem(comp, v) {
        H.add(comp, v, count.contents)
        incr(count)
      }
    }
    G.iter_vertex(visit, g)
    (count.contents, v => H.find(comp, UF.find(v, uf)))
  }

  let components_array = g => {
    let (n, f) = components(g)
    let t = Array.make(n, list{})
    G.iter_vertex(v => {
      let i = f(v)
      t[i] = list{v, ...t[i]}
    }, g)
    t
  }

  let components_list = g => {
    let a = components_array(g)
    Array.fold_right((l, acc) => list{l, ...acc}, a, list{})
  }
}
