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

/* $Id: path.ml,v 1.6 2005-07-18 07:10:35 filliatr Exp $ */

module type G = {
  type t
  module V: Sig.COMPARABLE
  module E: {
    type t
    type label
    let label: t => label
    let src: t => V.t
    let dst: t => V.t
    let create: (V.t, label, V.t) => t
  }
  let iter_vertex: (V.t => unit, t) => unit
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let iter_succ: (V.t => unit, t, V.t) => unit
  let iter_succ_e: (E.t => unit, t, V.t) => unit
  let fold_edges_e: ((E.t, 'a) => 'a, t, 'a) => 'a
  let nb_vertex: t => int
}

@ocaml.doc(" Weight signature for Johnson's algorithm. ")
module type WJ = {
  include Sig.WEIGHT
  @ocaml.doc(" Subtraction of weights. ")
  let sub: (t, t) => t
}

module Dijkstra = (G: G, W: Sig.WEIGHT with type edge = G.E.t) => {
  open G.E

  module H = Hashtbl.Make(G.V)

  module Elt = {
    type t = (W.t, G.V.t, list<G.E.t>)

    /* weights are compared first, and minimal weights come first in the
     queue */
    let compare = ((w1, v1, _), (w2, v2, _)) => {
      let cw = W.compare(w2, w1)
      if cw !== 0 {
        cw
      } else {
        G.V.compare(v1, v2)
      }
    }
  }

  module PQ = Heap.Imperative(Elt)

  let shortest_path = (g, v1, v2) => {
    let visited = H.create(97)
    let dist = H.create(97)
    let q = PQ.create(17)
    let rec loop = () => {
      if PQ.is_empty(q) {
        raise(Not_found)
      }
      let (w, v, p) = PQ.pop_maximum(q)
      if G.V.compare(v, v2) == 0 {
        (List.rev(p), w)
      } else {
        if !H.mem(visited, v) {
          H.add(visited, v, ())
          G.iter_succ_e(e => {
            let ev = dst(e)
            if !H.mem(visited, ev) {
              let dev = W.add(w, W.weight(e))
              let improvement = try W.compare(dev, H.find(dist, ev)) < 0 catch {
              | Not_found => true
              }

              if improvement {
                H.replace(dist, ev, dev)
                PQ.add(q, (dev, ev, list{e, ...p}))
              }
            }
          }, g, v)
        }
        loop()
      }
    }

    PQ.add(q, (W.zero, v1, list{}))
    H.add(dist, v1, W.zero)
    loop()
  }
}

/* The following module is a contribution of Yuto Takei (University of Tokyo) */

module BellmanFord = (G: G, W: Sig.WEIGHT with type edge = G.E.t) => {
  open G.E

  module H = Hashtbl.Make(G.V)

  exception NegativeCycle(list<G.E.t>)

  let all_shortest_paths = (g, vs) => {
    let dist = H.create(97)
    H.add(dist, vs, W.zero)
    let admissible = H.create(97)
    let build_cycle_from = x0 => {
      let rec traverse_parent = (x, ret) => {
        let e = H.find(admissible, x)
        let s = src(e)
        if G.V.equal(s, x0) {
          list{e, ...ret}
        } else {
          traverse_parent(s, list{e, ...ret})
        }
      }

      traverse_parent(x0, list{})
    }

    let find_cycle = x0 => {
      let visited = H.create(97)
      let rec visit = x =>
        if H.mem(visited, x) {
          build_cycle_from(x)
        } else {
          H.add(visited, x, ())
          let e = H.find(admissible, x)
          visit(src(e))
        }

      visit(x0)
    }

    let rec relax = i => {
      let update = G.fold_edges_e((e, x) => {
        let ev1 = src(e)
        let ev2 = dst(e)
        try {
          let dev1 = H.find(dist, ev1)
          let dev2 = W.add(dev1, W.weight(e))
          let improvement = try W.compare(dev2, H.find(dist, ev2)) < 0 catch {
          | Not_found => true
          }

          if improvement {
            H.replace(dist, ev2, dev2)
            H.replace(admissible, ev2, e)
            Some(ev2)
          } else {
            x
          }
        } catch {
        | Not_found => x
        }
      }, g, None)
      switch update {
      | Some(x) =>
        if i === G.nb_vertex(g) {
          raise(NegativeCycle(find_cycle(x)))
        } else {
          relax(i + 1)
        }
      | None => dist
      }
    }

    relax(0)
  }

  let find_negative_cycle_from = (g, vs) =>
    try {
      let _ = all_shortest_paths(g, vs)
      raise(Not_found)
    } catch {
    | NegativeCycle(l) => l
    }

  module Comp = Components.Make(G)

  /* This is rather inefficient implementation. Indeed, for each
     strongly connected component, we run a full Bellman-Ford
     algorithm using one of its vertex as source, taking all edges
     into consideration.  Instead, we could limit ourselves to the
     edges of the component. */
  let find_negative_cycle = g => {
    let rec iter = x =>
      switch x {
      | list{} => raise(Not_found)
      | list{list{x, ..._}, ...cl} =>
        try find_negative_cycle_from(g, x) catch {
        | Not_found => iter(cl)
        }
      | list{list{}, ..._} => assert false
      } /* a component is not empty */

    iter(Comp.scc_list(g))
  }
}

module Johnson = (G: G, W: WJ with type edge = G.E.t) => {
  module HVV = Hashtbl.Make(Util.HTProduct(G.V, G.V))

  module G' = {
    type t = G.t
    module V = {
      type t = New | Old(G.V.t)
      let compare = (v, u) =>
        switch (v, u) {
        | (New, New) => 0
        | (New, Old(_)) => -1
        | (Old(_), New) => 1
        | (Old(v), Old(u)) => G.V.compare(v, u)
        }
      let hash = v =>
        switch v {
        | Old(v) => G.V.hash(v)
        | New => 42
        }
      let equal = (v, u) =>
        switch (v, u) {
        | (New, New) => true
        | (New, Old(_)) | (Old(_), New) => false
        | (Old(v), Old(u)) => G.V.equal(v, u)
        }
    }
    module E = {
      type label = G.E.label
      type t = NewE(V.t) | OldE(G.E.t)
      let src = e =>
        switch e {
        | NewE(_) => V.New
        | OldE(e) => V.Old(G.E.src(e))
        }
      let dst = e =>
        switch e {
        | NewE(v) => v
        | OldE(e) => V.Old(G.E.dst(e))
        }
      let label = e =>
        switch e {
        | NewE(_) => assert false
        | OldE(e) => G.E.label(e)
        }
      let create = (v, l, u) =>
        switch (v, u) {
        | (V.New, V.Old(u)) => NewE(V.Old(u))
        | (V.Old(v), V.Old(u)) => OldE(G.E.create(v, l, u))
        | (_, _) => assert false
        }
    }
    let iter_vertex = (f, g) => {
      f(V.New)
      G.iter_vertex(v => f(V.Old(v)), g)
    }
    let fold_vertex = (f, g, acc) => {
      let acc' = f(V.New, acc)
      G.fold_vertex((v, a) => f(V.Old(v), a), g, acc')
    }
    let iter_succ = (f, g, v) =>
      switch v {
      | V.New => G.iter_vertex(u => f(V.Old(u)), g)
      | V.Old(v) => G.iter_succ(u => f(V.Old(u)), g, v)
      }
    let iter_succ_e = (f, g, v) =>
      switch v {
      | V.New => G.iter_vertex(u => f(E.NewE(V.Old(u))), g)
      | V.Old(v) => G.iter_succ_e(e => f(E.OldE(e)), g, v)
      }
    let fold_edges_e = (f, g, acc) => {
      let acc' = G.fold_vertex((x, _) => f(E.NewE(V.Old(x)), acc), g, acc)

      G.fold_edges_e(edg => {
        let v1 = G.E.src(edg)
        let v2 = G.E.dst(edg)
        let l = G.E.label(edg)
        f(E.create(V.Old(v1), l, V.Old(v2)))
      }, g, acc')
    }
    let nb_vertex = g => G.nb_vertex(g) + 1
  }

  module W' = {
    open G'.E

    type edge = G'.E.t
    type t = W.t
    let zero = W.zero
    let weight = e =>
      switch e {
      | NewE(_) => zero
      | OldE(e) => W.weight(e)
      }
    let compare = W.compare
    let add = W.add
  }

  module BF = BellmanFord(G', W')

  let all_pairs_shortest_paths = g => {
    let pairs_dist = HVV.create(97)
    let bf_res = BF.all_shortest_paths(g, G'.V.New)
    module W'' = {
      type edge = W.edge
      type t = W.t
      let add = W.add
      let sub = W.sub
      let weight = e => {
        let v1 = G.E.src(e)
        let v2 = G.E.dst(e)
        add(W.weight(e), W.sub(BF.H.find(bf_res, G'.V.Old(v1)), BF.H.find(bf_res, G'.V.Old(v2))))
      }
      let compare = W.compare
      let zero = W.zero
    }

    module D = Dijkstra(G, W'')
    G.iter_vertex(v => G.iter_vertex(u =>
        try {
          let (_, d) = D.shortest_path(g, v, u)
          HVV.add(
            pairs_dist,
            (v, u),
            W''.add(d, W''.sub(BF.H.find(bf_res, G'.V.Old(u)), BF.H.find(bf_res, G'.V.Old(v)))),
          )
        } catch {
        | Not_found => ()
        }
      , g), g)
    pairs_dist
  }
}

module Check = (
  G: {
    type t
    module V: Sig.COMPARABLE
    let iter_succ: (V.t => unit, t, V.t) => unit
  },
) => {
  module HV = Hashtbl.Make(G.V)
  module HVV = Hashtbl.Make(Util.HTProduct(G.V, G.V))

  /* the cache contains the path tests already computed */
  type path_checker = {cache: HVV.t<bool>, graph: G.t}

  let create = g => {cache: HVV.create(97), graph: g}

  let check_path = (pc, v1, v2) =>
    try HVV.find(pc.cache, (v1, v2)) catch {
    | Not_found =>
      /* the path is not in cache; we check it with a BFS */
      let visited = HV.create(97)
      let q = Queue.create()
      /* [visited] contains exactly the vertices that have been added to [q] */
      let push = v =>
        if !HV.mem(visited, v) {
          HV.add(visited, v, ())
          Queue.add(v, q)
        }
      let rec loop = () =>
        if Queue.is_empty(q) {
          HVV.add(pc.cache, (v1, v2), false)
          false
        } else {
          let v = Queue.pop(q)
          HVV.add(pc.cache, (v1, v), true)
          if G.V.compare(v, v2) == 0 {
            true
          } else {
            G.iter_succ(push, pc.graph, v)
            loop()
          }
        }

      push(v1)
      loop()
    }
}
