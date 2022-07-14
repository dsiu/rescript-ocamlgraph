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

open Sig
open Blocks

module Stdlib = Pervasives

module type S = {
  @ocaml.doc(" Imperative Unlabeled Graphs ")
  module Concrete: (V: COMPARABLE) =>
  (
    Sig.I
      with type V.t = V.t
      and type V.label = V.t
      and type E.t = (V.t, V.t)
      and type E.label = unit
  )

  @ocaml.doc(" Abstract Imperative Unlabeled Graphs ")
  module Abstract: (
    V: {
      type t
    },
  ) => (Sig.IM with type V.label = V.t and type E.label = unit and type E.label = unit)

  @ocaml.doc(" Imperative Labeled Graphs ")
  module ConcreteLabeled: (V: COMPARABLE, E: ORDERED_TYPE_DFT) =>
  (
    Sig.I
      with type V.t = V.t
      and type V.label = V.t
      and type E.t = (V.t, E.t, V.t)
      and type E.label = E.t
  )

  @ocaml.doc(" Abstract Imperative Labeled Graphs ")
  module AbstractLabeled: (
    V: {
      type t
    },
    E: ORDERED_TYPE_DFT,
  ) => (Sig.IM with type V.label = V.t and type E.label = E.t)
}

module I = Make(Make_Hashtbl)

type abstract_vertex<'a> = {tag: int, label: 'a, mutable mark: int}

/* Implement module type [MARK]. */
module Make_Mark = (
  X: {
    type graph
    type label
    let iter_vertex: (abstract_vertex<label> => unit, graph) => unit
  },
) => {
  type vertex = abstract_vertex<X.label>
  type graph = X.graph
  let get = v => v.mark
  let set = (v, m) => v.mark = m
  let clear = g => X.iter_vertex(v => set(v, 0), g)
}

/* Vertex for abstract imperative graphs:
 comparing to vertex for abstract **persistent** graphs, marks are added. */
module AbstractVertex = (
  V: {
    type t
  },
) => {
  type label = V.t
  type t = abstract_vertex<label>
  let compare = (x, y) => Stdlib.compare(x.tag, y.tag)
  let hash = x => x.tag
  let equal = (x, y) => x.tag == y.tag
  let label = x => x.label
  let create = l => {
    if cpt_vertex.contents == first_value_for_cpt_vertex - 1 {
      invalid_arg("Too much vertices")
    }
    incr(cpt_vertex)
    {tag: cpt_vertex.contents, label: l, mark: 0}
  }
}

module Digraph = {
  module Concrete = (V: COMPARABLE) => {
    include I.Digraph.Concrete(V)
    let add_vertex = (g, v) => ignore(add_vertex(g, v))
    let add_edge = (g, v1, v2) => ignore(add_edge(g, v1, v2))
    let remove_edge = (g, v1, v2) => ignore(remove_edge(g, v1, v2))
    let remove_edge_e = (g, e) => ignore(remove_edge_e(g, e))
    let add_edge_e = (g, e) => ignore(add_edge_e(g, e))
    let remove_vertex = (g, v) =>
      if HM.mem(v, g) {
        ignore(HM.remove(v, g))
        HM.iter((k, s) => ignore(HM.add(k, S.remove(v, s), g)), g)
      }
  }

  module ConcreteLabeled = (V: COMPARABLE, E: ORDERED_TYPE_DFT) => {
    include I.Digraph.ConcreteLabeled(V, E)
    let add_vertex = (g, v) => ignore(add_vertex(g, v))
    let remove_edge = (g, v1, v2) => ignore(remove_edge(g, v1, v2))
    let remove_edge_e = (g, e) => ignore(remove_edge_e(g, e))
    let add_edge_e = (g, e) => ignore(add_edge_e(g, e))
    let add_edge = (g, v1, v2) => ignore(add_edge(g, v1, v2))
    let remove_vertex = (g, v) =>
      if HM.mem(v, g) {
        ignore(HM.remove(v, g))
        let remove = v => S.filter(((v2, _)) => !V.equal(v, v2))
        HM.iter((k, s) => ignore(HM.add(k, remove(v, s), g)), g)
      }
  }

  module ConcreteBidirectional = (V: COMPARABLE) => {
    include I.Digraph.ConcreteBidirectional(V)

    let add_vertex = (g, v) => ignore(add_vertex(g, v))
    let add_edge = (g, v1, v2) => ignore(add_edge(g, v1, v2))
    let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)

    let remove_edge = (g, v1, v2) => ignore(remove_edge(g, v1, v2))
    let remove_edge_e = (g, e) => ignore(remove_edge_e(g, e))

    let remove_vertex = (g, v) =>
      if HM.mem(v, g) {
        iter_pred_e(e => remove_edge_e(g, e), g, v)
        iter_succ_e(e => remove_edge_e(g, e), g, v)
        ignore(HM.remove(v, g))
      }
  }

  module ConcreteBidirectionalLabeled = (V: COMPARABLE, E: ORDERED_TYPE_DFT) => {
    include I.Digraph.ConcreteBidirectionalLabeled(V, E)

    let add_vertex = (g, v) => ignore(add_vertex(g, v))
    let add_edge = (g, v1, v2) => ignore(add_edge(g, v1, v2))
    let add_edge_e = (g, (v1, l, v2)) => ignore(add_edge_e(g, (v1, l, v2)))

    let remove_edge = (g, v1, v2) => ignore(remove_edge(g, v1, v2))
    let remove_edge_e = (g, e) => ignore(remove_edge_e(g, e))

    let remove_vertex = (g, v) =>
      if HM.mem(v, g) {
        iter_pred_e(e => remove_edge_e(g, e), g, v)
        iter_succ_e(e => remove_edge_e(g, e), g, v)
        ignore(HM.remove(v, g))
      }
  }

  module Abstract = (
    V: {
      type t
    },
  ) => {
    include I.Digraph.Abstract(AbstractVertex(V))

    let add_vertex = (g, v) =>
      if !HM.mem(v, g.edges) {
        g.size = Stdlib.succ(g.size)
        ignore(G.unsafe_add_vertex(g.edges, v))
      }

    let add_edge = (g, v1, v2) => {
      add_vertex(g, v1)
      add_vertex(g, v2)
      ignore(unsafe_add_edge(g.edges, v1, v2))
    }

    let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)

    let remove_vertex = (g, v) =>
      if HM.mem(v, g.edges) {
        let e = g.edges
        ignore(HM.remove(v, e))
        HM.iter((k, s) => ignore(HM.add(k, S.remove(v, s), e)), e)
        g.size = Stdlib.pred(g.size)
      }

    module Mark = Make_Mark({
      type graph = t
      type label = V.label
      let iter_vertex = iter_vertex
    })

    let remove_edge = (g, v1, v2) => ignore(remove_edge(g, v1, v2))
    let remove_edge_e = (g, e) => ignore(remove_edge_e(g, e))
  }

  module AbstractLabeled = (
    V: {
      type t
    },
    Edge: ORDERED_TYPE_DFT,
  ) => {
    include I.Digraph.AbstractLabeled(AbstractVertex(V), Edge)

    let add_vertex = (g, v) =>
      if !HM.mem(v, g.edges) {
        g.size = Stdlib.succ(g.size)
        ignore(G.unsafe_add_vertex(g.edges, v))
      }

    let add_edge_e = (g, (v1, l, v2)) => {
      add_vertex(g, v1)
      add_vertex(g, v2)
      ignore(unsafe_add_edge(g.edges, v1, (v2, l)))
    }

    let add_edge = (g, v1, v2) => add_edge_e(g, (v1, Edge.default, v2))

    let remove_vertex = (g, v) =>
      if HM.mem(v, g.edges) {
        let remove = s => S.fold(((v2, _) as e, s) =>
            if !V.equal(v, v2) {
              S.add(e, s)
            } else {
              s
            }
          , s, S.empty)

        let e = g.edges
        ignore(HM.remove(v, e))
        HM.iter((k, s) => ignore(HM.add(k, remove(s), e)), e)
        g.size = Stdlib.pred(g.size)
      }

    module Mark = Make_Mark({
      type graph = t
      type label = V.label
      let iter_vertex = iter_vertex
    })

    let remove_edge = (g, v1, v2) => ignore(remove_edge(g, v1, v2))
    let remove_edge_e = (g, e) => ignore(remove_edge_e(g, e))
  }
}

module Graph = {
  module Concrete = (V: COMPARABLE) => {
    module G = {
      include Digraph.Concrete(V)
      type return = unit
    }
    include Blocks.Graph(G)

    /* Redefine the [add_edge] and [remove_edge] operations */

    let add_edge = (g, v1, v2) =>
      if !mem_edge(g, v1, v2) {
        G.add_edge(g, v1, v2)
        assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
        ignore(G.unsafe_add_edge(g, v2, v1))
      }

    let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)

    let remove_edge = (g, v1, v2) => {
      G.remove_edge(g, v1, v2)
      assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
      ignore(G.unsafe_remove_edge(g, v2, v1))
    }

    let remove_edge_e = (g, (v1, v2)) => remove_edge(g, v1, v2)
  }

  module ConcreteLabeled = (V: COMPARABLE, Edge: ORDERED_TYPE_DFT) => {
    module G = {
      include Digraph.ConcreteLabeled(V, Edge)
      type return = unit
    }
    include Blocks.Graph(G)

    /* Redefine the [add_edge] and [remove_edge] operations */

    let add_edge_e = (g, (v1, l, v2) as e) =>
      if !mem_edge_e(g, e) {
        G.add_edge_e(g, e)
        assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
        ignore(G.unsafe_add_edge(g, v2, (v1, l)))
      }

    let add_edge = (g, v1, v2) => add_edge_e(g, (v1, Edge.default, v2))

    let remove_edge = (g, v1, v2) => {
      G.remove_edge(g, v1, v2)
      assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
      ignore(G.unsafe_remove_edge(g, v2, v1))
    }

    let remove_edge_e = (g, (v1, l, v2) as e) => {
      G.remove_edge_e(g, e)
      assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
      ignore(G.unsafe_remove_edge_e(g, (v2, l, v1)))
    }
  }

  module Abstract = (
    V: {
      type t
    },
  ) => {
    module G = {
      include Digraph.Abstract(V)
      type return = unit
    }
    include Blocks.Graph(G)

    /* Export some definitions of [G] */
    module Mark = G.Mark

    /* Redefine the [add_edge] and [remove_edge] operations */

    let add_edge = (g, v1, v2) => {
      G.add_edge(g, v1, v2)
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      ignore(G.unsafe_add_edge(g.G.edges, v2, v1))
    }

    let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)

    let remove_edge = (g, v1, v2) => {
      G.remove_edge(g, v1, v2)
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      ignore(G.unsafe_remove_edge(g.G.edges, v2, v1))
    }

    let remove_edge_e = (g, (v1, v2)) => remove_edge(g, v1, v2)
  }

  module AbstractLabeled = (
    V: {
      type t
    },
    Edge: ORDERED_TYPE_DFT,
  ) => {
    module G = {
      include Digraph.AbstractLabeled(V, Edge)
      type return = unit
    }
    include Blocks.Graph(G)

    /* Export some definitions of [G] */
    module Mark = G.Mark

    /* Redefine the [add_edge] and [remove_edge] operations */

    let add_edge_e = (g, (v1, l, v2) as e) => {
      G.add_edge_e(g, e)
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      ignore(G.unsafe_add_edge(g.G.edges, v2, (v1, l)))
    }

    let add_edge = (g, v1, v2) => add_edge_e(g, (v1, Edge.default, v2))

    let remove_edge = (g, v1, v2) => {
      G.remove_edge(g, v1, v2)
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      ignore(G.unsafe_remove_edge(g.G.edges, v2, v1))
    }

    let remove_edge_e = (g, (v1, l, v2) as e) => {
      ignore(G.remove_edge_e(g, e))
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      ignore(G.unsafe_remove_edge_e(g.G.edges, (v2, l, v1)))
    }
  }
}

module Matrix = {
  module type S = {
    include Sig.I with type V.t = int and type V.label = int and type E.t = (int, int)
    let make: int => t
  }

  module Digraph = {
    module V = {
      type t = int
      type label = int
      let compare: (t, t) => int = Stdlib.compare
      let hash = Hashtbl.hash
      let equal = \"=="
      let create = i => i
      let label = i => i
    }

    module E = {
      type t = (V.t, V.t)
      type vertex = V.t
      let compare: (t, t) => int = Stdlib.compare
      type label = unit
      let create = (v1, _, v2) => (v1, v2)
      let src = fst
      let dst = snd
      let label = _ => ()
    }

    type t = array<Bitv.t>
    type vertex = V.t
    type edge = E.t

    let create = (~size as _=?, ()) =>
      failwith("[ocamlgraph] do not use Matrix.create; please use Matrix.make instead")

    let make = n => {
      if n < 0 {
        invalid_arg("[ocamlgraph] Matrix.make")
      }
      Array.init(n, _ => Bitv.create(n, false))
    }

    let is_directed = true

    let nb_vertex = Array.length
    let is_empty = g => nb_vertex(g) == 0
    let nb_edges = Array.fold_left(
      Bitv.fold_left((n, b) =>
        if b {
          n + 1
        } else {
          n
        }
      ),
      0,
    )

    let mem_vertex = (g, v) => 0 <= v && v < nb_vertex(g)
    let mem_edge = (g, i, j) => Bitv.get(g[i], j)
    let mem_edge_e = (g, (i, j)) => Bitv.get(g[i], j)
    let find_edge = (g, i, j) =>
      if mem_edge(g, i, j) {
        (i, j)
      } else {
        raise(Not_found)
      }
    let find_all_edges = (g, i, j) =>
      try list{find_edge(g, i, j)} catch {
      | Not_found => list{}
      }

    /* constructors */
    let add_edge = (g, i, j) => Bitv.set(g[i], j, true)
    let add_edge_e = (g, (i, j)) => Bitv.set(g[i], j, true)

    let remove_edge = (g, i, j) => Bitv.set(g[i], j, false)
    let remove_edge_e = (g, (i, j)) => Bitv.set(g[i], j, false)

    let unsafe_add_edge = (g, i, j) => Bitv.unsafe_set(Array.unsafe_get(g, i), j, true)
    let unsafe_remove_edge = (g, i, j) => Bitv.unsafe_set(Array.unsafe_get(g, i), j, false)

    let remove_vertex = (_, _) => ()
    let add_vertex = (_, _) => ()

    let clear = g => Array.iter(b => Bitv.iteri((j, _) => Bitv.set(b, j, false), b), g)

    let copy = g => Array.init(nb_vertex(g), i => Bitv.copy(g[i]))

    /* iter/fold on all vertices/edges of a graph */
    let iter_vertex = (f, g) =>
      for i in 0 to nb_vertex(g) - 1 {
        f(i)
      }

    let iter_edges = (f, g) =>
      for i in 0 to nb_vertex(g) - 1 {
        Bitv.iteri((j, b) =>
          if b {
            f(i, j)
          }
        , g[i])
      }

    let fold_vertex = (f, g, a) => {
      let n = nb_vertex(g)
      let rec fold = (i, a) =>
        if i == n {
          a
        } else {
          fold(i + 1, f(i, a))
        }
      fold(0, a)
    }

    let fold_edges = (f, g, a) => fold_vertex((i, a) => Bitv.foldi_right((j, b, a) =>
          if b {
            f(i, j, a)
          } else {
            a
          }
        , g[i], a), g, a)

    /* successors and predecessors of a vertex */
    let succ = (g, i) => Bitv.foldi_left((l, j, b) =>
        if b {
          list{j, ...l}
        } else {
          l
        }
      , list{}, g[i])

    let pred = (g, i) => fold_vertex((j, a) =>
        if Bitv.unsafe_get(g[j], i) {
          list{j, ...a}
        } else {
          a
        }
      , g, list{})

    /* iter/fold on all successor/predecessor of a vertex. */
    let iter_succ = (f, g, i) => {
      let si = g[i]
      for j in 0 to nb_vertex(g) - 1 {
        if Bitv.unsafe_get(si, j) {
          f(j)
        }
      }
    }
    /* optimization w.r.t.
       [Bitv.iteri (fun j b -> if b then f j) g.(i)]
 */

    let iter_pred = (f, g, i) =>
      for j in 0 to nb_vertex(g) - 1 {
        if Bitv.unsafe_get(g[j], i) {
          f(j)
        }
      }

    let fold_succ = (f, g, i, a) => Bitv.foldi_right((j, b, a) =>
        if b {
          f(j, a)
        } else {
          a
        }
      , g[i], a)

    let fold_pred = (f, g, i, a) => fold_vertex((j, a) =>
        if Bitv.unsafe_get(g[j], i) {
          f(j, a)
        } else {
          a
        }
      , g, a)

    /* degree */
    let out_degree = (g, i) => fold_succ((_, n) => n + 1, g, i, 0)

    let in_degree = (g, i) => fold_pred((_, n) => n + 1, g, i, 0)

    /* map iterator on vertex */
    let map_vertex = (f, g) => {
      let n = nb_vertex(g)
      let f = i => {
        /* ensures f is applied exactly once for each vertex */
        let fi = f(i)
        if fi < 0 || fi >= n {
          invalid_arg("[ocamlgraph] map_vertex")
        }
        fi
      }
      let v = Array.init(n, f)
      let g' = make(n)
      iter_edges((i, j) => Bitv.unsafe_set(g'[v[i]], v[j], true), g)
      g'
    }

    /* labeled edges going from/to a vertex */
    /* successors and predecessors of a vertex */
    let succ_e = (g, i) => Bitv.foldi_left((l, j, b) =>
        if b {
          list{(i, j), ...l}
        } else {
          l
        }
      , list{}, g[i])

    let pred_e = (g, i) => fold_vertex((j, a) =>
        if Bitv.unsafe_get(g[j], i) {
          list{(j, i), ...a}
        } else {
          a
        }
      , g, list{})

    /* iter/fold on all labeled edges of a graph */
    let iter_edges_e = (f, g) =>
      for i in 0 to nb_vertex(g) - 1 {
        Bitv.iteri((j, b) =>
          if b {
            f((i, j))
          }
        , g[i])
      }

    let fold_edges_e = (f, g, a) => fold_vertex((i, a) => Bitv.foldi_right((j, b, a) =>
          if b {
            f((i, j), a)
          } else {
            a
          }
        , g[i], a), g, a)

    /* iter/fold on all edges going from/to a vertex */
    let iter_succ_e = (f, g, i) => {
      let si = g[i]
      for j in 0 to nb_vertex(g) - 1 {
        if Bitv.unsafe_get(si, j) {
          f((i, j))
        }
      }
    }

    let iter_pred_e = (f, g, i) =>
      for j in 0 to nb_vertex(g) - 1 {
        if Bitv.unsafe_get(g[j], i) {
          f((j, i))
        }
      }

    let fold_succ_e = (f, g, i, a) => Bitv.foldi_right((j, b, a) =>
        if b {
          f((i, j), a)
        } else {
          a
        }
      , g[i], a)

    let fold_pred_e = (f, g, i, a) => fold_vertex((j, a) =>
        if Bitv.unsafe_get(g[j], i) {
          f((j, i), a)
        } else {
          a
        }
      , g, a)
  }

  module Graph = {
    module G = {
      include Digraph
      type return = unit
    }
    include Blocks.Graph(G)

    /* Export some definitions of [G] */
    let make = G.make

    /* Redefine the [add_edge] and [remove_edge] operations */

    let add_edge = (g, v1, v2) => {
      G.add_edge(g, v1, v2)
      ignore(G.unsafe_add_edge(g, v2, v1))
    }

    let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)

    let remove_edge = (g, v1, v2) => {
      G.remove_edge(g, v1, v2)
      ignore(G.unsafe_remove_edge(g, v2, v1))
    }

    let remove_edge_e = (g, (v1, v2)) => remove_edge(g, v1, v2)
  }
}

/* Faster implementations when vertices are not shared between graphs. */
/* ***
   module UV = struct

   let cpt_vertex = ref min_int

   type ('label, 'succ) vertex = {
    tag : int;
    label : 'label;
    mutable mark : int;
    mutable succ : 'succ;
   }

   module Digraph = struct

    module Abstract(L: ANY_TYPE) :
      Sig.IM with type V.label = L.t and type E.label = unit
    =
    struct

      module rec V :
        VERTEX with type label = L.t and type t = (L.t, S.t) vertex  =
      struct
   type label = L.t
   type t = (L.t, S.t) vertex

   let compare x y = compare x.tag y.tag
   let hash x = Hashtbl.hash x.tag
   let equal x y = x.tag = y.tag
   let label x = x.label

   let create l =
    assert (!cpt_vertex < max_int);
    incr cpt_vertex;
    { tag = !cpt_vertex; label = l; mark = 0; succ = S.empty }
      end
      and S : Set.S with type elt = V.t = Set.Make(V)

      type vertex = V.t

      module E = struct
   type t = V.t * V.t
   type vertex = V.t
   let compare = Stdlib.compare
   type label = unit
   let create v1 _ v2 = (v1, v2)
   let src = fst
   let dst = snd
   let label _ = ()
      end

      type edge = E.t

      type t = {
   mutable vertices : S.t;
      }

      let create ?size () = { vertices = S.empty }
      let is_directed = true
      let is_empty g = S.is_empty g.vertices
      let nb_vertex g = S.cardinal g.vertices
      let out_degree _ v = S.cardinal v.succ
      let clear g = g.vertices <- S.empty

      let add_vertex g v = g.vertices <- S.add v g.vertices
      let mem_vertex g v = S.mem v g.vertices
      let iter_vertex f g = S.iter f g.vertices
      let fold_vertex f g = S.fold f g.vertices
      let succ _ v = S.elements v.succ
      let succ_e _ v = List.map (fun w -> (v, w)) (S.elements v.succ)
      let iter_succ f _ v = S.iter f v.succ
      let iter_succ_e f _ v = S.iter (fun w -> f (v, w)) v.succ
      let fold_succ f _ v acc = S.fold f v.succ acc
      let fold_succ_e f _ v acc = S.fold (fun w acc -> f (v, w) acc) v.succ acc

      let add_edge _ v1 v2 = v1.succ <- S.add v2 v1.succ
      let add_edge_e g (v1, v2) = add_edge g v1 v2
      let mem_edge _ v1 v2 = S.mem v2 v1.succ
      let mem_edge_e g (v1, v2) = mem_edge g v1 v2
      let remove_edge _ v1 v2 = v1.succ <- S.remove v2 v1.succ
      let remove_edge_e g (v1, v2) = remove_edge g v1 v2
      let nb_edges g = fold_vertex (fun v n -> n + S.cardinal v.succ) g 0

      let find_edge g i j = if mem_edge g i j then i, j else raise Not_found
      let find_all_edges g i j = try [ find_edge g i j ] with Not_found -> []

      module Mark = struct
        type graph = t
        type vertex = V.t
        let clear g = S.iter (fun v -> v.mark <- 0) g.vertices
        let get v = v.mark
        let set v m = v.mark <- m
      end
    end

    module AbstractLabeled (V: ANY_TYPE)(E: ORDERED_TYPE_DFT) :
      Sig.IM with type V.label = V.t and type E.label = E.t
    =
    AbstractLabeled
      (V)(struct type t = unit let compare _ _ = 0 let default = () end)

   end

   (**
   module Graph = struct

    module Abstract(V: ANY_TYPE) :
      Sig.IM with type V.label = V.t and type E.label = unit

    module AbstractLabeled (V: ANY_TYPE)(E: ORDERED_TYPE_DFT) :
      Sig.IM with type V.label = V.t and type E.label = E.t

   end
 **)
   end
 ****/

/*
Local Variables:
compile-command: "make -C .."
End:
*/
