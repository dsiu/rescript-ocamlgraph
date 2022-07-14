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
  @ocaml.doc(" Persistent Unlabeled Graphs ")
  module Concrete: (V: COMPARABLE) =>
  (
    Sig.P
      with type V.t = V.t
      and type V.label = V.t
      and type E.t = (V.t, V.t)
      and type E.label = unit
  )

  @ocaml.doc(" Abstract Persistent Unlabeled Graphs ")
  module Abstract: (
    V: {
      type t
    },
  ) => (Sig.P with type V.label = V.t and type E.label = unit)

  @ocaml.doc(" Persistent Labeled Graphs ")
  module ConcreteLabeled: (V: COMPARABLE, E: ORDERED_TYPE_DFT) =>
  (
    Sig.P
      with type V.t = V.t
      and type V.label = V.t
      and type E.t = (V.t, E.t, V.t)
      and type E.label = E.t
  )

  @ocaml.doc(" Abstract Persistent Labeled Graphs ")
  module AbstractLabeled: (
    V: {
      type t
    },
    E: ORDERED_TYPE_DFT,
  ) => (Sig.P with type V.label = V.t and type E.label = E.t)
}

module P = Make(Make_Map)

type abstract_vertex<'a> = {tag: int, label: 'a}

/* Vertex for the abstract persistent graphs. */
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
    {tag: cpt_vertex.contents, label: l}
  }
}

module Digraph = {
  module Concrete = (V: COMPARABLE) => {
    include P.Digraph.Concrete(V)
    let remove_vertex = (g, v) =>
      if HM.mem(v, g) {
        let g = HM.remove(v, g)
        HM.fold((k, s) => HM.add(k, S.remove(v, s)), g, empty)
      } else {
        g
      }
  }

  module ConcreteLabeled = (V: COMPARABLE, E: ORDERED_TYPE_DFT) => {
    include P.Digraph.ConcreteLabeled(V, E)
    let remove_vertex = (g, v) =>
      if HM.mem(v, g) {
        let g = HM.remove(v, g)
        let remove = v => S.filter(((v2, _)) => !V.equal(v, v2))
        HM.fold((k, s) => HM.add(k, remove(v, s)), g, empty)
      } else {
        g
      }
  }

  module ConcreteBidirectional = (V: COMPARABLE) => {
    include P.Digraph.ConcreteBidirectional(V)
    let remove_vertex = (g, v) =>
      if HM.mem(v, g) {
        let remove = v => S.filter(v' => !V.equal(v, v'))
        let g = fold_pred((v', acc) => {
          let (in_set, out_set) = HM.find(v', acc)
          HM.add(v', (in_set, remove(v, out_set)), acc)
        }, g, v, g)

        let g = fold_succ((v', acc) => {
          let (in_set, out_set) = HM.find(v', acc)
          HM.add(v', (remove(v, in_set), out_set), acc)
        }, g, v, g)

        HM.remove(v, g)
      } else {
        g
      }
  }

  module ConcreteBidirectionalLabeled = (V: COMPARABLE, E: ORDERED_TYPE_DFT) => {
    include P.Digraph.ConcreteBidirectionalLabeled(V, E)
    let remove_vertex = (g: t, v: vertex) =>
      if HM.mem(v, g) {
        let remove = v => S.filter(((v', _)) => !V.equal(v, v'))
        let g = fold_pred((v', acc) => {
          let (in_set, out_set) = HM.find(v', acc)
          HM.add(v', (in_set, remove(v, out_set)), acc)
        }, g, v, g)

        let g = fold_succ((v', acc) => {
          let (in_set, out_set) = HM.find(v', acc)
          HM.add(v', (remove(v, in_set), out_set), acc)
        }, g, v, g)

        HM.remove(v, g)
      } else {
        g
      }
  }

  module Abstract = (
    V: {
      type t
    },
  ) => {
    include P.Digraph.Abstract(AbstractVertex(V))

    let empty = {edges: G.empty, size: 0}

    let add_vertex = (g, v) =>
      if mem_vertex(g, v) {
        g
      } else {
        {
          edges: G.unsafe_add_vertex(g.edges, v),
          size: Stdlib.succ(g.size),
        }
      }

    let add_edge = (g, v1, v2) => {
      let g = add_vertex(g, v1)
      let g = add_vertex(g, v2)
      {...g, edges: G.unsafe_add_edge(g.edges, v1, v2)}
    }

    let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)

    let remove_vertex = (g, v) =>
      if HM.mem(v, g.edges) {
        let e = HM.remove(v, g.edges)
        let e = HM.fold((k, s, g) => HM.add(k, S.remove(v, s), g), e, HM.empty)
        {edges: e, size: Stdlib.pred(g.size)}
      } else {
        g
      }

    let remove_edge = (g, v1, v2) => {...g, edges: remove_edge(g, v1, v2)}
    let remove_edge_e = (g, e) => {...g, edges: remove_edge_e(g, e)}
  }

  module AbstractLabeled = (
    V: {
      type t
    },
    Edge: ORDERED_TYPE_DFT,
  ) => {
    include P.Digraph.AbstractLabeled(AbstractVertex(V), Edge)

    let empty = {edges: G.empty, size: 0}

    let add_vertex = (g, v) =>
      if mem_vertex(g, v) {
        g
      } else {
        {
          edges: G.unsafe_add_vertex(g.edges, v),
          size: Stdlib.succ(g.size),
        }
      }

    let add_edge_e = (g, (v1, l, v2)) => {
      let g = add_vertex(g, v1)
      let g = add_vertex(g, v2)
      {...g, edges: G.unsafe_add_edge(g.edges, v1, (v2, l))}
    }

    let add_edge = (g, v1, v2) => add_edge_e(g, (v1, Edge.default, v2))

    let remove_vertex = (g, v) =>
      if HM.mem(v, g.edges) {
        let remove = (v, s) => S.fold(((v2, _) as e, s) =>
            if !V.equal(v, v2) {
              S.add(e, s)
            } else {
              s
            }
          , s, S.empty)

        let edges = HM.remove(v, g.edges)
        {
          edges: HM.fold((k, s, g) => HM.add(k, remove(v, s), g), edges, HM.empty),
          size: Stdlib.pred(g.size),
        }
      } else {
        g
      }

    let remove_edge = (g, v1, v2) => {...g, edges: remove_edge(g, v1, v2)}
    let remove_edge_e = (g, e) => {...g, edges: remove_edge_e(g, e)}
  }
}

module Graph = {
  module Concrete = (V: COMPARABLE) => {
    module G = {
      include Digraph.Concrete(V)
      type return = t
    }
    include Blocks.Graph(G)

    /* Export some definitions of [G] */
    let empty = G.empty

    /* Redefine the [add_edge] and [remove_edge] operations */

    let add_edge = (g, v1, v2) => {
      let g = G.add_edge(g, v1, v2)
      assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
      G.unsafe_add_edge(g, v2, v1)
    }

    let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)

    let remove_edge = (g, v1, v2) => {
      let g = G.remove_edge(g, v1, v2)
      assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
      G.unsafe_remove_edge(g, v2, v1)
    }

    let remove_edge_e = (g, (v1, v2)) => remove_edge(g, v1, v2)
  }

  module ConcreteLabeled = (V: COMPARABLE, Edge: ORDERED_TYPE_DFT) => {
    module G = {
      include Digraph.ConcreteLabeled(V, Edge)
      type return = t
    }
    include Blocks.Graph(G)

    /* Export some definitions of [G] */
    let empty = G.empty

    /* Redefine the [add_edge] and [remove_edge] operations */

    let add_edge_e = (g, (v1, l, v2) as e) => {
      let g = G.add_edge_e(g, e)
      assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
      G.unsafe_add_edge(g, v2, (v1, l))
    }

    let add_edge = (g, v1, v2) => add_edge_e(g, (v1, Edge.default, v2))

    let remove_edge = (g, v1, v2) => {
      let g = G.remove_edge(g, v1, v2)
      assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
      G.unsafe_remove_edge(g, v2, v1)
    }

    let remove_edge_e = (g, (v1, l, v2) as e) => {
      let g = G.remove_edge_e(g, e)
      assert (G.HM.mem(v1, g) && G.HM.mem(v2, g))
      G.unsafe_remove_edge_e(g, (v2, l, v1))
    }
  }

  module Abstract = (
    V: {
      type t
    },
  ) => {
    module G = {
      include Digraph.Abstract(V)
      type return = t
    }
    include Blocks.Graph(G)

    /* Export some definitions of [G] */
    let empty = G.empty

    /* Redefine the [add_edge] and [remove_edge] operations */

    let add_edge = (g, v1, v2) => {
      let g = G.add_edge(g, v1, v2)
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      {...g, G.edges: G.unsafe_add_edge(g.G.edges, v2, v1)}
    }

    let add_edge_e = (g, (v1, v2)) => add_edge(g, v1, v2)

    let remove_edge = (g, v1, v2) => {
      let g = G.remove_edge(g, v1, v2)
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      {...g, G.edges: G.unsafe_remove_edge(g.G.edges, v2, v1)}
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
      type return = t
    }
    include Blocks.Graph(G)

    /* Export some definitions of [G] */
    let empty = G.empty

    /* Redefine the [add_edge] and [remove_edge] operations */

    let add_edge_e = (g, (v1, l, v2) as e) => {
      let g = G.add_edge_e(g, e)
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      {...g, G.edges: G.unsafe_add_edge(g.G.edges, v2, (v1, l))}
    }

    let add_edge = (g, v1, v2) => add_edge_e(g, (v1, Edge.default, v2))

    let remove_edge = (g, v1, v2) => {
      let g = G.remove_edge(g, v1, v2)
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      {...g, G.edges: G.unsafe_remove_edge(g.G.edges, v2, v1)}
    }

    let remove_edge_e = (g, (v1, l, v2) as e) => {
      let g = G.remove_edge_e(g, e)
      assert (G.HM.mem(v1, g.G.edges) && G.HM.mem(v2, g.G.edges))
      {...g, G.edges: G.unsafe_remove_edge_e(g.G.edges, (v2, l, v1))}
    }
  }
}

/*
Local Variables:
compile-command: "make -C .."
End:
*/
