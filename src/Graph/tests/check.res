/* ************************************************************************ */
/*  */
/* Ocamlgraph: a generic graph library for OCaml */
/* Copyright (C) 2004-2007 */
/* Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles */
/*  */
/* This software is free software; you can redistribute it and/or */
/* modify it under the terms of the GNU Library General Public */
/* License version 2, with the special exception on linking */
/* described in file LICENSE. */
/*  */
/* This software is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. */
/*  */
/* ************************************************************************ */

//open Graph

module Int = {
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = \"="
  let default = 0
}

/* pair with equality which ignores the second component */
module Pair = {
  type t = (int, int)
  let compare = ((x, _), (y, _)) => Int.compare(x, y)
  let hash = ((x, _)) => Int.hash(x)
  let equal = ((x, _), (y, _)) => x == y
  let default = (0, 0)
}

module W = (
  E: {
    type t
    type label
    let label: t => label
  },
) => {
  type edge = E.t
  type t = int
  let weight = E.label
  let zero = 0
  let add = \"+"
  let compare = compare
}

/* ****************************************** */
/* Generic functions */
/* ****************************************** */

module Generic = {
  /* Generic tests for imperative graphs */
  module Make = (
    G: Sig.I with type V.label = int,
    V: {
      let v: int
      let e: int
    },
  ) => {
    module O = Oper.I(G)
    let test_mirror = g =>
      if G.is_directed {
        /* TODO: remove */
        let g' = O.mirror(g)
        assert (G.nb_vertex(g) == G.nb_vertex(g'))
        G.iter_edges((v1, v2) => assert G.mem_edge(g', v2, v1), g)
        G.iter_edges((v1, v2) => assert G.mem_edge(g, v2, v1), g')
        ()
      }

    let g = G.create()
    let () = {
      let v1 = G.V.create(1)
      let v2 = G.V.create(2)
      let v3 = G.V.create(3)
      test_mirror(g)
      G.add_edge(g, v1, v2)
      G.add_edge(g, v1, v3)
      G.add_edge(g, v2, v1)
      G.add_edge(g, v2, v2)
      G.add_edge(g, v2, v2)
      test_mirror(g)
      assert (G.nb_vertex(g) == V.v && G.nb_edges(g) == V.e)
      G.remove_vertex(g, v1)
      assert (G.nb_vertex(g) == 2 && G.nb_edges(g) == 1)
      G.remove_vertex(g, v2)
      assert (G.nb_vertex(g) == 1 && G.nb_edges(g) == 0)
      test_mirror(g)
      G.clear(g)
      assert (G.nb_vertex(g) == 0 && G.nb_edges(g) == 0)
    }
  }

  let () = {
    module A = Make(
      Imperative.Digraph.ConcreteLabeled(Int, Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = Make(
      Imperative.Graph.ConcreteLabeled(Int, Int),
      {
        let v = 3
        let e = 3
      },
    )

    module A = Make(
      Imperative.Digraph.AbstractLabeled(Int, Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = Make(
      Imperative.Graph.AbstractLabeled(Int, Int),
      {
        let v = 3
        let e = 3
      },
    )

    module A = Make(
      Imperative.Digraph.Concrete(Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = Make(
      Imperative.Graph.Concrete(Int),
      {
        let v = 3
        let e = 3
      },
    )

    module A = Make(
      Imperative.Digraph.Abstract(Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = Make(
      Imperative.Graph.Abstract(Int),
      {
        let v = 3
        let e = 3
      },
    )

    module A = Make(
      Imperative.Digraph.ConcreteBidirectional(Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = Make(
      Imperative.Digraph.ConcreteBidirectionalLabeled(Int, Int),
      {
        let v = 3
        let e = 4
      },
    )

    ()
  }

  /* Generic tests for persistent graphs */
  module MakeP = (
    G: Sig.P with type V.label = int,
    V: {
      let v: int
      let e: int
    },
  ) => {
    module O = Oper.P(G)
    let test_mirror = g => {
      let g' = O.mirror(g)
      assert (G.nb_vertex(g) == G.nb_vertex(g'))
    }

    let () = {
      let g = G.empty
      let v1 = G.V.create(1)
      let v2 = G.V.create(2)
      let v3 = G.V.create(3)
      test_mirror(g)
      let g = G.add_edge(g, v1, v2)
      let g = G.add_edge(g, v1, v3)
      let g = G.add_edge(g, v2, v1)
      let g = G.add_edge(g, v2, v2)
      let g = G.add_edge(g, v2, v2)
      test_mirror(g)
      assert (G.nb_vertex(g) == V.v && G.nb_edges(g) == V.e)
      let g = G.remove_vertex(g, v1)
      assert (G.nb_vertex(g) == 2 && G.nb_edges(g) == 1)
      let g = G.remove_vertex(g, v2)
      assert (G.nb_vertex(g) == 1 && G.nb_edges(g) == 0)
      test_mirror(g)
    }
  }

  let () = {
    module A = MakeP(
      Persistent.Digraph.ConcreteLabeled(Int, Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = MakeP(
      Persistent.Graph.ConcreteLabeled(Int, Int),
      {
        let v = 3
        let e = 3
      },
    )

    module A = MakeP(
      Persistent.Digraph.AbstractLabeled(Int, Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = MakeP(
      Persistent.Graph.AbstractLabeled(Int, Int),
      {
        let v = 3
        let e = 3
      },
    )

    module A = MakeP(
      Persistent.Digraph.Concrete(Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = MakeP(
      Persistent.Graph.Concrete(Int),
      {
        let v = 3
        let e = 3
      },
    )

    module A = MakeP(
      Persistent.Digraph.Abstract(Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = MakeP(
      Persistent.Graph.Abstract(Int),
      {
        let v = 3
        let e = 3
      },
    )

    module A = MakeP(
      Persistent.Digraph.ConcreteBidirectional(Int),
      {
        let v = 3
        let e = 4
      },
    )

    module A = MakeP(
      Persistent.Digraph.ConcreteBidirectionalLabeled(Int, Int),
      {
        let v = 3
        let e = 4
      },
    )

    ()
  }

  /* Generic tests for imperative concrete graphs with custom equality */
  module Make_pair = (
    G: Sig.I with type V.label = (int, int),
    V: {
      let v: int
      let e: int
    },
  ) => {
    module O = Oper.I(G)
    let test_mirror = g =>
      if G.is_directed {
        /* TODO: remove */
        let g' = O.mirror(g)
        assert (G.nb_vertex(g) == G.nb_vertex(g'))
        G.iter_edges((v1, v2) => assert G.mem_edge(g', v2, v1), g)
        G.iter_edges((v1, v2) => assert G.mem_edge(g, v2, v1), g')
        ()
      }

    let g = G.create()
    let () = {
      let v1 = G.V.create((1, 0))
      let v2 = G.V.create((2, 0))
      let v3 = G.V.create((2, 1))
      test_mirror(g)
      G.add_edge(g, v1, v2)
      G.add_edge(g, v2, v1)
      G.add_edge(g, v1, v3)
      G.iter_vertex(v => assert (snd(G.V.label(v)) == 0), g)
      test_mirror(g)
      assert (G.nb_vertex(g) == V.v && G.nb_edges(g) == V.e)
      G.remove_vertex(g, v3)
      assert (G.nb_vertex(g) == 1 && G.nb_edges(g) == 0)
      test_mirror(g)
      G.clear(g)
      assert (G.nb_vertex(g) == 0 && G.nb_edges(g) == 0)
    }
  }

  let () = {
    module A = Make_pair(
      Imperative.Digraph.ConcreteLabeled(Pair, Pair),
      {
        let v = 2
        let e = 2
      },
    )

    module A = Make_pair(
      Imperative.Graph.ConcreteLabeled(Pair, Pair),
      {
        let v = 2
        let e = 1
      },
    )

    module A = Make_pair(
      Imperative.Digraph.Concrete(Pair),
      {
        let v = 2
        let e = 2
      },
    )

    module A = Make_pair(
      Imperative.Graph.Concrete(Pair),
      {
        let v = 2
        let e = 1
      },
    )

    module A = Make_pair(
      Imperative.Digraph.ConcreteBidirectional(Pair),
      {
        let v = 2
        let e = 2
      },
    )

    module A = Make_pair(
      Imperative.Digraph.ConcreteBidirectionalLabeled(Pair, Pair),
      {
        let v = 2
        let e = 2
      },
    )

    ()
  }

  /* find_edge */

  module Make2 = (
    G: Sig.I with type V.t = int and type E.label = int and type E.t = (int, int, int),
  ) => {
    let g = G.create()

    let test_exn = (v1, v2) => {
      assert (G.find_all_edges(g, v1, v2) == list{})
      try {
        let _ = G.find_edge(g, v1, v2)
        assert false
      } catch {
      | Not_found => ()
      }
    }

    let () = {
      let e1 = (1, 0, 2)
      let e2 = (1, 1, 3)
      let e2' = (1, 2, 3)
      let e3 = (2, 2, 1)
      G.add_edge_e(g, e1)
      G.add_edge_e(g, e2)
      G.add_edge_e(g, e2')
      G.add_edge_e(g, e3)
      G.add_edge_e(g, e3)
      assert (G.find_edge(g, 1, 2) == e1)
      assert (List.length(G.find_all_edges(g, 1, 3)) == 2)
      test_exn(2, 3)
      test_exn(2, 4)
      test_exn(5, 2)
      G.remove_vertex(g, 2)
      assert (G.nb_vertex(g) == 2 && G.nb_edges(g) == 2)
    }
  }

  let () = {
    module D = Make2(Imperative.Digraph.ConcreteLabeled(Int, Int))
    D.test_exn(3, 1)
    module G = Imperative.Graph.ConcreteLabeled(Int, Int)
    module G2 = Make2(G)
    assert (G.find_edge(G2.g, 3, 1) == (3, 1, 1))
  }
}

/* ****************************************** */
/* Dijkstra */
/* ****************************************** */

module Dijkstra = {
  module TestDijkstra = (
    G: Sig.G with type V.label = int and type E.label = int,
    B: Builder.S with module G = G,
  ) => {
    let g = B.empty()
    let v1 = G.V.create(1)
    let g = B.add_vertex(g, v1)
    let v2 = G.V.create(2)
    let g = B.add_vertex(g, v2)
    let v3 = G.V.create(3)
    let g = B.add_vertex(g, v3)
    let v4 = G.V.create(4)
    let g = B.add_vertex(g, v4)
    let v5 = G.V.create(5)
    let g = B.add_vertex(g, v5)

    let g = B.add_edge_e(g, G.E.create(v1, 10, v2))
    let g = B.add_edge_e(g, G.E.create(v2, 50, v3))
    let g = B.add_edge_e(g, G.E.create(v1, 30, v4))
    let g = B.add_edge_e(g, G.E.create(v1, 100, v5))
    let g = B.add_edge_e(g, G.E.create(v3, 10, v5))
    let g = B.add_edge_e(g, G.E.create(v4, 20, v3))
    let g = B.add_edge_e(g, G.E.create(v4, 60, v5))

    module Dij = Path.Dijkstra(G, W(G.E))
    module Dfs = Traverse.Dfs(G)

    let test = (g, i, j, w, l) => {
      let (p, w') = Dij.shortest_path(g, i, j)
      assert (w' == w && List.length(p) == l)
    }
    let test_not_found = (g, i, j) =>
      try {
        let _ = Dij.shortest_path(g, i, j)
        assert false
      } catch {
      | Not_found => ()
      }

    let () = test(g, v1, v5, 60, 3)
    let () = test(g, v1, v1, 0, 0)
    let () = if G.is_directed {
      test_not_found(g, v5, v1)
    }
    let () = assert !Dfs.has_cycle(g)
    let gc = B.add_edge_e(g, G.E.create(v5, 10, v1))
    let v6 = G.V.create(6)
    let gc = B.add_vertex(gc, v6)
    let () = if G.is_directed {
      test(gc, v1, v5, 60, 3)
    }
    let () = test(gc, v5, v1, 10, 1)
    let () = test_not_found(gc, v1, v6)

    let () = assert Dfs.has_cycle(gc)
  }

  /* Dijkstra on Persistent Directed Labeled Graphs */

  module G = Persistent.Digraph.ConcreteLabeled(Int, Int)
  module Test1 = TestDijkstra(G, Builder.P(G))

  /* Dijkstra on Persistent Directed Abstract Labeled Graphs */

  module G2 = Persistent.Digraph.AbstractLabeled(Int, Int)
  module Test2 = TestDijkstra(G2, Builder.P(G2))

  /* Dijkstra on Imperative Hashed Directed Labeled Graphs */

  module G3 = Imperative.Digraph.ConcreteLabeled(Int, Int)
  module Test3 = TestDijkstra(G3, Builder.I(G3))
}

/* ****************************************** */
/* Traversal */
/* ****************************************** */

module Traversal = {
  module G = Imperative.Digraph.AbstractLabeled(Int, Int)
  module Dfs = Traverse.Dfs(G)
  module Mark = Traverse.Mark(G)

  let g = G.create()
  let newv = g => {
    let v = G.V.create(0)
    G.add_vertex(g, v)
    v
  }
  let v1 = newv(g)
  let v2 = newv(g)
  let v3 = newv(g)
  let v4 = newv(g)
  let v5 = newv(g)
  let add_edge = (g, v1, l, v2) => G.add_edge_e(g, G.E.create(v1, l, v2))
  let () = {
    add_edge(g, v1, 10, v2)
    add_edge(g, v2, 50, v3)
    add_edge(g, v1, 30, v4)
    add_edge(g, v1, 100, v5)
    add_edge(g, v3, 10, v5)
    add_edge(g, v4, 20, v3)
    add_edge(g, v4, 60, v5)
  }
  let () = assert (!Mark.has_cycle(g) && !Dfs.has_cycle(g))
  let v6 = newv(g)
  let () = assert (!Mark.has_cycle(g) && !Dfs.has_cycle(g))
  let () = add_edge(g, v5, 10, v1)
  let () = assert (Mark.has_cycle(g) && Dfs.has_cycle(g))

  /* debug dfs / Cormen p 479 */

  let g = G.create()
  let newv = i => {
    let v = G.V.create(i)
    G.add_vertex(g, v)
    v
  }
  let u = newv(1)
  let v = newv(2)
  let w = newv(3)
  let x = newv(4)
  let y = newv(5)
  let z = newv(6)
  let edge = (a, b) => add_edge(g, a, 0, b)
  let () = {
    edge(u, v)
    edge(u, x)
    edge(v, y)
    edge(w, y)
    edge(w, z)
    edge(x, v)
    edge(y, x)
    edge(z, z)
  }

  @@warning("-3")
  open Format
  let pre = v => printf("pre %d@.", G.V.label(v))
  let post = v => printf("post %d@.", G.V.label(v))
  /*
  let () = printf "iter:@."; Dfs.iter_component ~pre ~post g w
  let () = printf "prefix:@."; Dfs.prefix_component pre g w
  let () =
    printf "step:@.";
    let rec visit it =
      let v = Dfs.get it in
      printf "visit %d@." (G.V.label v);
      visit (Dfs.step it)
    in
    try visit (Dfs.start g) with Exit -> ()
 */
}

/* ****************************************** */
/* Ford-Fulkerson and Goldberg */
/* ****************************************** */

module FF_Goldberg = {
  module G = Persistent.Digraph.ConcreteLabeled(Int, Int)

  let add_edge = (g, v1, l, v2) => G.add_edge_e(g, G.E.create(v1, l, v2))
  let g = G.empty
  let g = add_edge(g, 1, 16, 2)
  let g = add_edge(g, 1, 13, 3)
  let g = add_edge(g, 2, 10, 3)
  let g = add_edge(g, 3, 4, 2)
  let g = add_edge(g, 2, 12, 4)
  let g = add_edge(g, 4, 9, 3)
  let g = add_edge(g, 3, 14, 5)
  let g = add_edge(g, 5, 7, 4)
  let g = add_edge(g, 4, 20, 6)
  let g = add_edge(g, 5, 4, 6)

  module F = {
    type label = int
    type t = int
    let max_capacity = x => x
    let min_capacity = _ => 0
    let flow = _ => 0
    let add = \"+"
    let sub = \"-"
    let compare = compare
    let zero = 0
  }

  module FF = Flow.Ford_Fulkerson(G, F)
  module Gold = Flow.Goldberg_Tarjan(G, F)

  let () = {
    assert (snd(FF.maxflow(g, 1, 6)) == 23)
    assert (snd(Gold.maxflow(g, 1, 6)) == 23)
    assert (snd(FF.maxflow(g, 1, 1)) == 0)
    assert (snd(Gold.maxflow(g, 1, 1)) == 0)
  }

  module G2 = Persistent.Digraph.ConcreteLabeled(
    Int,
    {
      include Util.OTProduct(Int, Int)
      let default = (0, 0)
    },
  )

  let add_edge = (g, v1, l, v2) => G2.add_edge_e(g, G2.E.create(v1, l, v2))
  let g = G2.empty
  let g = add_edge(g, 1, (1, 1), 2)
  let g = add_edge(g, 1, (3, 0), 3)
  let g = add_edge(g, 2, (1, 1), 3)
  let g = add_edge(g, 3, (1, 0), 2)
  let g = add_edge(g, 2, (3, 0), 4)
  let g = add_edge(g, 3, (1, 1), 4)

  module F2 = {
    type label = (int, int)
    type t = int
    let max_capacity = fst
    let min_capacity = _ => 0
    let flow = snd
    let add = \"+"
    let sub = \"-"
    let compare = compare
    let zero = 0
  }

  module FF2 = Flow.Ford_Fulkerson(G2, F2)
  module Gold2 = Flow.Goldberg_Tarjan(G2, F2)

  let () = {
    assert (snd(FF2.maxflow(g, 1, 4)) == 2) /* growth of the flow */
    assert (snd(Gold2.maxflow(g, 1, 4)) == 3)
  } /* max flow */
}

/* ****************************************** */
/* Neighbourhood */
/* ****************************************** */

module Neighbourhood = {
  module G = Imperative.Graph.Concrete(Int)
  open G

  let g = create()
  let add = add_edge(g)
  let () = {
    add(1, 2)
    add(1, 3)
    add(1, 4)
    add(2, 5)
    add(3, 5)
    add(4, 5)
    add(5, 6)
  }

  module N = Oper.Neighbourhood(G)
  module V = N.Vertex_Set
  let s2 = V.add(1, V.singleton(5))
  let () = assert V.equal(N.set_from_vertex(g, 2), s2)
  let s25 = V.add(1, V.add(3, V.add(4, V.singleton(6))))
  let () = assert V.equal(N.set_from_vertices(g, list{2, 5}), s25)
}

/* ****************************************** */
/* Minimal seperators */
/* ****************************************** */

module Minsep = {
  module P = {
    module G = Persistent.Graph.Concrete(Int)
    open G

    let g = empty
    let g = add_edge(g, 1, 2)
    let g = add_edge(g, 1, 3)
    let g = add_edge(g, 1, 4)
    let g = add_edge(g, 2, 5)
    let g = add_edge(g, 3, 5)
    let g = add_edge(g, 4, 5)
    let g = add_edge(g, 5, 6)

    module M = Minsep.P(G)
    module S = M.Vertex_Set
    module VS = M.VSetset
    let s5 = S.singleton(5)
    let s15 = S.add(1, s5)
    let s234 = S.add(2, S.add(3, S.singleton(4)))
    let bigs = VS.add(s5, VS.add(s15, VS.singleton(s234)))
    let () = assert VS.equal(M.set_of_allminsep(g), bigs)
  }

  module I = {
    module G = Imperative.Graph.Abstract({
      type t = unit
    })
    open G

    let g = create()
    let v1 = V.create()
    let v2 = V.create()
    let v3 = V.create()
    let v4 = V.create()
    let v5 = V.create()
    let v6 = V.create()
    let add = add_edge(g)
    let () = {
      add(v1, v2)
      add(v1, v3)
      add(v1, v4)
      add(v2, v5)
      add(v3, v5)
      add(v4, v5)
      add(v5, v6)
    }

    module M = Minsep.I(G)
    module S = M.Vertex_Set
    module VS = M.VSetset
    let s5 = S.singleton(v5)
    let s15 = S.add(v1, s5)
    let s234 = S.add(v2, S.add(v3, S.singleton(v4)))
    let bigs = VS.add(s5, VS.add(s15, VS.singleton(s234)))
    let () = {
      let _ = G.copy(g)
      assert VS.equal(M.set_of_allminsep(g), bigs)
    }
  }
}

/* ****************************************** */
/* Checking signature */
/* ****************************************** */

/* check that signature [Sig_pack.S] (which is manually expanded) does not
 forget anything */
module type RightSigPack = {
  include Sig.IM with type V.label = int and type E.label = int
  let find_vertex: (t, int) => V.t
  include Oper.S with type g = t
  module Dfs: {
    let iter: (~pre: V.t => unit=?, ~post: V.t => unit=?, t) => unit
    let prefix: (V.t => unit, t) => unit
    let postfix: (V.t => unit, t) => unit

    let iter_component: (~pre: V.t => unit=?, ~post: V.t => unit=?, t, V.t) => unit
    let prefix_component: (V.t => unit, t, V.t) => unit
    let postfix_component: (V.t => unit, t, V.t) => unit

    let has_cycle: t => bool
  }
  module Bfs: {
    let iter: (V.t => unit, t) => unit
    let iter_component: (V.t => unit, t, V.t) => unit
  }
  module Marking: {
    let dfs: t => unit
    let has_cycle: t => bool
  }
  module Classic: {
    let divisors: int => t
    let de_bruijn: int => t
    let vertex_only: int => t
    let full: (~self: bool=?, int) => t
  }
  module Rand: {
    let graph: (~loops: bool=?, ~v: int, ~e: int, unit) => t
    let labeled: ((V.t, V.t) => E.label, ~loops: bool=?, ~v: int, ~e: int, unit) => t
  }
  module Components: {
    let scc: t => (int, V.t => int)
    let scc_array: t => array<list<V.t>>
    let scc_list: t => list<list<V.t>>
  }
  let shortest_path: (t, V.t, V.t) => (list<E.t>, int)
  let ford_fulkerson: (t, V.t, V.t) => (E.t => int, int)
  let goldberg_tarjan: (t, V.t, V.t) => (E.t => int, int)
  let dot_output: (t, string) => unit
}

module TestSigPack: RightSigPack = {
  include Pack.Digraph
  type g = t
}

module Test_clique = {
  /* Test file for Brom-Kerbosch */

  //  open Graph

  module G = Persistent.Graph.Concrete({
    type t = int
    let compare = compare
    let hash = Hashtbl.hash
    let equal = \"="
  })

  module BK = Clique.Bron_Kerbosch(G)

  let () = {
    let vertices = list{1, 2, 3, 4, 5, 6, 7}
    let edges = list{(1, 2), (1, 5), (2, 5), (2, 3), (4, 5), (3, 4), (4, 6)}
    let g = List.fold_left((graph, v) => G.add_vertex(graph, v), G.empty, vertices)
    let g = List.fold_left((graph, (v1, v2)) => G.add_edge(graph, v1, v2), g, edges)
    let cliques = BK.maximalcliques(g)
    /* The cliques of this graph should be: [2, 3], [3, 4], [1, 2, 5], [4, 5], [4, 6], [7] */
    assert (List.length(cliques) === 6)
    assert List.exists(cl => List.length(cl) === 2 && (List.mem(2, cl) && List.mem(3, cl)), cliques)
    assert List.exists(cl => List.length(cl) === 2 && (List.mem(3, cl) && List.mem(4, cl)), cliques)
    assert List.exists(
      cl => List.length(cl) === 3 && (List.mem(1, cl) && (List.mem(2, cl) && List.mem(5, cl))),
      cliques,
    )
    assert List.exists(cl => List.length(cl) === 2 && (List.mem(4, cl) && List.mem(5, cl)), cliques)
    assert List.exists(cl => List.length(cl) === 2 && (List.mem(4, cl) && List.mem(6, cl)), cliques)
    assert List.exists(cl => List.length(cl) === 1 && List.mem(7, cl), cliques)
  }
}

module Test_reduction = {
  //  open Graph

  module G = Imperative.Digraph.Concrete({
    type t = int
    let compare = compare
    let hash = Hashtbl.hash
    let equal = \"="
  })
  open G

  module R = Rand.I(G)
  module O = Oper.I(G)

  let check_included = (g1, g2) => {
    iter_vertex(v => assert mem_vertex(g2, v), g1)
    iter_edges((u, v) => assert mem_edge(g1, u, v), g1)
  }

  let check_same_graph = (g1, g2) => {
    check_included(g1, g2)
    check_included(g2, g1)
  }

  let test = (v, e) => {
    let g = R.graph(~loops=true, ~v, ~e, ())
    let t = O.transitive_closure(g)
    check_included(g, t)
    let r = O.transitive_reduction(g)
    check_included(r, g)
    check_same_graph(O.transitive_closure(r), t)
  }

  let () = for v in 1 to 10 {
    for e in 0 to v * (v - 1) / 2 {
      test(v, e)
    }
  }

  /* issue #91 */
  let () = {
    let g = create()
    for v in 1 to 5 {
      add_vertex(g, v)
    }
    add_edge(g, 1, 2)
    add_edge(g, 2, 3)
    add_edge(g, 3, 4)
    add_edge(g, 4, 5)
    add_edge(g, 2, 5)
    let r = O.transitive_reduction(g)
    check_included(r, g)
    assert (nb_edges(r) == 4)
    assert !mem_edge(r, 2, 5)
    ()
  }
}

@@warning("-3")
let () = Format.printf("check: all tests succeeded@.")

/*
Local Variables:
compile-command: "make -C .. check"
End:
*/
