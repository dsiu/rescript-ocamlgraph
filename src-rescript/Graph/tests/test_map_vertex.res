/* Check that map_vertex applies the function exactly once per vertex */

//open Graph

let () = Random.init(1597)

module TestB = (B: Builder.S with type G.V.label = int) => {
  let test = n => {
    let v = Array.init(n, B.G.V.create)
    let rec make = (g, i) =>
      if i == n {
        g
      } else {
        make(B.add_vertex(g, v[i]), i + 1)
      }
    let g = ref(make(B.empty(), 0))
    for i in 0 to n - 1 {
      for j in 0 to n - 1 {
        if Random.bool() {
          g := B.add_edge(g.contents, v[i], v[j])
        }
      }
    }
    let counter = ref(0)
    let f = x => {
      incr(counter)
      x
    }
    let g' = B.G.map_vertex(f, g.contents)
    assert (counter.contents == n)
    assert (B.G.nb_vertex(g') == n)
  }

  let () = for n in 0 to 10 {
    test(n)
  }
}
module TestI = (G: Sig.I with type V.label = int) => TestB(Builder.I(G))
module TestP = (G: Sig.P with type V.label = int) => TestB(Builder.P(G))

module Int = {
  type t = int
  let compare = compare
  let equal = \"="

  let hash = x => x
  let default = 42
}

include TestI(Pack.Digraph)
include TestI(Pack.Graph)

/* imperative, directed */
include TestI(Imperative.Digraph.Concrete(Int))
include TestI(Imperative.Digraph.Abstract(Int))
include TestI(Imperative.Digraph.ConcreteBidirectional(Int))
include TestI(Imperative.Digraph.ConcreteLabeled(Int, Int))
include TestI(Imperative.Digraph.AbstractLabeled(Int, Int))
include TestI(Imperative.Digraph.ConcreteBidirectionalLabeled(Int, Int))
/* imperative, undirected */
include TestI(Imperative.Graph.Concrete(Int))
include TestI(Imperative.Graph.Abstract(Int))
include TestI(Imperative.Graph.ConcreteLabeled(Int, Int))
include TestI(Imperative.Graph.AbstractLabeled(Int, Int))

module TestM = (G: Imperative.Matrix.S) => {
  let test = n => {
    let g = G.make(n)
    for i in 0 to n - 1 {
      for j in 0 to n - 1 {
        if Random.bool() {
          G.add_edge(g, i, j)
        }
      }
    }
    let counter = ref(0)
    let f = x => {
      incr(counter)
      x
    }
    let g' = G.map_vertex(f, g)
    assert (counter.contents == n)
    assert (G.nb_vertex(g') == n)
  }

  let () = for n in 0 to 10 {
    test(n)
  }
}
include TestM(Imperative.Matrix.Digraph)
include TestM(Imperative.Matrix.Graph)

/* persistent, directed */
include TestP(Persistent.Digraph.Concrete(Int))
include TestP(Persistent.Digraph.Abstract(Int))
include TestP(Persistent.Digraph.ConcreteBidirectional(Int))
include TestP(Persistent.Digraph.ConcreteLabeled(Int, Int))
include TestP(Persistent.Digraph.AbstractLabeled(Int, Int))
include TestP(Persistent.Digraph.ConcreteBidirectionalLabeled(Int, Int))
/* persistent, undirected */
include TestP(Persistent.Graph.Concrete(Int))
include TestP(Persistent.Graph.Abstract(Int))
include TestP(Persistent.Graph.ConcreteLabeled(Int, Int))
include TestP(Persistent.Graph.AbstractLabeled(Int, Int))
