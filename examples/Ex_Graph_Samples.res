/* Use Ocamlraph Module */
open Belt
open Graph

let log = Js.log
let logList = l => l->List.toArray->log
let log2 = (x, y) => Js.log2(y, x)
let logList2 = (l, str) => l->List.toArray->log2(str)

module Str = {
  type t = string
  let compare = compare
  let equal = \"="
  let hash = Hashtbl.hash
}

//  module G = Imperative.Graph.Concrete(Str)
module G = Imperative.Digraph.ConcreteBidirectional(Str)

let createSampleGraph = l => {
  let g = G.create()

  List.forEach(l, ((v, e)) => {
    e->List.forEach(y => G.add_edge(g, v, y))
  })
  g
}

let sample1 = list{
  ("u", list{"v", "x"}),
  ("v", list{"y"}),
  ("w", list{"z", "y"}),
  ("x", list{"v"}),
  ("y", list{"x"}),
  ("z", list{"z"}),
}

let sample1 = createSampleGraph(sample1)
