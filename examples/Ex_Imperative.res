open Belt
@@warning("-3-32")

let () = {
  module Str = {
    type t = string
    let compare = compare
    let equal = \"="
    let hash = Hashtbl.hash
    let default = ""
  }

  module G = Imperative.Graph.Concrete(Str)
  //  module G = Imperative.Graph.ConcreteLabeled(Str, Str)
  //  module G = Imperative.Graph.ConcreteBidirectional(Str)
  //  module G = Imperative.Digraph.Concrete(Str)
  //  module G = Imperative.Digraph.ConcreteLabeled(Str, Str)
  //  module G = Imperative.Digraph.ConcreteBidirectional(Str)
  module Dfs = Traverse.Dfs(G)

  let g = G.create()

  List.forEach(
    list{
      ("u", list{"v", "x"}),
      ("v", list{"y"}),
      ("w", list{"z", "y"}),
      ("x", list{"v"}),
      ("y", list{"x"}),
      ("z", list{"z"}),
    },
    ((v, e)) => {
      e->List.forEach(y => G.add_edge(g, v, y))
    },
  )

  module Dfs = Traverse.Dfs(G)
  Dfs.has_cycle(g)->Js.log

  // Dfs.iter

  let pre = v => Js.log(` pre ${G.V.label(v)}.`)
  let post = v => Js.log(`post ${G.V.label(v)}.`)

  "iter: "->Js.log
  Dfs.iter_component(~pre, ~post, g, "w")
  "prefix: "->Js.log
  Dfs.prefix_component(pre, g, "w")

  // output Graphvis DOT
  module Display = {
    include G
    let vertex_name = v => V.label(v)
    let graph_attributes = _ => list{}
    let default_vertex_attributes = _ => list{}
    let vertex_attributes = _ => list{}
    let default_edge_attributes = _ => list{}
    let edge_attributes = _ => list{}
    let get_subgraph = _ => None
  }

  module Gv = Graphviz.Dot(Display)
  //  let file_ch = open_out("DFS.dot")
  //  let () = Gv.output_graph(stdout, temp)
  let () = Gv.fprint_graph(Format.str_formatter, g)
  let s = Format.flush_str_formatter()
  "s="->Js.log
  s->Js.log
}
