open Belt
@@warning("-3")

let () = {
  module Str = {
    type t = string
    let compare = compare
    let equal = \"="
    let hash = Hashtbl.hash
  }

  //  module G = Imperative.Graph.Concrete(Str)
  module G = Imperative.Digraph.ConcreteBidirectional(Str)

  let g = G.create()
  let temp = G.create()

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

  let rec visit = it => {
    let v = Dfs.get(it)
    Js.log(`visit ${G.V.label(v)}`)
    visit(Dfs.step(it))
  }

  //  try {
  //    visit(Dfs.start(g))
  //  } catch {
  //  | Exit => ()
  //  }

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

  temp->G.add_edge("a", "b")
  temp->G.add_edge("a", "c")
  temp->G.add_edge("b", "e")
  temp->G.add_edge("a", "a")
  //  let file_ch = open_out("DFS.dot")
  //  let () = Gv.output_graph(stdout, temp)
  let () = Gv.fprint_graph(Format.str_formatter, g)
  let s = Format.flush_str_formatter()
  "s="->Js.log
  s->Js.log
}
