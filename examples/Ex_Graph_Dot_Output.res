open Graph
open Ex_Graph_Samples

let () = {
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

  let g = sample1

  let () = Gv.fprint_graph(Format.str_formatter, g)
  let s = Format.flush_str_formatter()
  s->Js.log
}
