open Ex_Graph_Samples

let () = {
  // output Graphml

  module Gr = {
    include G
    let vertex_properties = list{("id1", "string", None)}
    let edge_properties = list{("ed", "string", Some("3"))}
    let map_edge = e => list{("ed", E.dst(e))}
    let map_vertex = v => list{("id1", v)}
    let vertex_uid = G.V.hash
    let edge_uid = e => Hashtbl.hash((vertex_uid(G.E.src(e)), G.E.label(e), vertex_uid(G.E.dst(e))))
  }

  module GraphPrinter = Graph.Graphml.Print(G, Gr)

  let print = g => GraphPrinter.print(Format.std_formatter, g)

  let g = sample1
  print(g)
}
