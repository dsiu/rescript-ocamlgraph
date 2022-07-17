open Graph
open Ex_Graph_Samples

let () = {
  // output Graphml
  module L = {
    include G
    let node = n => list{("label", Gml.String(n))}
    let edge = _ => list{}
  }
  module GmlPrinter = Graph.Gml.Print(G, L)

  let print = g => GmlPrinter.print(Format.std_formatter, g)

  let g = sample1
  print(g)
}
