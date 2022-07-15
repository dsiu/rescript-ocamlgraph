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

/* $Id: pack.ml,v 1.13 2006-05-12 14:07:16 filliatr Exp $ */

module Stdlib = Pervasives

module Generic = (G: Sig.IM with type V.label = int and type E.label = int) => {
  include G

  exception Found(V.t)
  let find_vertex = (g, i) =>
    try {
      iter_vertex(v =>
        if V.label(v) == i {
          raise(Found(v))
        }
      , g)
      raise(Not_found)
    } catch {
    | Found(v) => v
    }

  module Builder = Builder.I(G)

  module Dfs = Traverse.Dfs(G)
  module Bfs = Traverse.Bfs(G)
  module Marking = Traverse.Mark(G)
  module Coloring = Coloring.Mark(G)

  module Classic = Classic.I(G)

  module Rand = Rand.I(G)

  module Components = Components.Make(G)

  module W = {
    type edge = G.E.t
    type t = int
    let weight = e => G.E.label(e)
    let zero = 0
    let add = \"+"
    let sub = \"-"
    let compare: (t, t) => int = Stdlib.compare
  }

  include Path.Dijkstra(G, W)
  include Path.Johnson(G, W)

  module BF = Path.BellmanFord(G, W)
  let bellman_ford = BF.find_negative_cycle_from

  module F = {
    type label = int
    type t = int
    let max_capacity = x => x
    let min_capacity = _ => 0
    let flow = _ => 0
    let add = \"+"
    let sub = \"-"
    let compare: (t, t) => int = Stdlib.compare
    let zero = 0
  }

  module FF = Flow.Ford_Fulkerson(G, F)
  let ford_fulkerson = g => {
    if !G.is_directed {
      invalid_arg("ford_fulkerson: not a directed graph")
    }
    FF.maxflow(g)
  }

  module Goldberg = Flow.Goldberg_Tarjan(G, F)
  let goldberg_tarjan = g => {
    if !G.is_directed {
      invalid_arg("goldberg: not a directed graph")
    }
    Goldberg.maxflow(g)
  }

  include Oper.Make(Builder)

  module PathCheck = Path.Check(G)

  module Topological = {
    include Topological.Make(G)
    module S = Topological.Make_stable(G)
    let fold_stable = S.fold
    let iter_stable = S.iter
  }

  module Eulerian = {
    include Eulerian.Make(G)
  }

  module Int = {
    type t = int
    let compare: (t, t) => int = Stdlib.compare
  }

  include Kruskal.Make(G, Int)

  module Display = {
    include G
    let vertex_name = v => string_of_int(V.label(v))
    let graph_attributes = _ => list{}
    let default_vertex_attributes = _ => list{}
    let vertex_attributes = _ => list{}
    let default_edge_attributes = _ => list{}
    let edge_attributes = e => list{#Label(string_of_int(E.label(e)))}
    let get_subgraph = _ => None
  }
  module Dot_ = Graphviz.Dot(Display)
  module Neato = Graphviz.Neato(Display)

  let dot_output = (g, f) => {
    let oc = open_out(f)
    if is_directed {
      Dot_.output_graph(oc, g)
    } else {
      Neato.output_graph(oc, g)
    }
    close_out(oc)
  }

  // todo: replace Sys with Rescript.JS
  let display_with_gv = g => {
    let tmp = Filename.temp_file("graph", ".dot")
    dot_output(g, tmp)
    ignore(Sys.command("dot -Tps " ++ (tmp ++ " | gv -")))
    Sys.remove(tmp)
  }

  // Todo: fix this
  /*
  module GmlParser = Gml.Parse(
    Builder,
    {
      let node = l =>
        try switch List.assoc("id", l) {
        | Gml.Int(n) => n
        | _ => -1
        } catch {
        | Not_found => -1
        }
      let edge = _ => 0
    },
  )

  let parse_gml_file = GmlParser.parse

  module DotParser = Dot.Parse(
    Builder,
    {
      let nodes = Hashtbl.create(97)
      let new_node = ref(0)
      let node = ((id, _), _) =>
        try Hashtbl.find(nodes, id) catch {
        | Not_found =>
          incr(new_node)
          Hashtbl.add(nodes, id, new_node.contents)
          new_node.contents
        }
      let edge = _ => 0
    },
  )

  let parse_dot_file = DotParser.parse
  open Format

  module GmlPrinter = Gml.Print(
    G,
    {
      let node = n => list{("label", Gml.Int(n))}
      let edge = n => list{("label", Gml.Int(n))}
    },
  )

  let print_gml = GmlPrinter.print
  let print_gml_file = (g, f) => {
    let c = open_out(f)
    let fmt = formatter_of_out_channel(c)
    fprintf(fmt, "%a@.", GmlPrinter.print, g)
    close_out(c)
  }
 */

  /*
  module GraphmlPrinter =
    Graphml.Print
      (G)
      (struct
   let node n = ["label", Gml.Int n]
   let edge n = ["label", Gml.Int n]
         module Vhash = Hashtbl.Make(G.V)
         let vertex_uid = uid (Vhash.create 17) Vhash.find Vhash.add
         module Ehash = Hashtbl.Make(G.E)
         let edge_uid = uid (Ehash.create 17) Ehash.find Ehash.add
       end)

  let print_gml = GmlPrinter.print
  let print_gml_file g f =
    let c = open_out f in
    let fmt = formatter_of_out_channel c in
    fprintf fmt "%a@." GmlPrinter.print g;
    close_out c
*/
}

module I = {
  type t = int
  let compare: (t, t) => int = Stdlib.compare
  let default = 0
}

module Digraph = Generic(Imperative.Digraph.AbstractLabeled(I, I))

module Graph = Generic(Imperative.Graph.AbstractLabeled(I, I))
