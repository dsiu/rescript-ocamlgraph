@@ocaml.text(
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

  " {b Signatures for graph implementations.} "
)

@@ocaml.text(" {2 Signature for ordered and hashable types} ")

@ocaml.doc(" Signature with only an abstract type. ")
module type ANY_TYPE = {
  type t
}

@ocaml.doc(" Signature equivalent to [Set.OrderedType]. ")
module type ORDERED_TYPE = {
  type t
  let compare: (t, t) => int
}

@ocaml.doc(" Signature equivalent to [Set.OrderedType] with a default value. ")
module type ORDERED_TYPE_DFT = {
  include ORDERED_TYPE
  let default: t
}

@ocaml.doc(" Signature equivalent to [Hashtbl.HashedType]. ")
module type HASHABLE = {
  type t
  let hash: t => int
  let equal: (t, t) => bool
}

@ocaml.doc(" Signature merging {!ORDERED_TYPE} and {!HASHABLE}. ")
module type COMPARABLE = {
  type t
  let compare: (t, t) => int
  let hash: t => int
  let equal: (t, t) => bool
}

@@ocaml.text(" {2 Signatures for graph implementations} ")

@ocaml.doc(" Signature for vertices. ")
module type VERTEX = {
  @@ocaml.text(" Vertices are {!COMPARABLE}. ")

  type t

  include COMPARABLE with type t := t

  @@ocaml.text(" Vertices are labeled. ")

  type label
  let create: label => t
  let label: t => label
}

@ocaml.doc(" Signature for edges. ")
module type EDGE = {
  @@ocaml.text(" Edges are {!ORDERED_TYPE}. ")

  type t
  let compare: (t, t) => int

  @@ocaml.text(" Edges are directed. ")

  type vertex

  @ocaml.doc(" Edge origin. ")
  let src: t => vertex

  @ocaml.doc(" Edge destination. ")
  let dst: t => vertex

  @@ocaml.text(" Edges are labeled. ")

  type label
  @ocaml.doc(" [create v1 l v2] creates an edge from [v1] to [v2] with label [l] ")
  let create: (vertex, label, vertex) => t

  @ocaml.doc(" Get the label of an edge. ")
  let label: t => label
}

@ocaml.doc(" Common signature for all graphs. ")
module type G = {
  @@ocaml.text(" {2 Graph structure} ")

  @ocaml.doc(" Abstract type of graphs ")
  type t

  @ocaml.doc(" Vertices have type [V.t] and are labeled with type [V.label]
      (note that an implementation may identify the vertex with its
      label) ")
  module V: VERTEX
  type vertex = V.t

  @ocaml.doc(" Edges have type [E.t] and are labeled with type [E.label].
      [src] (resp. [dst]) returns the origin (resp. the destination) of a
      given edge. ")
  module E: EDGE with type vertex = vertex
  type edge = E.t

  @ocaml.doc(" Is this an implementation of directed graphs? ")
  let is_directed: bool

  @@ocaml.text(" {2 Size functions} ")

  let is_empty: t => bool
  let nb_vertex: t => int
  let nb_edges: t => int

  @@ocaml.text(" Degree of a vertex ")

  @ocaml.doc(" [out_degree g v] returns the out-degree of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let out_degree: (t, vertex) => int

  @ocaml.doc(" [in_degree g v] returns the in-degree of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let in_degree: (t, vertex) => int

  @@ocaml.text(" {2 Membership functions} ")

  let mem_vertex: (t, vertex) => bool
  let mem_edge: (t, vertex, vertex) => bool
  let mem_edge_e: (t, edge) => bool

  @ocaml.doc(" [find_edge g v1 v2] returns the edge from [v1] to [v2] if it exists.
      Unspecified behaviour if [g] has several edges from [v1] to [v2].
      @raise Not_found if no such edge exists. ")
  let find_edge: (t, vertex, vertex) => edge

  @ocaml.doc(" [find_all_edges g v1 v2] returns all the edges from [v1] to [v2].
      @since ocamlgraph 1.8 ")
  let find_all_edges: (t, vertex, vertex) => list<edge>

  @@ocaml.text(" {2 Successors and predecessors}

      You should better use iterators on successors/predecessors (see
      Section \"Vertex iterators\"). ")

  @ocaml.doc(" [succ g v] returns the successors of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let succ: (t, vertex) => list<vertex>

  @ocaml.doc(" [pred g v] returns the predecessors of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let pred: (t, vertex) => list<vertex>

  @@ocaml.text(" Labeled edges going from/to a vertex ")

  @ocaml.doc(" [succ_e g v] returns the edges going from [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let succ_e: (t, vertex) => list<edge>

  @ocaml.doc(" [pred_e g v] returns the edges going to [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let pred_e: (t, vertex) => list<edge>

  @@ocaml.text(" {2 Graph iterators} ")

  @ocaml.doc(" Iter on all vertices of a graph. ")
  let iter_vertex: (vertex => unit, t) => unit

  @ocaml.doc(" Fold on all vertices of a graph. ")
  let fold_vertex: ((vertex, 'a) => 'a, t, 'a) => 'a

  @ocaml.doc(" Iter on all edges of a graph. Edge label is ignored. ")
  let iter_edges: ((vertex, vertex) => unit, t) => unit

  @ocaml.doc(" Fold on all edges of a graph. Edge label is ignored. ")
  let fold_edges: ((vertex, vertex, 'a) => 'a, t, 'a) => 'a

  @ocaml.doc(" Iter on all edges of a graph. ")
  let iter_edges_e: (edge => unit, t) => unit

  @ocaml.doc(" Fold on all edges of a graph. ")
  let fold_edges_e: ((edge, 'a) => 'a, t, 'a) => 'a

  @ocaml.doc(" Map on all vertices of a graph.

      The current implementation requires the supplied function to be
      injective. Said otherwise, [map_vertex] cannot be used to contract
      a graph by mapping several vertices to the same vertex.
      To contract a graph, use instead [create], [add_vertex],
      and [add_edge]. ")
  let map_vertex: (vertex => vertex, t) => t

  @@ocaml.text(" {2 Vertex iterators}

      Each iterator [iterator f v g] iters [f] to the successors/predecessors
      of [v] in the graph [g] and raises [Invalid_argument] if [v] is not in
      [g]. It is the same for functions [fold_*] which use an additional
      accumulator.

      <b>Time complexity for ocamlgraph implementations:</b>
      operations on successors are in O(1) amortized for imperative graphs and
      in O(ln(|V|)) for persistent graphs while operations on predecessors are
      in O(max(|V|,|E|)) for imperative graphs and in O(max(|V|,|E|)*ln|V|) for
      persistent graphs. ")

  @@ocaml.text(" iter/fold on all successors/predecessors of a vertex. ")

  let iter_succ: (vertex => unit, t, vertex) => unit
  let iter_pred: (vertex => unit, t, vertex) => unit
  let fold_succ: ((vertex, 'a) => 'a, t, vertex, 'a) => 'a
  let fold_pred: ((vertex, 'a) => 'a, t, vertex, 'a) => 'a

  @@ocaml.text(" iter/fold on all edges going from/to a vertex. ")

  let iter_succ_e: (edge => unit, t, vertex) => unit
  let fold_succ_e: ((edge, 'a) => 'a, t, vertex, 'a) => 'a
  let iter_pred_e: (edge => unit, t, vertex) => unit
  let fold_pred_e: ((edge, 'a) => 'a, t, vertex, 'a) => 'a
}

@ocaml.doc(" Signature for persistent (i.e. immutable) graph. ")
module type P = {
  @ocaml.doc(" A persistent graph is a graph. ")
  include G

  @ocaml.doc(" The empty graph. ")
  let empty: t

  @ocaml.doc(" [add_vertex g v] adds the vertex [v] to the graph [g].
      Just return [g] if [v] is already in [g]. ")
  let add_vertex: (t, vertex) => t

  @ocaml.doc(" [remove g v] removes the vertex [v] from the graph [g]
      (and all the edges going from [v] in [g]).
      Just return [g] if [v] is not in [g].

      <b>Time complexity for ocamlgraph implementations:</b>
      O(|V|*ln(|V|)) for unlabeled graphs and
      O(|V|*max(ln(|V|),D)) for labeled graphs.
      D is the maximal degree of the graph. ")
  let remove_vertex: (t, vertex) => t

  @ocaml.doc(" [add_edge g v1 v2] adds an edge from the vertex [v1] to the vertex [v2]
      in the graph [g].
      Add also [v1] (resp. [v2]) in [g] if [v1] (resp. [v2]) is not in [g].
      Just return [g] if this edge is already in [g]. ")
  let add_edge: (t, vertex, vertex) => t

  @ocaml.doc(" [add_edge_e g e] adds the edge [e] in the graph [g].
      Add also [E.src e] (resp. [E.dst e]) in [g] if [E.src e] (resp. [E.dst
      e]) is not in [g].
      Just return [g] if [e] is already in [g]. ")
  let add_edge_e: (t, edge) => t

  @ocaml.doc(" [remove_edge g v1 v2] removes the edge going from [v1] to [v2] from the
      graph [g]. If the graph is labelled, all the edges going from [v1] to
      [v2] are removed from [g].
      Just return [g] if this edge is not in [g].
      @raise Invalid_argument if [v1] or [v2] are not in [g]. ")
  let remove_edge: (t, vertex, vertex) => t

  @ocaml.doc(" [remove_edge_e g e] removes the edge [e] from the graph [g].
      Just return [g] if [e] is not in [g].
      @raise Invalid_argument if [E.src e] or [E.dst e] are not in [g]. ")
  let remove_edge_e: (t, edge) => t
}

@ocaml.doc(" Signature for imperative (i.e. mutable) graphs. ")
module type I = {
  @ocaml.doc(" An imperative graph is a graph. ")
  include G

  @ocaml.doc(" [create ()] returns an empty graph. Optionally, a size can be
      given, which should be on the order of the expected number of
      vertices that will be in the graph (for hash tables-based
      implementations).  The graph grows as needed, so [size] is
      just an initial guess. ")
  let create: (~size: int=?, unit) => t

  @ocaml.doc(" Remove all vertices and edges from the given graph.
      @since ocamlgraph 1.4 ")
  let clear: t => unit

  @ocaml.doc(" [copy g] returns a copy of [g]. Vertices and edges (and eventually
      marks, see module [Mark]) are duplicated. ")
  let copy: t => t

  @ocaml.doc(" [add_vertex g v] adds the vertex [v] to the graph [g].
      Do nothing if [v] is already in [g]. ")
  let add_vertex: (t, vertex) => unit

  @ocaml.doc(" [remove g v] removes the vertex [v] from the graph [g]
      (and all the edges going from [v] in [g]).
      Do nothing if [v] is not in [g].

      <b>Time complexity for ocamlgraph implementations:</b>
      O(|V|*ln(D)) for unlabeled graphs and O(|V|*D)  for
      labeled graphs. D is the maximal degree of the graph. ")
  let remove_vertex: (t, vertex) => unit

  @ocaml.doc(" [add_edge g v1 v2] adds an edge from the vertex [v1] to the vertex [v2]
      in the graph [g].
      Add also [v1] (resp. [v2]) in [g] if [v1] (resp. [v2]) is not in [g].
      Do nothing if this edge is already in [g]. ")
  let add_edge: (t, vertex, vertex) => unit

  @ocaml.doc(" [add_edge_e g e] adds the edge [e] in the graph [g].
      Add also [E.src e] (resp. [E.dst e]) in [g] if [E.src e] (resp. [E.dst
      e]) is not in [g].
      Do nothing if [e] is already in [g]. ")
  let add_edge_e: (t, edge) => unit

  @ocaml.doc(" [remove_edge g v1 v2] removes the edge going from [v1] to [v2] from the
      graph [g]. If the graph is labelled, all the edges going from [v1] to
      [v2] are removed from [g].
      Do nothing if this edge is not in [g].
      @raise Invalid_argument if [v1] or [v2] are not in [g]. ")
  let remove_edge: (t, vertex, vertex) => unit

  @ocaml.doc(" [remove_edge_e g e] removes the edge [e] from the graph [g].
      Do nothing if [e] is not in [g].
      @raise Invalid_argument if [E.src e] or [E.dst e] are not in [g]. ")
  let remove_edge_e: (t, edge) => unit
}

@ocaml.doc(" Signature for edges' weights. ")
module type WEIGHT = {
  @ocaml.doc(" Type for graph edges. ")
  type edge

  @ocaml.doc(" Type of edges' weights. ")
  type t

  @ocaml.doc(" Get the weight of an edge. ")
  let weight: edge => t

  @ocaml.doc(" Weights must be ordered. ")
  let compare: (t, t) => int

  @ocaml.doc(" Addition of weights. ")
  let add: (t, t) => t

  @ocaml.doc(" Neutral element for {!add}. ")
  let zero: t
}

@ocaml.doc(" Signature for marks on vertices. ")
module type MARK = {
  @ocaml.doc(" Type of graphs. ")
  type graph

  @ocaml.doc(" Type of graph vertices. ")
  type vertex

  @ocaml.doc(" [clear g] sets all the marks to 0 for all the vertices of [g]. ")
  let clear: graph => unit

  @ocaml.doc(" Mark value (in O(1)). ")
  let get: vertex => int

  @ocaml.doc(" Set the mark of the given vertex. ")
  let set: (vertex, int) => unit
}

@ocaml.doc(" Signature for imperative graphs with marks on vertices. ")
module type IM = {
  @ocaml.doc(" An imperative graph with marks is an imperative graph. ")
  include I

  @ocaml.doc(" Mark on vertices.
      Marks can be used if you want to store some information on vertices:
      it is more efficient to use marks than an external table. ")
  module Mark: MARK with type graph = t and type vertex = vertex
}

/*
Local Variables:
compile-command: "make -C .."
End:
*/
