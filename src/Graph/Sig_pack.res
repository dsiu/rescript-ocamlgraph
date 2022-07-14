@@ocaml.text(/* ************************************************************************ */
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

/* $Id: sig_pack.mli,v 1.23 2005-07-18 07:10:35 filliatr Exp $ */

" Immediate access to the library: contain a signature gathering an
    imperative graph signature and all algorithms.
    Vertices and edges are labeled with integers. ")

@ocaml.doc(" Signature gathering an imperative graph signature and all algorithms.
    Vertices and edges are labeled with integers. ")
module type S = {
  @@ocaml.text(" {2 Graph structure} ")

  @ocaml.doc(" abstract type of graphs ")
  type t

  @ocaml.doc(" Vertices ")
  module V: {
    @@ocaml.text(" Vertices are [COMPARABLE] ")

    type t
    let compare: (t, t) => int
    let hash: t => int
    let equal: (t, t) => bool

    @@ocaml.text(" vertices are labeled with integers ")

    type label = int
    let create: label => t
    let label: t => label
  }

  type vertex = V.t

  @ocaml.doc(" Edges ")
  module E: {
    @@ocaml.text(" Edges are [ORDERED]. ")

    type t
    let compare: (t, t) => int

    @@ocaml.text(" Edges are directed. ")

    let src: t => V.t
    let dst: t => V.t

    @@ocaml.text(" Edges are labeled with integers. ")

    type label = int
    @ocaml.doc(" [create v1 l v2] creates an edge from [v1] to [v2] with label [l] ")
    let create: (V.t, label, V.t) => t

    let label: t => label

    type vertex = V.t
  }

  type edge = E.t

  @ocaml.doc(" is this an implementation of directed graphs? ")
  let is_directed: bool

  @@ocaml.text(" {2 Graph constructors and destructors} ")

  @ocaml.doc(" Return an empty graph. Optionally, a size can be
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

  @ocaml.doc(" [add_vertex g v] adds the vertex [v] from the graph [g].
      Do nothing if [v] is already in [g]. ")
  let add_vertex: (t, V.t) => unit

  @ocaml.doc(" [remove g v] removes the vertex [v] from the graph [g]
      (and all the edges going from [v] in [g]).
      Do nothing if [v] is not in [g]. ")
  let remove_vertex: (t, V.t) => unit

  @ocaml.doc(" [add_edge g v1 v2] adds an edge from the vertex [v1] to the vertex [v2]
      in the graph [g].
      Add also [v1] (resp. [v2]) in [g] if [v1] (resp. [v2]) is not in [g].
      Do nothing if this edge is already in [g]. ")
  let add_edge: (t, V.t, V.t) => unit

  @ocaml.doc(" [add_edge_e g e] adds the edge [e] in the graph [g].
      Add also [E.src e] (resp. [E.dst e]) in [g] if [E.src e] (resp. [E.dst
      e]) is not in [g].
      Do nothing if [e] is already in [g]. ")
  let add_edge_e: (t, E.t) => unit

  @ocaml.doc(" [remove_edge g v1 v2] removes the edge going from [v1] to [v2] from the
      graph [g].
      Do nothing if this edge is not in [g].
      @raise Invalid_argument if [v1] or [v2] are not in [g]. ")
  let remove_edge: (t, V.t, V.t) => unit

  @ocaml.doc(" [remove_edge_e g e] removes the edge [e] from the graph [g].
      Do nothing if [e] is not in [g].
      @raise Invalid_argument if [E.src e] or [E.dst e] are not in [g]. ")
  let remove_edge_e: (t, E.t) => unit

  @ocaml.doc(" Vertices contains integers marks, which can be set or used by some
      algorithms (see for instance module [Marking] below) ")
  module Mark: {
    type graph = t
    type vertex = V.t
    @ocaml.doc(" [clear g] sets all marks to 0 from all the vertives of [g]. ")
    let clear: t => unit

    let get: V.t => int
    let set: (V.t, int) => unit
  }

  @@ocaml.text(" {2 Size functions} ")

  let is_empty: t => bool
  let nb_vertex: t => int
  let nb_edges: t => int

  @@ocaml.text(" Degree of a vertex ")

  @ocaml.doc(" [out_degree g v] returns the out-degree of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let out_degree: (t, V.t) => int

  @ocaml.doc(" [in_degree g v] returns the in-degree of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let in_degree: (t, V.t) => int

  @@ocaml.text(" {2 Membership functions} ")

  let mem_vertex: (t, V.t) => bool
  let mem_edge: (t, V.t, V.t) => bool
  let mem_edge_e: (t, E.t) => bool
  let find_edge: (t, V.t, V.t) => E.t
  let find_all_edges: (t, V.t, V.t) => list<E.t>

  @@ocaml.text(" {2 Successors and predecessors of a vertex} ")

  @ocaml.doc(" [succ g v] returns the successors of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let succ: (t, V.t) => list<V.t>

  @ocaml.doc(" [pred g v] returns the predecessors of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let pred: (t, V.t) => list<V.t>

  @@ocaml.text(" Labeled edges going from/to a vertex ")

  @ocaml.doc(" [succ_e g v] returns the edges going from [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let succ_e: (t, V.t) => list<E.t>

  @ocaml.doc(" [pred_e g v] returns the edges going to [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. ")
  let pred_e: (t, V.t) => list<E.t>

  @@ocaml.text(" {2 Graph iterators} ")

  @@ocaml.text(" iter/fold on all vertices/edges of a graph ")

  let iter_vertex: (V.t => unit, t) => unit
  let iter_edges: ((V.t, V.t) => unit, t) => unit
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let fold_edges: ((V.t, V.t, 'a) => 'a, t, 'a) => 'a

  @ocaml.doc(" map iterator on vertex ")
  let map_vertex: (V.t => V.t, t) => t

  @@ocaml.text(" iter/fold on all labeled edges of a graph ")

  let iter_edges_e: (E.t => unit, t) => unit
  let fold_edges_e: ((E.t, 'a) => 'a, t, 'a) => 'a

  @@ocaml.text(" {2 Vertex iterators}

      Each iterator [iterator f v g] iters [f] to the successors/predecessors
      of [v] in the graph [g] and raises [Invalid_argument] if [v] is not in
      [g]. ")

  @@ocaml.text(" iter/fold on all successors/predecessors of a vertex. ")

  let iter_succ: (V.t => unit, t, V.t) => unit
  let iter_pred: (V.t => unit, t, V.t) => unit
  let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
  let fold_pred: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a

  @@ocaml.text(" iter/fold on all edges going from/to a vertex. ")

  let iter_succ_e: (E.t => unit, t, V.t) => unit
  let fold_succ_e: ((E.t, 'a) => 'a, t, V.t, 'a) => 'a
  let iter_pred_e: (E.t => unit, t, V.t) => unit
  let fold_pred_e: ((E.t, 'a) => 'a, t, V.t, 'a) => 'a

  @@ocaml.text(" {2 Basic operations} ")

  @ocaml.doc(" [vertex g i] returns a vertex of label [i] in [g]. The behaviour is
      unspecified if [g] has several vertices with label [i].
      Note: this function is inefficient (linear in the number of vertices);
      you should better keep the vertices as long as you create them. ")
  let find_vertex: (t, int) => V.t

  @ocaml.doc(" [transitive_closure ?reflexive g] returns the transitive closure
      of [g] (as a new graph). Loops (i.e. edges from a vertex to itself)
      are added only if [reflexive] is [true] (default is [false]). ")
  let transitive_closure: (~reflexive: bool=?, t) => t

  @ocaml.doc(" [add_transitive_closure ?reflexive g] replaces [g] by its
      transitive closure. Meaningless for persistent implementations
      (then acts as [transitive_closure]). ")
  let add_transitive_closure: (~reflexive: bool=?, t) => t

  @ocaml.doc(" [transitive_reduction ?reflexive g] returns the transitive reduction
      of [g] (as a new graph). Loops (i.e. edges from a vertex to itself)
      are removed only if [reflexive] is [true] (default is [false]). ")
  let transitive_reduction: (~reflexive: bool=?, t) => t

  @ocaml.doc(" [replace_by_transitive_reduction ?reflexive g] replaces [g] by its
      transitive reduction. Meaningless for persistent implementations
      (then acts as [transitive_reduction]). ")
  let replace_by_transitive_reduction: (~reflexive: bool=?, t) => t

  @ocaml.doc(" [mirror g] returns a new graph which is the mirror image of [g]:
      each edge from [u] to [v] has been replaced by an edge from [v] to [u].
      For undirected graphs, it simply returns a copy of [g]. ")
  let mirror: t => t

  @ocaml.doc(" [complement g] builds a new graph which is the complement of [g]:
      each edge present in [g] is not present in the resulting graph and
      vice-versa. Edges of the returned graph are unlabeled. ")
  let complement: t => t

  @ocaml.doc(" [intersect g1 g2] returns a new graph which is the intersection of [g1]
      and [g2]: each vertex and edge present in [g1] *and* [g2] is present
      in the resulting graph. ")
  let intersect: (t, t) => t

  @ocaml.doc(" [union g1 g2] returns a new graph which is the union of [g1] and [g2]:
      each vertex and edge present in [g1] *or* [g2] is present in the
      resulting graph. ")
  let union: (t, t) => t

  @@ocaml.text(" {2 Traversal} ")

  @ocaml.doc(" Depth-first search ")
  module Dfs: {
    @ocaml.doc(" [iter pre post g] visits all nodes of [g] in depth-first search,
        applying [pre] to each visited node before its successors,
        and [post] after them. Each node is visited exactly once. ")
    let iter: (~pre: V.t => unit=?, ~post: V.t => unit=?, t) => unit

    @ocaml.doc(" applies only a prefix function ")
    let prefix: (V.t => unit, t) => unit

    @ocaml.doc(" applies only a postfix function ")
    let postfix: (V.t => unit, t) => unit

    let fold: ((V.t, 'a) => 'a, 'a, t) => 'a

    @@ocaml.text(" Same thing, but for a single connected component ")

    let iter_component: (~pre: V.t => unit=?, ~post: V.t => unit=?, t, V.t) => unit
    let prefix_component: (V.t => unit, t, V.t) => unit
    let postfix_component: (V.t => unit, t, V.t) => unit
    let fold_component: ((V.t, 'a) => 'a, 'a, t, V.t) => 'a

    let has_cycle: t => bool
  }

  @ocaml.doc(" Breadth-first search ")
  module Bfs: {
    let iter: (V.t => unit, t) => unit
    let iter_component: (V.t => unit, t, V.t) => unit
  }

  @ocaml.doc(" Graph traversal with marking ")
  module Marking: {
    let dfs: t => unit
    let has_cycle: t => bool
  }

  @ocaml.doc(" Coloring ")
  module Coloring: {
    @ocaml.doc(" [coloring g k] colors the nodes of graph [g] using [k] colors,
          assigning the marks integer values between 1 and [k]. ")
    let coloring: (t, int) => unit

    @ocaml.doc(" [two_color g] attemps to color [g] with colors 1 and 2. ")
    let two_color: t => unit
  }

  @@ocaml.text(" {2 Graph generators} ")

  @ocaml.doc(" Classic graphs ")
  module Classic: {
    @ocaml.doc(" [divisors n] builds the graph of divisors.
        Vertices are integers from [2] to [n]. [i] is connected to [j] if
        and only if [i] divides [j].
        @raise Invalid_argument is [n < 2]. ")
    let divisors: int => t

    @ocaml.doc(" [de_bruijn n] builds the de Bruijn graph of order [n].
        Vertices are bit sequences of length [n] (encoded as their
        interpretation as binary integers). The sequence [xw] is connected
        to the sequence [wy] for any bits [x] and [y] and any bit sequence
        [w] of length [n-1].
        @raise Invalid_argument is [n < 1] or [n > Sys.word_size-1]. ")
    let de_bruijn: int => t

    @ocaml.doc(" [vertex_only n] builds a graph with [n] vertices and no edge. ")
    let vertex_only: int => t

    @ocaml.doc(" [full n] builds a graph with [n] vertices and all possible edges.
        The optional argument [self] indicates if loop edges should be added
        (default value is [true]). ")
    let full: (~self: bool=?, int) => t

    @ocaml.doc(" [cycle n] builds a graph that is a cycle with [n] vertices.
        Vertices are labelled with 0,1,...,n-1 and there is an edge from
        vertex [i] to vertex [(i+1) mod n].
        Vertices are also returned in an array for convenience. ")
    let cycle: int => (t, array<V.t>)

    @ocaml.doc(" [grid n m] builds a grid graph with [n*m] vertices, with edges
        from vertex [(i,j)] to vertices [(i+1,j)] and [(i,j+1)] (and no
        wrapping around). Vertex [(i,j)] is labelled with [i*m+j].
        Vertices are also returned in a [n*m] matrix for convenience. ")
    let grid: (~n: int, ~m: int) => (t, array<array<V.t>>)
  }

  @ocaml.doc(" Random graphs ")
  module Rand: {
    @ocaml.doc(" [random v e] generates a random with [v] vertices and [e] edges. ")
    let graph: (~loops: bool=?, ~v: int, ~e: int, unit) => t

    @ocaml.doc(" [random_labeled f] is similar to [random] except that edges are
            labeled using function [f] ")
    let labeled: ((V.t, V.t) => E.label, ~loops: bool=?, ~v: int, ~e: int, unit) => t

    @ocaml.doc(" [gnp v prob] generates a random graph with [v] vertices and
        where each edge is selected with probality [prob] (G(n,p) model) ")
    let gnp: (~loops: bool=?, ~v: int, ~prob: float, unit) => t

    @ocaml.doc(" [gnp_labeled add_edge v prob] is similar to [gnp] except that
          edges are labeled using function [f] ")
    let gnp_labeled: ((V.t, V.t) => E.label, ~loops: bool=?, ~v: int, ~prob: float, unit) => t
  }

  @ocaml.doc(" Strongly connected components ")
  module Components: {
    @ocaml.doc(" strongly connected components ")
    let scc: t => (int, V.t => int)

    let scc_array: t => array<list<V.t>>
    let scc_list: t => list<list<V.t>>
  }

  @@ocaml.text(" {2 Classical algorithms} ")

  @ocaml.doc(" Dijkstra's shortest path algorithm. Weights are the labels. ")
  let shortest_path: (t, V.t, V.t) => (list<E.t>, int)

  @ocaml.doc(" Ford Fulkerson maximum flow algorithm ")
  let ford_fulkerson: (t, V.t, V.t) => (E.t => int, int)

  @ocaml.doc(" Goldberg-Tarjan maximum flow algorithm ")
  let goldberg_tarjan: (t, V.t, V.t) => (E.t => int, int)

  @ocaml.doc(" [bellman_ford g v] finds a negative cycle from [v], and returns it,
      or raises [Not_found] if there is no such cycle ")
  let bellman_ford: (t, V.t) => list<E.t>

  @ocaml.doc(" Path checking ")
  module PathCheck: {
    type path_checker
    let create: t => path_checker
    let check_path: (path_checker, V.t, V.t) => bool
  }

  @ocaml.doc(" Topological order ")
  module Topological: {
    let fold: ((V.t, 'a) => 'a, t, 'a) => 'a
    let iter: (V.t => unit, t) => unit

    let fold_stable: ((V.t, 'a) => 'a, t, 'a) => 'a
    let iter_stable: (V.t => unit, t) => unit
  }

  @ocaml.doc(" Eulerian path ")
  module Eulerian: {
    @ocaml.doc(" [path g] returns an Eulerian path of g. The Boolean indicates
          whether the path is a cycle. Raises [Invalid_argument] if there is
          no Eulerian path. ")
    let path: t => (list<E.t>, bool)

    let cycle: t => list<E.t>
  }

  @ocaml.doc(" Kruskal algorithm ")
  let spanningtree: t => list<E.t>

  @@ocaml.text(" {2 Input / Output} ")

  @ocaml.doc(" DOT output in a file ")
  let dot_output: (t, string) => unit

  @ocaml.doc(" Displays the given graph using the external tools \"dot\" and \"gv\"
      and returns when gv's window is closed ")
  let display_with_gv: t => unit

  let parse_gml_file: string => t
  let parse_dot_file: string => t

  @@warning("-3")
  let print_gml: (Format.formatter, t) => unit
  let print_gml_file: (t, string) => unit
  /* val print_graphml : Format.formatter -> t -> unit */
}
