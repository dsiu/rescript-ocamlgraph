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

  /* $Id: rand.mli,v 1.12 2005-03-31 13:32:51 filliatr Exp $ */

  " Random graph generation. "
)

@@ocaml.text(" {2 Random graphs} ")

module type S = {
  type graph
  type vertex
  type edge_label

  @ocaml.doc(" [graph v e] generates a random graph with exactly [v] vertices
      and [e] edges. Vertices are labeled with [0] ... [v-1].
      The boolean [loops] indicates whether loops are allowed;
      default value is no loop ([false]).
      @raise Invalid_argument if [e] exceeds the maximal number of edges. ")
  let graph: (~loops: bool=?, ~v: int, ~e: int, unit) => graph

  @ocaml.doc(" [labeled f] is similar to [graph] except that edges are labeled
      using function [f].
      @raise Invalid_argument if there are too many edges. ")
  let labeled: ((vertex, vertex) => edge_label, ~loops: bool=?, ~v: int, ~e: int, unit) => graph

  @@ocaml.text(" The two functions above actually make a choice between two
      different implementations according to the ratio e/(v*v).
      When this ratio is small, [random_few_edges] is selected;
      otherwise [random_many_edges] is selected. ")

  let random_few_edges: (~loops: bool, ~v: int, ~e: int) => graph
  let random_many_edges: (~loops: bool, ~v: int, ~e: int) => graph

  @ocaml.doc(" random graph using the G(n,p) model.
      [gnp v prob] generates a random graph with exactly [v] vertices
      and where each edge is selected with probability [prob] ")
  let gnp: (~loops: bool=?, ~v: int, ~prob: float, unit) => graph

  @ocaml.doc(" [gnp_labeled add_edge v e] is similar to [gnp] except that edges
        are labeled using function [f]. ")
  let gnp_labeled: (
    (vertex, vertex) => edge_label,
    ~loops: bool=?,
    ~v: int,
    ~prob: float,
    unit,
  ) => graph
}

@ocaml.doc(" Random graphs ")
module Make: (B: Builder.INT) =>
(S with type graph = B.G.t and type vertex = B.G.V.t and type edge_label = B.G.E.label)

@ocaml.doc(" Random persistent graphs ")
module P: (G: Sig.P with type V.label = int) =>
(S with type graph = G.t and type vertex = G.V.t and type edge_label = G.E.label)

@ocaml.doc(" Random imperative graphs ")
module I: (G: Sig.I with type V.label = int) =>
(S with type graph = G.t and type vertex = G.V.t and type edge_label = G.E.label)

@@ocaml.text(" {2 Random planar graphs} ")

module Planar: {
  module type S = {
    type graph

    @ocaml.doc(" [graph xrange yrange prob v]
          generates a random planar graph with exactly [v] vertices.
          Vertices are labeled with integer coordinates, randomly distributed
          according to [xrange] and [yrange].
          Edges are built as follows: the full Delaunay triangulation is
          constructed and then each edge is discarded with probabiblity [prob]
          (which should lie in [0..1]). In particular [prob = 0.0] gives the
          full triangulation.
          Edges are labeled with the (rounded) Euclidean distance between
          the two vertices.
          The boolean [loops] indicates whether loops are allowed;
          default value is no loop ([false]). ")
    let graph: (
      ~loops: bool=?,
      ~xrange: (int, int),
      ~yrange: (int, int),
      ~prob: float,
      int,
    ) => graph
  }

  @ocaml.doc(" Random planar graphs ")
  module Make: (B: Builder.S with type G.V.label = (int, int) and type G.E.label = int) =>
  (S with type graph = B.G.t)

  @ocaml.doc(" Random persistent planar graphs ")
  module P: (G: Sig.P with type V.label = (int, int) and type E.label = int) =>
  (S with type graph = G.t)

  @ocaml.doc(" Random imperative planar graphs ")
  module I: (G: Sig.I with type V.label = (int, int) and type E.label = int) =>
  (S with type graph = G.t)
}
