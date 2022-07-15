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

  /* $Id: delaunay.mli,v 1.8 2004-02-20 14:37:40 signoles Exp $ */

  " Delaunay triangulation. "
)

@ocaml.doc(" Delaunay triangulation is available for any CCC system in the sense
    of Knuth's ``Axioms and Hulls'' ")
module type CCC = {
  type point

  @ocaml.doc(" The counterclockwise relation [ccw p q r] states that the
      circle through points [(p,q,r)] is traversed counterclockwise
      when we encounter the points in cyclic order [p,q,r,p,...] *")
  let ccw: (point, point, point) => bool

  @ocaml.doc(" The relation [in_circle p q r s] states that [s] lies
      inside the circle [(p,q,r)] if [ccw p q r] is true, or outside that
      circle if [ccw p q r] is false. ")
  let in_circle: (point, point, point, point) => bool
}

@ocaml.doc(" The result of triangulation is an abstract value of type [triangulation].
    Then one can iterate over all edges of the triangulation. ")
module type Triangulation = {
  module S: CCC

  type triangulation

  @ocaml.doc(" [triangulate a] computes the Delaunay triangulation of a set of
      points, given as an array [a]. If [N] is the number of points
      (that is [Array.length a]), then the running time is $O(N \log N)$
      on the average and $O(N^2)$ on the worst-case. The space used is
      always $O(N)$. ")
  let triangulate: array<S.point> => triangulation

  @ocaml.doc(" [iter f t] iterates over all edges of the triangulation [t].
      [f u v] is called once for each undirected edge [(u,v)]. ")
  let iter: ((S.point, S.point) => unit, triangulation) => unit

  let fold: ((S.point, S.point, 'a) => 'a, triangulation, 'a) => 'a

  let iter_triangles: ((S.point, S.point, S.point) => unit, triangulation) => unit
}

@ocaml.doc(" Generic Delaunay triangulation ")
module Make: (S: CCC) => (Triangulation with module S = S)

@ocaml.doc(" Points with integer coordinates ")
module IntPoints: CCC with type point = (int, int)

@ocaml.doc(" Delaunay triangulation with integer coordinates ")
module Int: Triangulation with module S = IntPoints

@ocaml.doc(" Points with floating point coordinates ")
module FloatPoints: CCC with type point = (float, float)

@ocaml.doc(" Delaunay triangulation with floating point coordinates ")
module Float: Triangulation with module S = FloatPoints
