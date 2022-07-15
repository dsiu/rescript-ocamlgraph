/* ************************************************************************ */
/*  */
/* Ocamlgraph: a generic graph library for OCaml */
/* Copyright (C) 2004-2007 */
/* Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles */
/*  */
/* This software is free software; you can redistribute it and/or */
/* modify it under the terms of the GNU Library General Public */
/* License version 2, with the special exception on linking */
/* described in file LICENSE. */
/*  */
/* This software is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. */
/*  */
/* ************************************************************************ */
@@warning("-3-44")

open Format

module C = Components.Undirected(Pack.Graph)

open Pack.Graph

let () = {
  Random.init(42)
  let g = Rand.graph(~v=10, ~e=3, ())
  let (n, f) = C.components(g)
  printf("%d components@.", n)
  iter_vertex(v => printf("%d -> %d@.", V.label(v), f(v)), g)
}

/*
Local Variables:
compile-command: "ocaml -I .. graph.cma test_components.ml"
End:
*/
