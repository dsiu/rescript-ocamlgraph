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

/* Unionfind structure over hash-ordered types.

   This module implements a unionfind data structure, given a total ordering
   function and a hash function over the elements.

*/

module type HashedOrderedType = {
  /* The type of the elements */
  type t
  let equal: (t, t) => bool
  let hash: t => int
  let compare: (t, t) => int
}

/* Input signature of the functor Unionfind.Make */

module type S = {
  type elt
  type t

  let init: list<elt> => t
  let find: (elt, t) => elt
  let union: (elt, elt, t) => unit
}

module Make: (X: HashedOrderedType) => (S with type elt = X.t)
