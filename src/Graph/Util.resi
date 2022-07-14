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

  " Some useful operations. "
)

open Sig

@ocaml.doc(" Cartesian product of two ordered types. ")
module OTProduct: (X: ORDERED_TYPE, Y: ORDERED_TYPE) => (ORDERED_TYPE with type t = (X.t, Y.t))

@ocaml.doc(" Cartesian product of two hashable types. ")
module HTProduct: (X: HASHABLE, Y: HASHABLE) => (HASHABLE with type t = (X.t, Y.t))

@ocaml.doc(" Cartesian product of two comparable types. ")
module CMPProduct: (X: COMPARABLE, Y: COMPARABLE) => (COMPARABLE with type t = (X.t, Y.t))

@ocaml.doc(" Create a vertex type with some data attached to it ")
module DataV: (
  L: {
    type t
  },
  V: Sig.COMPARABLE,
) =>
{
  type rec data = L.t
  and label = V.t
  and t = (ref<data>, V.t)
  let compare: (t, t) => int
  let hash: t => int
  let equal: (t, t) => bool
  let create: (data, V.t) => t
  let label: t => V.t
  let data: t => data
  let set_data: (t, data) => unit
}

module Memo: (X: HASHABLE) =>
{
  let memo: (~size: int=?, X.t => 'a, X.t) => 'a
}
