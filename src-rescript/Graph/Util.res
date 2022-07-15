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

open Sig

module OTProduct = (X: ORDERED_TYPE, Y: ORDERED_TYPE) => {
  type t = (X.t, Y.t)
  let compare = ((x1, y1), (x2, y2)) => {
    let cv = X.compare(x1, x2)
    if cv !== 0 {
      cv
    } else {
      Y.compare(y1, y2)
    }
  }
}

module HTProduct = (X: HASHABLE, Y: HASHABLE) => {
  type t = (X.t, Y.t)
  let equal = ((x1, y1), (x2, y2)) => X.equal(x1, x2) && Y.equal(y1, y2)
  let hash = ((x, y)) => Hashtbl.hash((X.hash(x), Y.hash(y)))
}

module CMPProduct = (X: COMPARABLE, Y: COMPARABLE) => {
  include HTProduct(X, Y)
  include (
    OTProduct(X, Y): {
      let compare: (t, t) => int
    }
  )
}

module DataV = (
  L: {
    type t
  },
  V: Sig.COMPARABLE,
) => {
  type data = L.t
  type label = V.t
  type t = (ref<data>, V.t)
  let compare = ((_, x), (_, x')) => V.compare(x, x')
  let hash = ((_, x)) => V.hash(x)
  let equal = ((_, x), (_, x')) => V.equal(x, x')
  let create = (y, lbl) => (ref(y), lbl)
  let label = ((_, z)) => z
  let data = ((y, _)) => y.contents
  let set_data = ((y, _)) => \":="(y)
}

module Memo = (X: HASHABLE) => {
  module H = Hashtbl.Make(X)
  let memo = (~size=128, f) => {
    let h = H.create(size)
    x =>
      try H.find(h, x) catch {
      | Not_found =>
        let y = f(x)
        H.add(h, x, y)
        y
      }
  }
}
