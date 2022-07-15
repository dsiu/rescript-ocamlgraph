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

module type Ordered = {
  type t
  let compare: (t, t) => int
}

exception EmptyHeap

module Imperative = (X: Ordered) => {
  /* The heap is encoded in the array [data], where elements are stored
     from [0] to [size - 1]. From an element stored at [i], the left
     (resp. right) subtree, if any, is rooted at [2*i+1] (resp. [2*i+2]). */

  type t = {mutable size: int, mutable data: array<X.t>}

  /* When [create n] is called, we cannot allocate the array, since there is
     no known value of type [X.t]; we'll wait for the first addition to
     do it, and we remember this situation with a negative size. */

  let create = n => {
    if n <= 0 {
      invalid_arg("create")
    }
    {size: -n, data: []}
  }

  let is_empty = h => h.size <= 0

  /* [resize] doubles the size of [data] */

  let resize = h => {
    let n = h.size
    assert (n > 0)
    let n' = 2 * n
    let d = h.data
    let d' = Array.make(n', d[0])
    Array.blit(d, 0, d', 0, n)
    h.data = d'
  }

  let add = (h, x) => {
    /* first addition: we allocate the array */
    if h.size < 0 {
      h.data = Array.make(-h.size, x)
      h.size = 0
    }
    let n = h.size
    /* resizing if needed */
    if n === Array.length(h.data) {
      resize(h)
    }
    let d = h.data
    /* moving [x] up in the heap */
    let rec moveup = i => {
      let fi = (i - 1) / 2
      if i > 0 && X.compare(d[fi], x) < 0 {
        d[i] = d[fi]
        moveup(fi)
      } else {
        d[i] = x
      }
    }

    moveup(n)
    h.size = n + 1
  }

  let maximum = h => {
    if h.size <= 0 {
      raise(EmptyHeap)
    }
    h.data[0]
  }

  let remove = h => {
    if h.size <= 0 {
      raise(EmptyHeap)
    }
    let n = h.size - 1
    h.size = n
    let d = h.data
    let x = d[n]
    /* moving [x] down in the heap */
    let rec movedown = i => {
      let j = 2 * i + 1
      if j < n {
        let j = {
          let j' = j + 1
          if j' < n && X.compare(d[j'], d[j]) > 0 {
            j'
          } else {
            j
          }
        }

        if X.compare(d[j], x) > 0 {
          d[i] = d[j]
          movedown(j)
        } else {
          d[i] = x
        }
      } else {
        d[i] = x
      }
    }

    movedown(0)
  }

  let pop_maximum = h => {
    let m = maximum(h)
    remove(h)
    m
  }

  let iter = (f, h) => {
    let d = h.data
    for i in 0 to h.size - 1 {
      f(d[i])
    }
  }

  let fold = (f, h, x0) => {
    let n = h.size
    let d = h.data
    let rec foldrec = (x, i) =>
      if i >= n {
        x
      } else {
        foldrec(f(d[i], x), succ(i))
      }

    foldrec(x0, 0)
  }
}
