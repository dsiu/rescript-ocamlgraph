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

/* $Id: classic.ml,v 1.9 2004-02-02 08:11:14 filliatr Exp $ */

module type S = {
  type graph
  type vertex
  let divisors: int => graph
  let de_bruijn: int => graph
  let vertex_only: int => graph
  let full: (~self: bool=?, int) => graph
  let cycle: int => (graph, array<vertex>)
  let grid: (~n: int, ~m: int) => (graph, array<array<vertex>>)
}

module Generic = (B: Builder.INT) => {
  type graph = B.G.t

  type vertex = B.G.V.t

  let divisors = n => {
    if n < 2 {
      invalid_arg("divisors")
    }
    let v = Array.init(n + 1, i => B.G.V.create(i))
    let rec loop = (g, i) => {
      let sqrt_i = truncate(sqrt(float(i)))
      let rec loop_i = (g, d) =>
        if d > sqrt_i {
          g
        } else if mod(i, d) === 0 {
          loop_i(B.add_edge(B.add_edge(g, v[i / d], v[i]), v[d], v[i]), d + 1)
        } else {
          loop_i(g, succ(d))
        }

      if i > n {
        g
      } else {
        loop(loop_i(B.add_vertex(g, v[i]), 2), i + 1)
      }
    }

    loop(B.empty(), 2)
  }

  let fold_for = (i0, i1, f) => {
    let rec loop = (i, v) =>
      if i > i1 {
        v
      } else {
        loop(i + 1, f(v, i))
      }
    loop(i0)
  }

  let de_bruijn = n => {
    if n < 1 || n > Sys.word_size - 1 {
      invalid_arg("de_bruijn")
    }
    let v = Array.init(lsl(1, n), i => B.G.V.create(i))
    let all_1 = lsl(1, n) - 1 /* 11...1 */
    let g = fold_for(0, all_1, (g, i) => B.add_vertex(g, v[i]), B.empty())
    let rec loop = (g, i) =>
      if i > all_1 {
        g
      } else {
        let si = land(lsl(i, 1), all_1)
        let g = B.add_edge(g, v[i], v[si])
        let g = B.add_edge(g, v[i], v[lor(si, 1)])
        loop(g, i + 1)
      }

    loop(g, 0)
  }

  let vertex_only = n => fold_for(1, n, (g, i) => B.add_vertex(g, B.G.V.create(i)), B.empty())

  let full = (~self=true, n) => {
    let v = Array.init(n + 1, i => B.G.V.create(i))
    fold_for(
      1,
      n,
      (g, i) =>
        fold_for(
          1,
          n,
          (g, j) =>
            if self || i != j {
              B.add_edge(g, v[i], v[j])
            } else {
              g
            },
          g,
        ),
      fold_for(1, n, (g, i) => B.add_vertex(g, v[i]), B.empty()),
    )
  }

  let cycle = n => {
    if n < 0 {
      invalid_arg("cycle")
    }
    let v = Array.init(n, i => B.G.V.create(i))
    let g = Array.fold_left(B.add_vertex, B.empty(), v)
    let rec loop = (g, i) =>
      if i == n {
        g
      } else {
        let g = B.add_edge(g, v[i], v[mod(i + 1, n)])
        loop(g, i + 1)
      }
    (loop(g, 0), v)
  }

  let grid = (~n, ~m) => {
    if n < 0 || m < 0 {
      invalid_arg("grid")
    }
    let create = (i, j) => B.G.V.create(m * i + j)
    let v = Array.init(n, i => Array.init(m, j => create(i, j)))
    let g = Array.fold_left(Array.fold_left(B.add_vertex), B.empty(), v)
    let rec loop = (g, i, j) =>
      if i == n {
        g
      } else if j == m {
        loop(g, i + 1, 0)
      } else {
        let g = if j < m - 1 {
          B.add_edge(g, v[i][j], v[i][j + 1])
        } else {
          g
        }
        let g = if i < n - 1 {
          B.add_edge(g, v[i][j], v[i + 1][j])
        } else {
          g
        }
        loop(g, i, j + 1)
      }
    (loop(g, 0, 0), v)
  }
}

module P = (G: Sig.P with type V.label = int) => Generic(Builder.P(G))

module I = (G: Sig.I with type V.label = int) => Generic(Builder.I(G))
