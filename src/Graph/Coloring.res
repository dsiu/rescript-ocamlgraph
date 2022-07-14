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

module type GM = {
  let is_directed: bool
  type t
  let nb_vertex: t => int
  module V: Sig.COMPARABLE
  let out_degree: (t, V.t) => int
  let iter_vertex: (V.t => unit, t) => unit
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let iter_succ: (V.t => unit, t, V.t) => unit
  let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
  module Mark: {
    let get: V.t => int
    let set: (V.t, int) => unit
  }
}

exception NoColoring

@ocaml.doc(" Graph coloring with marking.
    Only applies to imperative graphs with marks. ")
module Mark = (G: GM) => {
  module Bfs = Traverse.Bfs(G)

  let coloring = (g, k) => {
    if G.is_directed {
      invalid_arg("coloring: directed graph")
    }
    /* first step: we eliminate vertices with less than [k] successors */
    let stack = Stack.create()
    let nb_to_color = ref(G.nb_vertex(g))
    let count = ref(1)
    while count.contents > 0 {
      count := 0
      let erase = v => {
        incr(count)
        G.Mark.set(v, k + 1)
        Stack.push(v, stack)
      }
      G.iter_vertex(v =>
        if G.Mark.get(v) == 0 && G.out_degree(g, v) < k {
          erase(v)
        }
      , g)
      /* Format.printf "eliminating %d nodes@." !count; */
      nb_to_color := nb_to_color.contents - count.contents
    }
    /* second step: we k-color the remaining of the graph */
    /* [try_color v i] tries to assign color [i] to vertex [v] */
    let try_color = (v, i) => {
      G.Mark.set(v, i)
      G.iter_succ(w =>
        if G.Mark.get(w) == i {
          raise(NoColoring)
        }
      , g, v)
    }

    let uncolor = v => G.Mark.set(v, 0)
    if nb_to_color.contents > 0 {
      let rec iterate = iter => {
        let v = Bfs.get(iter)
        let m = G.Mark.get(v)
        if m > 0 {
          iterate(Bfs.step(iter))
        } else {
          for i in 1 to k {
            try {
              try_color(v, i)
              iterate(Bfs.step(iter))
            } catch {
            | NoColoring => ()
            }
          }
          uncolor(v)
          raise(NoColoring)
        }
      }

      try iterate(Bfs.start(g)) catch {
      | Exit => ()
      }
    }
    /* third step: we color the eliminated vertices, in reverse order */
    Stack.iter(v =>
      try {
        for i in 1 to k {
          try {
            try_color(v, i)
            raise(Exit)
          } catch {
          | NoColoring => ()
          }
        }
        raise(NoColoring) /* it may still fail on a self edge v->v */
      } catch {
      | Exit => ()
      }
    , stack)
  }

  let two_color = g => {
    if G.is_directed {
      invalid_arg("coloring: directed graph")
    }
    /* first, set all colors to 0 */
    let erase = v => G.Mark.set(v, 0)
    G.iter_vertex(erase, g)
    /* then, use dfs to color the nodes */
    let rec dfs = (c, v) =>
      switch G.Mark.get(v) {
      | (1 | 2) as cv =>
        if cv != c {
          raise(NoColoring)
        } /* check for cycles */
      | _ =>
        G.Mark.set(v, c)
        G.iter_succ(dfs(1 - c), g, v)
      }
    let start = v =>
      switch G.Mark.get(v) {
      | 1 | 2 => ()
      | _ => dfs(1, v)
      }
    G.iter_vertex(start, g)
  }
}

@@ocaml.text(" Graph coloring for graphs without marks: we use an external hash table ")

module type G = {
  let is_directed: bool
  type t
  let nb_vertex: t => int
  module V: Sig.COMPARABLE
  let out_degree: (t, V.t) => int
  let iter_vertex: (V.t => unit, t) => unit
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let iter_succ: (V.t => unit, t, V.t) => unit
  let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
}

module Make = (G: G) => {
  module H = Hashtbl.Make(G.V)

  let add_marks = () => {
    let h = H.create(97)
    (
      h,
      module(
        {
          include G
          module Mark = {
            let get = v =>
              try H.find(h, v) catch {
              | Not_found => 0
              }
            let set = (v, n) => H.replace(h, v, n)
          }
        }: GM with type t = G.t and type V.t = G.V.t
      ),
    )
  }

  let coloring = (g, k) => {
    let (h, module(GM)) = add_marks()
    module M = Mark(GM)
    M.coloring(g, k)
    h
  }

  let two_color = g => {
    let (h, module(GM)) = add_marks()
    module M = Mark(GM)
    M.two_color(g)
    h
  }
}
