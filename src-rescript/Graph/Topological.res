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

module type G = {
  type t
  module V: Sig.COMPARABLE
  let iter_vertex: (V.t => unit, t) => unit
  let iter_succ: (V.t => unit, t, V.t) => unit
}

module Make = (G: G) => {
  module Scc = Components.Make(G)

  let fold = (f, g, acc) => {
    /* build the graph of strongly-connected components */
    let (n, scc) = Scc.scc(g)
    let vertices = Array.make(n, list{})
    let edges = Array.make(n, list{})
    let degree = Array.make(n, 0) /* in-degree */
    let add_vertex = x => {
      let ix = scc(x)
      vertices[ix] = list{x, ...vertices[ix]}
      let add_edge = y => {
        let iy = scc(y)
        if ix != iy {
          edges[ix] = list{iy, ...edges[ix]}
          degree[iy] = degree[iy] + 1
        }
      }

      G.iter_succ(add_edge, g, x)
    }

    G.iter_vertex(add_vertex, g)
    /* standard topological sort on a DAG */
    let todo = Queue.create()
    let rec walk = acc =>
      if Queue.is_empty(todo) {
        acc
      } else {
        let i = Queue.pop(todo)
        let acc = List.fold_right(f, vertices[i], acc)
        List.iter(j => {
          let d = degree[j]
          assert (d > 0) /* no back edge */
          if d == 1 {
            Queue.push(j, todo)
          } else {
            degree[j] = d - 1
          }
        }, edges[i])
        walk(acc)
      }

    for i in 0 to n - 1 {
      if degree[i] == 0 {
        Queue.push(i, todo)
      }
    }
    walk(acc)
  }

  let iter = (f, g) => fold((v, ()) => f(v), g, ())
}

module Make_stable = (
  G: {
    include G
    let in_degree: (t, V.t) => int
  },
) => {
  module H = Hashtbl.Make(G.V)
  module C = Path.Check(G)

  let choose = (~old, (v, n): (G.V.t, int)) => {
    let (l, min) = old
    if n == min {
      (list{v, ...l}, n)
    } else if n < min {
      (list{v}, n)
    } else {
      old
    }
  }

  module Q = {
    module S = Set.Make(G.V)
    let create = () => ref(S.empty)
    let push = (v, s) => s := S.add(v, s.contents)
    let pop = s => {
      let r = S.min_elt(s.contents)
      s := S.remove(r, s.contents)
      r
    }
    let is_empty = s => S.is_empty(s.contents)
    let choose = (~old, new_) => {
      let (l, n) = choose(~old, new_)
      (List.sort(G.V.compare, l), n)
    }
  }

  /* in case of multiple cycles, choose one vertex in a cycle which
   does not depend of any other. */
  let find_top_cycle = (checker, vl) => {
    /* choose [v] if each other vertex [v'] is in the same cycle
       (a path from v to v') or is in a separate component
       (no path from v' to v).
       So, if there is a path from v' to without any path from v to v',
       discard v. */
    let on_top_cycle = v =>
      List.for_all(
        v' => G.V.equal(v, v') || (C.check_path(checker, v, v') || !C.check_path(checker, v', v)),
        vl,
      )

    List.filter(on_top_cycle, vl)
  }

  let fold = (f, g, acc) => {
    let checker = C.create(g)
    let degree = H.create(97)
    let todo = Q.create()
    let push = x => {
      H.remove(degree, x)
      Q.push(x, todo)
    }

    let rec walk = acc =>
      if Q.is_empty(todo) {
        /* let's find any node of minimal degree */
        let (min, _) = H.fold((v, d, old) => Q.choose(~old, (v, d)), degree, (list{}, max_int))

        switch min {
        | list{} => acc
        | _ =>
          let vl = find_top_cycle(checker, min)
          List.iter(push, vl)
          /* let v = choose_independent_vertex checker min in push v; */
          walk(acc)
        }
      } else {
        let v = Q.pop(todo)
        let acc = f(v, acc)
        G.iter_succ(x =>
          try {
            let d = H.find(degree, x)
            if d == 1 {
              push(x)
            } else {
              H.replace(degree, x, d - 1)
            }
          } catch {
          | Not_found => /* [x] already visited */
            ()
          }
        , g, v)
        walk(acc)
      }

    G.iter_vertex(v => {
      let d = G.in_degree(g, v)
      if d == 0 {
        Q.push(v, todo)
      } else {
        H.add(degree, v, d)
      }
    }, g)
    walk(acc)
  }

  let iter = (f, g) => fold((v, ()) => f(v), g, ())
}

/*
Local Variables:
compile-command: "make -C .."
End:
*/
