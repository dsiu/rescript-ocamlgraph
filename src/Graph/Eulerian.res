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
  let is_directed: bool
  module V: Sig.COMPARABLE
  module E: Sig.EDGE with type vertex = V.t
  let iter_edges_e: (E.t => unit, t) => unit
}

@@ocaml.text(" The following implements Hierholzer's algorithm.

    It is sketched as follows:

    1. make a round trip from a random vertex, by following random
       edges until we get back to the starting point (it will, as we
       first check that all vertices have even degrees).

    2. if any vertex along this cycle still has outgoing edges, pick one
       and make another round trip from it, and then join the two cycles
       into a single one. Repeat step 2 until all edges are exhausted.

    The implementation makes use of the following:

    - A table, called `out` in the following, that maps each vertex to
      outgoing edges not yet used in the Eulerian path.

    - In order to achieve optimal complexity, paths are built as
      doubly-linked lists, so that we can merge two cycles with a common
      vertex in constant time. This is type `dll` below.
")

module Make = (G: G) => {
  open G

  let rev = e => E.create(E.dst(e), E.label(e), E.src(e))

  module H = Hashtbl.Make(V)

  type out = H.t<H.t<E.t>>

  let add_out_edge = (out, x, y, e) => {
    let s = try H.find(out, x) catch {
    | Not_found =>
      let s = H.create(4)
      H.add(out, x, s)
      s
    }
    H.add(s, y, e)
  }

  @ocaml.doc(" compute the table of outgoing edges ")
  let setup = (g): (int, out) => {
    let nbe = ref(0)
    let out = H.create(16)
    let add = e => {
      incr(nbe)
      let x = E.src(e) and y = E.dst(e)
      add_out_edge(out, x, y, e)
      if !is_directed && !V.equal(x, y) {
        add_out_edge(out, y, x, rev(e))
      }
    }
    iter_edges_e(add, g)
    (nbe.contents, out)
  }

  exception Found(V.t)
  let any = h =>
    try {
      H.iter((v, _) => raise(Found(v)), h)
      assert false
    } catch {
    | Found(v) => (v, H.find(h, v))
    }

  type rec dll = {mutable prev: dll, edge: E.t, mutable next: dll}

  let remove_edge = (out, e) => {
    let remove = (h, x, y) => {
      let s = H.find(h, x)
      assert H.mem(s, y)
      H.remove(s, y)
      if H.length(s) == 0 {
        H.remove(h, x)
      }
    }
    let v = E.src(e) and w = E.dst(e)
    remove(out, v, w)
  }

  let self = e => V.equal(E.src(e), E.dst(e))

  let remove_edge = (edges, e) => {
    remove_edge(edges, e)
    if !is_directed && !self(e) {
      remove_edge(edges, rev(e))
    }
  }

  let any_out_edge = (out, v) => {
    assert H.mem(out, v)
    let s = H.find(out, v)
    assert (H.length(s) > 0)
    let (_, e) = any(s)
    remove_edge(out, e)
    e
  }

  @ocaml.doc(" build an arbitrary cycle from vertex [start] ")
  let round_trip = (edges, start) => {
    let e = any_out_edge(edges, start)
    let rec path = {prev: path, edge: e, next: path}
    let rec tour = e => {
      let v = E.dst(e.edge)
      if V.equal(v, start) {
        path.prev = e
        path
      } else {
        let e' = {prev: e, edge: any_out_edge(edges, v), next: path}
        e.next = e'
        tour(e')
      }
    }
    tour(path)
  }

  let connect = (e, e') => {
    e.next = e'
    e'.prev = e
  }

  @ocaml.doc(" build an Eulerian cycle from vertex [start] ")
  let eulerian_cycle = (out, start) => {
    let todos = H.create(8) /* vertex on cycle with out edges -> cycle edge */
    let todo = e => {
      let v = E.src(e.edge)
      if H.mem(out, v) {
        H.replace(todos, v, e)
      } else {
        H.remove(todos, v)
      }
    }
    let rec update = (start, e) => {
      todo(e)
      if !V.equal(E.dst(e.edge), start) {
        update(start, e.next)
      }
    }
    let path = round_trip(out, start)
    update(start, path)
    while H.length(todos) > 0 {
      let (v, e) = any(todos)
      H.remove(todos, v)
      assert H.mem(out, v)
      let e' = round_trip(out, v)
      update(v, e')
      let p = e.prev
      assert (p.next === e)
      let p' = e'.prev
      assert (p'.next === e')
      connect(p, e')
      connect(p', e)
    }
    path
  }

  let list_of = path => {
    let rec convert = (acc, e) =>
      if e === path {
        List.rev(acc)
      } else {
        convert(list{e.edge, ...acc}, e.next)
      }
    convert(list{path.edge}, path.next)
  }

  let mem_edge = (out, x, y) =>
    try H.mem(H.find(out, x), y) catch {
    | Not_found => false
    }

  let out_degree = (out, x) =>
    try H.length(H.find(out, x)) catch {
    | Not_found => 0
    }

  let undirected = g => {
    let (nbe, out) = setup(g)
    let odds = H.create(2)
    let check = (v, s) => {
      let d = H.length(s)
      let d = if H.mem(s, v) {
        d - 1
      } else {
        d
      }
      if mod(d, 2) == 1 {
        H.add(odds, v, ())
      }
    }
    H.iter(check, out)
    let n = H.length(odds)
    if n != 0 && n != 2 {
      invalid_arg("Eulerian.path (bad degrees)")
    }
    let cycle = n == 0
    let path = if cycle {
      if nbe == 0 {
        list{}
      } else {
        let (v, _) = any(out)
        list_of(eulerian_cycle(out, v))
      }
    } else {
      /* we have two vertices x and y with odd degrees */
      let (x, _) = any(odds)
      H.remove(odds, x)
      let (y, _) = any(odds)

      if mem_edge(out, x, y) {
        /* there is an edge x--y => it connects 1 or 2 Eulerian cycles */
        let xy = H.find(H.find(out, x), y)
        remove_edge(out, xy)
        switch (out_degree(out, x), out_degree(out, y)) {
        | (0, 0) => list{xy}
        | (_, 0) => list{rev(xy), ...list_of(eulerian_cycle(out, x))}
        | (0, _) => list{xy, ...list_of(eulerian_cycle(out, y))}
        | _ =>
          let py = eulerian_cycle(out, y)
          /* caveat: the cycle from y may exhaust edges from x */
          if out_degree(out, x) == 0 {
            list{xy, ...list_of(py)}
          } else {
            \"@"(list_of(eulerian_cycle(out, x)), list{xy, ...list_of(py)})
          }
        }
        /* a bit of a pity to use list concatenation here,
         but this does not change the complexity */
      } else {
        /* no edge x--y => add one, build a cycle, then remove it */
        let dummy = E.label(snd(any(H.find(out, x))))
        let xy = E.create(x, dummy, y)
        H.add(H.find(out, x), y, xy)
        H.add(H.find(out, y), x, rev(xy))
        let p = eulerian_cycle(out, x)
        let rec find = e => {
          /* lookup for x--y, to break the cycle there */
          let v = E.src(e.edge)
          and w = E.dst(e.edge)
          if (V.equal(v, x) && V.equal(w, y)) || (V.equal(v, y) && V.equal(w, x)) {
            e
          } else {
            find(e.next)
          }
        }
        let start = find(p)
        List.tl(list_of(start))
      }
    }

    /* check that all edges have been consumed */
    if H.length(out) > 0 {
      invalid_arg("Eulerian.path (not connected)")
    }
    (path, cycle)
  }

  let directed = g => {
    let delta = H.create(16) /* out - in */
    let add = (v, d) =>
      H.replace(
        delta,
        v,
        d + try H.find(delta, v) catch {
        | Not_found => 0
        },
      )
    let add = e => {
      add(E.src(e), 1)
      add(E.dst(e), -1)
    }
    iter_edges_e(add, g)
    let start = ref(None) and finish = ref(None)
    let check = (v, x) =>
      switch x {
      | 1 if start.contents == None => start := Some(v)
      | -1 if finish.contents == None => finish := Some(v)
      | 0 => ()
      | _ => invalid_arg("Eulerian.path (bad degrees)")
      }
    H.iter(check, delta)
    let (nbe, out) = setup(g)
    let (path, cycle) = switch (start.contents, finish.contents) {
    | (None, None) if nbe == 0 => (list{}, true)
    | (None, None) =>
      let (v, _) = any(out)
      (list_of(eulerian_cycle(out, v)), true)
    | (Some(s), Some(f)) =>
      /* add one edge f->s, build a cycle, then remove it
             note: there may be already an edge f->s
                   if so, we are adding *a second one* and we are careful
                   about removing this one, not the other */
      let dummy = E.label(snd(any(H.find(out, s))))
      let fs = E.create(f, dummy, s)
      add_out_edge(out, f, s, fs)
      let p = eulerian_cycle(out, s)
      let rec find = e =>
        /* lookup for f->s, to break the cycle there */
        if e.edge === fs {
          e
        } else {
          find(e.next)
        }
      let start = find(p)
      (List.tl(list_of(start)), false)
    | (Some(_), None)
    | (None, Some(_)) =>
      assert false
    } /* since the sum of all deltas is zero */

    /* check that all edges have been consumed */
    if H.length(out) > 0 {
      invalid_arg("Eulerian.path (not connected)")
    }
    (path, cycle)
  }

  let path = if is_directed {
    directed
  } else {
    undirected
  }

  let cycle = g => {
    let (p, c) = path(g)
    if !c {
      invalid_arg("Eulerian.cycle")
    }
    p
  }
}
