@@ocaml.text(/* ************************************************************************ */
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

/* $Id: delaunay.ml,v 1.12 2005-11-02 13:43:35 filliatr Exp $ */

" Code follows Don Knuth's algorithm
    from ``Axioms and hulls'' (LNCS 606, Springer-Verlag, 1992), pp. 73-77.

    Some code and comments are taken from the Stanford Graph Base,
    file [gb_plane].
")

module type CCC = {
  type point
  let ccw: (point, point, point) => bool
  let in_circle: (point, point, point, point) => bool
}

module type Triangulation = {
  module S: CCC
  type triangulation
  let triangulate: array<S.point> => triangulation
  let iter: ((S.point, S.point) => unit, triangulation) => unit
  let fold: ((S.point, S.point, 'a) => 'a, triangulation, 'a) => 'a
  let iter_triangles: ((S.point, S.point, S.point) => unit, triangulation) => unit
}

module Make = (S: CCC) => {
  module S = S

  type point = Point(int) | Infinity

  /* Each edge of the current triangulation is represented by two arcs
     pointing in opposite directions; the two arcs are called mates. Each
     arc conceptually has a triangle on its left and a mate on its right. */

  type rec arc = {
    mutable vert: point,
    /* v, if this arc goes from u to v */
    mutable next: arc,
    /* the arc from v that shares a triangle with this one */
    mutable inst: ref<node>,
    /* instruction to change when the triangle is modified */
    mate: int,
  }
  and node =
    | Branch(int, int, ref<node>, ref<node>)
    | Terminal(arc)

  type triangulation = {
    points: array<S.point>,
    arcs: array<arc>,
    last_used_arc: int,
  }

  let rec dummy_arc = {
    vert: Infinity,
    next: dummy_arc,
    inst: ref(Terminal(dummy_arc)),
    mate: -1,
  }

  let make_arc = (n, i) => {
    vert: Infinity,
    next: dummy_arc,
    inst: ref(Terminal(dummy_arc)),
    mate: 6 * n - 7 - i,
  }

  let finite = x =>
    switch x {
    | Point(p) => p
    | Infinity => assert false
    }

  /* [flip] will be used in both steps T4 and T5 */
  let flip = (c, d, e, t'', p, n, n') => {
    let e' = e.next
    let c' = c.next
    let c'' = c'.next
    e.next = c
    c.next = c''
    c''.next = e
    c''.inst = n
    c.inst = n
    e.inst = n
    c.vert = Point(p)
    d.next = e'
    e'.next = c'
    c'.next = d
    c'.inst = n'
    e'.inst = n'
    d.inst = n'
    d.vert = Point(t'')
  }

  let triangulate = points => {
    let ccw = (p, q, r) => S.ccw(points[p], points[q], points[r])
    let in_circle = (p, q, r, s) => S.in_circle(points[p], points[q], points[r], points[s])

    let n = Array.length(points)
    if n < 2 {
      invalid_arg("triangulate")
    }
    let arcs = Array.init(6 * n - 6, make_arc(n))
    let mate = i => 6 * n - 7 - i

    /* i DEBUG
      let rec dump d l =
      eprintf "%s" (String.make (2*d) ' ');
      match !l with
        | Terminal a ->
      eprintf "T %d\n" (mate a.mate)
        | Branch (u, v, l, r) ->
      eprintf "N %d %d\n" u v;
      dump (d+1) l;
      dump (d+1) r
      in
      i*/

    /* initialization:
     create a trivial triangulation for the first 2 vertices */
    let u = 0
    let v = 1
    let a1 = arcs[0]
    let a2 = arcs[1]
    let a3 = arcs[2]
    let b1 = arcs[mate(0)]
    let b2 = arcs[mate(1)]
    let b3 = arcs[mate(2)]
    let l1 = ref(Terminal(a2))
    let l2 = ref(Terminal(b3))
    a1.vert = Point(v)
    a1.next = a2
    a1.inst = l1
    a2.vert = Infinity
    a2.next = a3
    a2.inst = l1
    a3.vert = Point(u)
    a3.next = a1
    a3.inst = l1
    b1.vert = Point(u)
    b1.next = b3
    b1.inst = l2
    b2.vert = Point(v)
    b2.next = b1
    b2.inst = l2
    b3.vert = Infinity
    b3.next = b2
    b3.inst = l2
    let l0 = ref(Branch(u, v, l1, l2))
    let j = ref(2) /* last used arc */

    /* then for each new vertex [p] */
    for p in 2 to n - 1 {
      /* Step T1 */
      let rec step_T1 = (l, p) =>
        switch l.contents {
        | Terminal(al) => (l, al)
        | Branch(pl, ql, al, bl) =>
          step_T1(
            if ccw(pl, ql, p) {
              al
            } else {
              bl
            },
            p,
          )
        }

      let (l, al) = step_T1(l0, p)

      /* Step T2 */
      let a = al
      let b = a.next
      let c = b.next
      let q = a.vert
      let r = b.vert
      let s = c.vert
      j := j.contents + 3
      let aj = arcs[j.contents]
      let aj_1 = arcs[j.contents - 1]
      let aj_2 = arcs[j.contents - 2]
      let bj = arcs[aj.mate]
      let bj_1 = arcs[aj_1.mate]
      let bj_2 = arcs[aj_2.mate]
      let l' = ref(Terminal(a))
      let l'' = ref(Terminal(aj))
      let l''' = ref(Terminal(c))
      aj.vert = q
      aj.next = b
      aj.inst = l''
      aj_1.vert = r
      aj_1.next = c
      aj_1.inst = l'''
      aj_2.vert = s
      aj_2.next = a
      aj_2.inst = l'
      bj.vert = Point(p)
      bj.next = aj_2
      bj.inst = l'
      bj_1.vert = Point(p)
      bj_1.next = aj
      bj_1.inst = l''
      bj_2.vert = Point(p)
      bj_2.next = aj_1
      bj_2.inst = l'''
      a.next = bj
      a.inst = l'
      b.next = bj_1
      b.inst = l''
      c.next = bj_2
      c.inst = l'''
      let r = finite(r)
      let s = finite(s)

      /* steps T3 or T4 depending on [q] */
      let r = switch q {
      | Point(q) =>
        /* Step T3 */
        let n = ref(Branch(q, p, l', l''))
        let n' = ref(Branch(s, p, l''', l'))
        l := Branch(r, p, n, n')
        r
      | Infinity =>
        /* Step T4 */
        let n = ref(Branch(s, p, l''', l'))
        l := Branch(r, p, l'', n)
        let rec loop = (m, a, d, s, t) =>
          if t != r && ccw(p, s, t) {
            let n = ref(Terminal(d))
            switch m.contents {
            | Branch(mu, mv, ml, is_l') =>
              assert (is_l' === l')
              m := Branch(mu, mv, ml, d.inst)
              d.inst := Branch(t, p, n, l')
              let m = d.inst
              flip(a, arcs[a.mate], d, t, p, n, l')
              let a = arcs[a.mate].next
              let d = arcs[a.mate].next
              let s = t
              let t = finite(d.vert)
              l' := Terminal(a)
              loop(m, a, d, s, t)
            | Terminal(_) => assert false
            }
          } else {
            /* at exit of while loop */
            let n = ref(Terminal(d.next))
            d.inst := Branch(s, p, n, l')
            d.inst = n
            d.next.inst = n
            d.next.next.inst = n
            s
          }

        let d = arcs[a.mate].next
        loop(n, a, d, s, finite(d.vert))
      }

      /* Step T5 */
      let rec loop = c => {
        let d = arcs[c.mate]
        let e = d.next
        let t = finite(d.vert)
        let t' = finite(c.vert)
        let t'' = e.vert
        if t'' != Infinity && in_circle(finite(t''), t', t, p) {
          let t'' = finite(t'')
          let n = ref(Terminal(e))
          let n' = ref(Terminal(d))
          c.inst := Branch(t'', p, n, n')
          d.inst := Branch(t'', p, n, n')
          flip(c, d, e, t'', p, n, n')
          loop(e)
        } else if t' != r {
          loop(arcs[c.next.mate].next)
        } else {
          ()
        }
      } /* break */

      loop(c)
    }
    {points: points, arcs: arcs, last_used_arc: j.contents}
  }

  let iter = (f, t) => {
    let points = t.points
    let n = Array.length(t.arcs)
    for i in 0 to t.last_used_arc {
      switch (t.arcs[i].vert, t.arcs[n - 1 - i].vert) {
      | (Point(u), Point(v)) => f(points[u], points[v])
      | _ => ()
      }
    }
  }

  let iter_triangles = (f, t) => {
    let n = Array.length(t.arcs)
    let seen_arc = Array.make(n, false)
    let mate = i => n - 1 - i
    let index = a => mate(a.mate)
    for i in 0 to n - 1 {
      if !seen_arc[i] {
        let a1 = t.arcs[i]
        let a2 = a1.next
        let a3 = a2.next
        seen_arc[i] = true
        seen_arc[index(a2)] = true
        seen_arc[index(a3)] = true
        switch (a1.vert, a2.vert, a3.vert) {
        | (Point(i1), Point(i2), Point(i3)) => f(t.points[i1], t.points[i2], t.points[i3])
        | _ => ()
        }
      }
    }
  }

  let fold = (f, t, a) => {
    let points = t.points
    let n = Array.length(t.arcs)
    let rec loop = (i, a) =>
      if i <= t.last_used_arc {
        switch (t.arcs[i].vert, t.arcs[n - 1 - i].vert) {
        | (Point(u), Point(v)) => loop(succ(i), f(points[u], points[v], a))
        | _ => loop(succ(i), a)
        }
      } else {
        a
      }

    loop(0, a)
  }
}

@@ocaml.text(" Points with floating point coordinates ")

module FloatPoints = {
  type point = (float, float)

  let \"+" = \"+."
  let \"-" = \"-."
  let \"*" = \"*."

  let det = x =>
    switch x {
    | [[a00, a01], [a10, a11]] => a00 * a11 - a01 * a10
    | [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]] =>
      a00 * a11 * a22 -
      a00 * a12 * a21 -
      a10 * a01 * a22 +
      a10 * a02 * a21 +
      a20 * a01 * a12 -
      a20 * a02 * a11
    | [[a00, a01, a02, a03], [a10, a11, a12, a13], [a20, a21, a22, a23], [a30, a31, a32, a33]] =>
      a00 * a11 * a22 * a33 -
      a00 * a11 * a23 * a32 -
      a00 * a21 * a12 * a33 +
      a00 * a21 * a13 * a32 +
      a00 * a31 * a12 * a23 -
      a00 * a31 * a13 * a22 -
      a10 * a01 * a22 * a33 +
      a10 * a01 * a23 * a32 +
      a10 * a21 * a02 * a33 -
      a10 * a21 * a03 * a32 -
      a10 * a31 * a02 * a23 +
      a10 * a31 * a03 * a22 +
      a20 * a01 * a12 * a33 -
      a20 * a01 * a13 * a32 -
      a20 * a11 * a02 * a33 +
      a20 * a11 * a03 * a32 +
      a20 * a31 * a02 * a13 -
      a20 * a31 * a03 * a12 -
      a30 * a01 * a12 * a23 +
      a30 * a01 * a13 * a22 +
      a30 * a11 * a02 * a23 -
      a30 * a11 * a03 * a22 -
      a30 * a21 * a02 * a13 +
      a30 * a21 * a03 * a12
    | _ => assert false
    }

  let ccw = ((xu, yu), (xv, yv), (xw, yw)) =>
    det([[xu, yu, 1.0], [xv, yv, 1.0], [xw, yw, 1.0]]) > 0.0

  /* i DEBUG
    let ccw (xu,yu) (xv,yv) (xw,yw) =
    eprintf "ccw((%.0f,%.0f),(%.0f,%.0f),(%.0f,%.0f)) -> "
      xu yu xv yv xw yw;
    let r = ccw (xu,yu) (xv,yv) (xw,yw) in
    eprintf "%b\n" r; flush stderr;
    r
    i*/

  let in_circle = ((xt, yt), (xu, yu), (xv, yv), (xw, yw)) =>
    det([
      [xt, yt, xt * xt + yt * yt, 1.0],
      [xu, yu, xu * xu + yu * yu, 1.0],
      [xv, yv, xv * xv + yv * yv, 1.0],
      [xw, yw, xw * xw + yw * yw, 1.0],
    ]) > 0.0

  /* i DEBUG
    let in_circle (xt,yt) (xu,yu) (xv,yv) (xw,yw) =
    eprintf "in_circle((%.0f,%.0f),(%.0f,%.0f),(%.0f,%.0f),(%.0f,%.0f)) -> "
      xt yt xu yu xv yv xw yw;
    let r = in_circle (xt,yt) (xu,yu) (xv,yv) (xw,yw) in
    eprintf "%b\n" r; flush stderr;
    r
    i*/
}

module Float = Make(FloatPoints)

@@ocaml.text(" Points with integer coordinates.
    We approximate using module [FloatPoints] but this could be made exact
    following Knuth's code in Axioms and Hulls ")

module IntPoints = {
  type point = (int, int)

  let ccw = ((xu, yu), (xv, yv), (xw, yw)) =>
    FloatPoints.ccw((float(xu), float(yu)), (float(xv), float(yv)), (float(xw), float(yw)))

  let in_circle = ((xt, yt), (xu, yu), (xv, yv), (xw, yw)) =>
    FloatPoints.in_circle(
      (float(xt), float(yt)),
      (float(xu), float(yu)),
      (float(xv), float(yv)),
      (float(xw), float(yw)),
    )
}

module Int = Make(IntPoints)
