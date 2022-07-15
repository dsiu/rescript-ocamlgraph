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

/* Graph traversal */

module type G = {
  let is_directed: bool
  type t
  module V: Sig.COMPARABLE
  let iter_vertex: (V.t => unit, t) => unit
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let iter_succ: (V.t => unit, t, V.t) => unit
  let fold_succ: ((V.t, 'a) => 'a, t, V.t, 'a) => 'a
}

/* depth-first search */
module Dfs = (G: G) => {
  module H = Hashtbl.Make(G.V)

  let fold = (f, i, g) => {
    let h = H.create(97)
    let s = Stack.create()
    let push = v =>
      if !H.mem(h, v) {
        H.add(h, v, ())
        Stack.push(v, s)
      }

    let rec loop = acc =>
      if !Stack.is_empty(s) {
        let v = Stack.pop(s)
        let ns = f(v, acc)
        G.iter_succ(push, g, v)
        loop(ns)
      } else {
        acc
      }

    G.fold_vertex((v, s) => {
      push(v)
      loop(s)
    }, g, i)
  }

  let iter = (~pre=_ => (), ~post=_ => (), g) => {
    let h = H.create(97)
    let rec visit = v =>
      if !H.mem(h, v) {
        H.add(h, v, ())
        pre(v)
        G.iter_succ(visit, g, v)
        post(v)
      }

    G.iter_vertex(visit, g)
  }

  let postfix = (post, g) => iter(~post, g)

  let fold_component = (f, i, g, v0) => {
    let h = H.create(97)
    let s = Stack.create()
    /* invariant: [h] contains exactly the vertices which have been pushed */
    let push = v =>
      if !H.mem(h, v) {
        H.add(h, v, ())
        Stack.push(v, s)
      }

    push(v0)
    let rec loop = acc =>
      if !Stack.is_empty(s) {
        let v = Stack.pop(s)
        let ns = f(v, acc)
        G.iter_succ(push, g, v)
        loop(ns)
      } else {
        acc
      }

    loop(i)
  }

  let iter_component = (~pre=_ => (), ~post=_ => (), g, v) => {
    let h = H.create(97)
    let rec visit = v => {
      H.add(h, v, ())
      pre(v)
      G.iter_succ(w =>
        if !H.mem(h, w) {
          visit(w)
        }
      , g, v)
      post(v)
    }

    visit(v)
  }

  let postfix_component = (post, g) => iter_component(~post, g)

  module Tail = {
    let has_cycle = g => {
      let h = H.create(97)
      let stack = Stack.create()
      let loop = () =>
        while !Stack.is_empty(stack) {
          let v = Stack.top(stack)
          if H.mem(h, v) {
            /* we are now done with node v */
            /* assert (H.find h v = true); */
            H.replace(h, v, false)
            ignore(Stack.pop(stack))
          } else {
            /* we start DFS from node v */
            H.add(h, v, true)
            G.iter_succ(w =>
              try if H.find(h, w) {
                raise(Exit)
              } catch {
              | Not_found => Stack.push(w, stack)
              }
            , g, v)
          }
        }

      try {
        G.iter_vertex(v =>
          if !H.mem(h, v) {
            Stack.push(v, stack)
            loop()
          }
        , g)
        false
      } catch {
      | Exit => true
      }
    }

    let has_cycle_undirected = g => {
      let h = H.create(97)
      let father = H.create(97)
      let is_father = (u, v) =>
        /* u is the father of v in the DFS descent */
        try G.V.equal(H.find(father, v), u) catch {
        | Not_found => false
        }

      let stack = Stack.create()
      let loop = () =>
        while !Stack.is_empty(stack) {
          let v = Stack.top(stack)
          if H.mem(h, v) {
            /* we are now done with node v */
            /* assert (H.find h v = true); */
            H.remove(father, v)
            H.replace(h, v, false)
            ignore(Stack.pop(stack))
          } else {
            /* we start DFS from node v */
            H.add(h, v, true)
            G.iter_succ(w =>
              try if H.find(h, w) && !is_father(w, v) {
                raise(Exit)
              } catch {
              | Not_found =>
                H.add(father, w, v)
                Stack.push(w, stack)
              }
            , g, v)
          }
        }

      try {
        G.iter_vertex(v =>
          if !H.mem(h, v) {
            Stack.push(v, stack)
            loop()
          }
        , g)
        false
      } catch {
      | Exit => true
      }
    }

    let has_cycle = g =>
      if G.is_directed {
        has_cycle(g)
      } else {
        has_cycle_undirected(g)
      }

    let iter = (f, g) => {
      let h = H.create(97)
      let stack = Stack.create()
      let loop = () =>
        while !Stack.is_empty(stack) {
          let v = Stack.pop(stack)
          if !H.mem(h, v) {
            H.add(h, v, ())
            f(v)
            G.iter_succ(w =>
              if !H.mem(h, w) {
                Stack.push(w, stack)
              }
            , g, v)
          }
        }

      G.iter_vertex(v =>
        if !H.mem(h, v) {
          Stack.push(v, stack)
          loop()
        }
      , g)
    }

    let iter_component = (f, g, v0) => {
      let h = H.create(97)
      let stack = Stack.create()
      Stack.push(v0, stack)
      while !Stack.is_empty(stack) {
        let v = Stack.pop(stack)
        if !H.mem(h, v) {
          H.add(h, v, ())
          f(v)
          G.iter_succ(w =>
            if !H.mem(h, w) {
              Stack.push(w, stack)
            }
          , g, v)
        }
      }
    }
  }

  let prefix = Tail.iter
  let has_cycle = Tail.has_cycle
  let prefix_component = Tail.iter_component

  /* step-by-step iterator */
  module S = Set.Make(G.V)

  @ocaml.doc(" (h, st, g) where h is the set of marked vertices and st the stack
      invariant: the first element of st is not in h i.e. to be visited ")
  type iterator = (S.t, list<G.V.t>, G.t)

  let start = g => {
    let st = G.fold_vertex((v, st) => list{v, ...st}, g, list{})
    (S.empty, st, g)
  }

  let get = ((_, st, _)) =>
    switch st {
    | list{} => raise(Exit)
    | list{v, ..._} => v
    }

  let step = ((s, st, g)) =>
    switch st {
    | list{} => raise(Exit)
    | list{v, ...st} =>
      let s' = S.add(v, s)
      let st' = G.fold_succ((w, st) => list{w, ...st}, g, v, st)
      let rec clean = x =>
        switch x {
        | list{w, ...st} if S.mem(w, s') => clean(st)
        | st => st
        }

      (s', clean(st'), g)
    }
}

/* breadth-first search */
module Bfs = (G: G) => {
  module H = Hashtbl.Make(G.V)

  let fold = (f, i, g: G.t) => {
    let h = H.create(97)
    let q = Queue.create()
    /* invariant: [h] contains exactly the vertices which have been pushed */
    let push = v =>
      if !H.mem(h, v) {
        H.add(h, v, ())
        Queue.add(v, q)
      }

    let rec loop = s =>
      if !Queue.is_empty(q) {
        let v = Queue.pop(q)
        let ns = f(v, s) /* Sticking to OCamlGraph's fold conv */
        G.iter_succ(push, g, v)
        loop(ns)
      } else {
        s
      }

    G.fold_vertex((v, s) => {
      push(v)
      loop(s)
    }, g, i)
  }

  let iter = f => fold((v, ()) => f(v), ())

  let fold_component = (f, i, g, v0) => {
    let h = H.create(97)
    let q = Queue.create()
    /* invariant: [h] contains exactly the vertices which have been pushed */
    let push = v =>
      if !H.mem(h, v) {
        H.add(h, v, ())
        Queue.add(v, q)
      }

    push(v0)
    let rec loop = s =>
      if !Queue.is_empty(q) {
        let v = Queue.pop(q)
        let ns = f(v, s)
        G.iter_succ(push, g, v)
        loop(ns)
      } else {
        s
      }

    loop(i)
  }

  let iter_component = f => fold_component((v, ()) => f(v), ())

  /* step-by-step iterator */

  /* simple, yet O(1)-amortized, persistent queues */
  module Q = {
    type t<'a> = (list<'a>, list<'a>)
    exception Empty
    let empty = (list{}, list{})
    let is_empty = x =>
      switch x {
      | (list{}, list{}) => true
      | _ => false
      }
    let push = (x, (i, o)) => (list{x, ...i}, o)
    let pop = x =>
      switch x {
      | (i, list{y, ...o}) => (y, (i, o))
      | (list{}, list{}) => raise(Empty)
      | (i, list{}) =>
        switch List.rev(i) {
        | list{x, ...o} => (x, (list{}, o))
        | list{} => assert false
        }
      }
    let peek = q => fst(pop(q))
  }

  module S = Set.Make(G.V)

  /* state is [(s,q,g)] : [s] contains elements never been pushed in [q] */
  type iterator = (S.t, Q.t<G.V.t>, G.t)

  let start = g => {
    let s = G.fold_vertex(S.add, g, S.empty)
    (s, Q.empty, g)
  }

  let get = ((s, q, _)) =>
    if Q.is_empty(q) {
      if S.is_empty(s) {
        raise(Exit)
      } else {
        S.choose(s)
      }
    } else {
      Q.peek(q)
    }

  let step = ((s, q, g)) => {
    let push = (v, (s, q) as acc) =>
      if S.mem(v, s) {
        (S.remove(v, s), Q.push(v, q))
      } else {
        acc
      }

    let (v, s', q') = if Q.is_empty(q) {
      if S.is_empty(s) {
        raise(Exit)
      }
      let v = S.choose(s)
      (v, S.remove(v, s), q)
    } else {
      let (v, q') = Q.pop(q)
      (v, s, q')
    }

    let (s'', q'') = G.fold_succ(push, g, v, (s', q'))
    (s'', q'', g)
  }
}

/* Graph traversal with marking. */

module type GM = {
  type t
  module V: {
    type t
  }
  let iter_vertex: (V.t => unit, t) => unit
  let iter_succ: (V.t => unit, t, V.t) => unit
  module Mark: {
    let clear: t => unit
    let get: V.t => int
    let set: (V.t, int) => unit
  }
}

module Mark = (G: GM) => {
  let dfs = g => {
    G.Mark.clear(g)
    let n = ref(0)
    let rec visit = v =>
      if G.Mark.get(v) == 0 {
        incr(n)
        G.Mark.set(v, n.contents)
        G.iter_succ(visit, g, v)
      }

    G.iter_vertex(visit, g)
  }

  /* invariant: [h v = 0] means not visited at all; [h v = 1] means
     already visited in the current component; [h v = 2] means
     already visited in another tree */
  let has_cycle = g => {
    G.Mark.clear(g)
    let rec visit = v => {
      G.Mark.set(v, 1)
      G.iter_succ(w => {
        let m = G.Mark.get(w)
        if m == 1 {
          raise(Exit)
        }
        if m == 0 {
          visit(w)
        }
      }, g, v)
      G.Mark.set(v, 2)
    }

    try {
      G.iter_vertex(v =>
        if G.Mark.get(v) == 0 {
          visit(v)
        }
      , g)
      false
    } catch {
    | Exit => true
    }
  }
}
