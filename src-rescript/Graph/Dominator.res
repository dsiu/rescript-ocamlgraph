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

/*
  Copyright © 2009 Carnegie-Mellon University, David Brumley, and Ivan Jager.
  From the BAP library; see http://bap.ece.cmu.edu
  Modified by OCamlGraph's authors.
*/

/* stuff to read:
   http://www.hipersoft.rice.edu/grads/publications/dom14.pdf
   Modern Compiler Implementation in ML, by Appel
   Introduction to Algorithms, Cormen et al
*/

exception Unreachable

module type G = {
  type t
  module V: Sig.COMPARABLE
  let pred: (t, V.t) => list<V.t>
  let succ: (t, V.t) => list<V.t>
  let fold_vertex: ((V.t, 'a) => 'a, t, 'a) => 'a
  let iter_vertex: (V.t => unit, t) => unit
  let iter_succ: (V.t => unit, t, V.t) => unit
  let nb_vertex: t => int
}

module type I = {
  include G
  let empty: unit => t
  let add_edge: (t, V.t, V.t) => t
}

module type S = {
  type t
  type vertex
  module S: Set.S with type elt = vertex
  type idom = vertex => vertex
  type idoms = (vertex, vertex) => bool
  type dom_tree = vertex => list<vertex>
  type dominators = vertex => list<vertex>
  type dom = (vertex, vertex) => bool
  type sdom = (vertex, vertex) => bool
  type dom_frontier = vertex => list<vertex>
  let compute_idom: (t, vertex, vertex) => vertex
  let dominators_to_dom: ('a => S.t, vertex, 'a) => bool
  let dominators_to_sdom: (vertex => S.t, vertex, vertex) => bool
  let dom_to_sdom: ((vertex, vertex) => bool, vertex, vertex) => bool
  let dominators_to_sdominators: (vertex => S.t, vertex) => S.t
  let dominators_to_idoms: (vertex => S.t, vertex, vertex) => bool
  let dominators_to_dom_tree: (
    t,
    ~pred: (t, vertex) => list<vertex>=?,
    vertex => S.t,
    vertex,
  ) => S.t
  let idom_to_dom_tree: (t, vertex => vertex, vertex) => list<vertex>
  let idom_to_idoms: (idom, vertex, vertex) => bool
  let compute_dom_frontier: (t, dom_tree, idom, vertex) => list<vertex>
  let idom_to_dominators: ('a => 'a, 'a) => list<'a>
  let idom_to_dom: (vertex => vertex, vertex, vertex) => bool
  let dom_tree_to_nontrivial_dom: (vertex, dom_tree) => list<vertex>
  let dom_tree_to_snontrivial_dom: (vertex, dom_tree) => S.t
}

module Make = (G: G) => {
  type t = G.t
  type vertex = G.V.t

  module H = Hashtbl.Make(G.V)
  module S = Set.Make(G.V)

  @ocaml.doc(" function from [n] to [n]'s immediate dominator ")
  type idom = vertex => vertex

  @ocaml.doc(" [idoms x y] is true when [x] is [y]'s immediate dominator ")
  type idoms = (vertex, vertex) => bool

  @ocaml.doc(" function from [x] to a list of nodes immediately dominated by [x] ")
  type dom_tree = vertex => list<vertex>

  @ocaml.doc(" function from node to a list of nodes that dominate it. ")
  type dominators = vertex => list<vertex>

  @ocaml.doc(" [dom x y] returns true iff [x] dominates [y] ")
  type dom = (vertex, vertex) => bool

  @ocaml.doc(" [sdom x y] returns true iff [x] strictly dominates [y]. ")
  type sdom = (vertex, vertex) => bool

  @ocaml.doc(" function from [x] to a list of nodes not dominated by [x], but with
      predecessors which are dominated by [x] ")
  type dom_frontier = vertex => list<vertex>

  let set_of_list = x => List.fold_left((set, v) => S.add(v, set), S.empty, x)

  @ocaml.doc(" Computes the dominator tree, using the Lengauer-Tarjan algorithm.
      [compute_idom cfg s0] returns a function [idom : V.t -> V.t] s.t.
      [idom x] returns the immediate dominator of [x]
  ")
  let compute_idom = (cfg, s0) => {
    /* based on the Tiger book, section 19.2.
       This uses path compression, but doesn't yet do balanced path
       compression, so the runtime is O(N log(N)) rather than
       O(N inverseackerman(N))
 */
    let size = G.nb_vertex(cfg)
    let bucket = H.create(size) /* node n -> */
    and dfnum_h = H.create(size) /* node -> DFS number */
    and parent = H.create(size) /* node -> parent in DFS tree */
    and semi_h = H.create(size) /* node -> semidominator */
    and ancestor = H.create(size) /* node -> */
    and best = H.create(size) /* node -> */
    and samedom = H.create(size) /* node -> node with same idom */
    and idom = H.create(size) /* node n -> idom n */
    and vertex = Array.make(size, s0) /* DFS number -> node */
    and nn = ref(0)
    let dfnum = x =>
      try H.find(dfnum_h, x) catch {
      | Not_found => raise(Unreachable)
      }
    and semi = H.find(semi_h)
    let dfs = n0 => {
      let stack = Stack.create()
      let loop = () =>
        while !Stack.is_empty(stack) {
          let (n, p) = Stack.pop(stack)
          if !H.mem(dfnum_h, n) {
            let enn = nn.contents
            H.add(dfnum_h, n, enn)
            vertex[enn] = n
            switch p {
            | Some(p) => H.add(parent, n, p)
            | None => ()
            }
            nn := enn + 1
            G.iter_succ(m =>
              if !H.mem(dfnum_h, m) {
                Stack.push((m, Some(n)), stack)
              }
            , cfg, n)
          }
        }

      Stack.push((n0, None), stack)
      loop()
    }

    let rec ancestor_with_lowest_semi = v =>
      try {
        let a = H.find(ancestor, v)
        let b = ancestor_with_lowest_semi(a)
        let () = H.replace(ancestor, v, H.find(ancestor, a))
        let best_v = H.find(best, v)
        if dfnum(semi(b)) < dfnum(semi(best_v)) {
          H.replace(best, v, b)
          b
        } else {
          best_v
        }
      } catch {
      | Not_found => H.find(best, v)
      }

    let link = (p, n) => {
      H.replace(ancestor, n, p)
      H.replace(best, n, n)
    }

    let semidominator = n => {
      let s = H.find(parent, n)
      List.fold_left((s, v) =>
        try {
          /* FIXME: do we want to allow unreachable nodes? */
          let s' = if dfnum(v) <= dfnum(n) {
            v
          } else {
            semi(ancestor_with_lowest_semi(v))
          }

          if dfnum(s') < dfnum(s) {
            s'
          } else {
            s
          }
        } catch {
        | Unreachable => /* maybe switch to Not_found later */
          s
        } /* v is unreachable from s0 */
      , s, G.pred(cfg, n))
    }

    let () = dfs(s0)
    let lastn = nn.contents - 1
    while {
      decr(nn)
      nn.contents > 0
    } {
      /* skip over the root node */
      let i = nn.contents
      let n = vertex[i]
      let p = H.find(parent, n)
      let s = semidominator(n)
      H.add(semi_h, n, s)
      H.add(bucket, s, n)
      link(p, n)
      /* now that the path from p to v is in the forest,
         calculate the dominator of v based on the first clause of the
         Dominator Theorem, otherwise defer until y's dominator is known */
      List.iter(v => {
        let y = ancestor_with_lowest_semi(v)
        if G.V.equal(semi(y), semi(v)) {
          H.add(idom, v, p)
        } else {
          H.add(samedom, v, y)
        }
        H.remove(bucket, p) /* could use H.remove_all if we used extlib */
      }, H.find_all(bucket, p))
    }
    /* now all the defered calculations can be done */
    for i in 1 to lastn {
      let n = vertex[i]
      try H.add(idom, n, H.find(idom, H.find(samedom, n))) catch {
      | Not_found => ()
      }
    }
    H.find(idom)
  }

  @ocaml.doc(" Given a function from a node to it's dominators, returns a function
      [dom : V.t -> V.t -> bool] s.t. [dom x y] returns true when
      [x] dominates [y]
  ")
  let dominators_to_dom = (dominators, x, y) => S.mem(x, dominators(y))

  @ocaml.doc(" Given a function from a node to it's dominators, returns a function
      [sdom : V.t -> V.t -> bool] s.t. [sdom x y] returns true when
      [x] strictly dominates [y] ")
  let dominators_to_sdom = (dominators, x, y) =>
    !G.V.equal(x, y) && dominators_to_dom(dominators, x, y)

  let dom_to_sdom = (dom, x, y) => !G.V.equal(x, y) && dom(x, y)

  @ocaml.doc(" Given a a function from a node to it's dominators, returns a function
      from a node to it's strict dominators. ")
  let dominators_to_sdominators = (dominators, x) => S.remove(x, dominators(x))

  @ocaml.doc(" Given a function from a node to it's dominators, returns a function
      [idoms : G.V.t -> G.V.t -> bool] s.t. [idoms x y] returns true when
      [x] is the immediate dominator of [y].
  ")
  let dominators_to_idoms = dominators => {
    let sdom = dominators_to_sdom(dominators)
    (x, y) =>
      sdom(x, y) && {
        let sdoms = dominators_to_sdominators(dominators, y)
        S.for_all(w => G.V.equal(x, w) || !sdom(x, w), sdoms)
      }
  }

  @ocaml.doc(" Computes a dominator tree (function from x to a list of nodes immediately
      dominated by x) for the given CFG and dominator function.
      Note: The dominator tree is also called [IDom] by Muchnick.
      Note: If you are computing a post-dominator tree, then the
      optional argument pred should be G.succ.
  ")
  let dominators_to_dom_tree = (cfg, ~pred=G.pred, dominators) => {
    let idoms = dominators_to_idoms(dominators)
    let tree = H.create(97)
    let () = G.iter_vertex(y =>
      switch pred(cfg, y) {
      | list{x} =>
        if (
          /* a node that is not reachable from start has no
           idom */
          S.is_empty(dominators(x))
        ) {
          ()
        } else {
          H.add(tree, x, y)
        }
      | _ => S.iter(x =>
          if idoms(x, y) {
            H.add(tree, x, y)
          }
        , dominators(y))
      }
    , cfg)

    /* FIXME: maybe faster to convert eagerly */
    x => set_of_list(H.find_all(tree, x))
  }

  @ocaml.doc(" Computes a dominator tree (function from x to a list of nodes immediately
      dominated by x) for the given CFG and idom function. ")
  let idom_to_dom_tree = (cfg, idom) => {
    let tree = H.create(G.nb_vertex(cfg))
    let () = G.iter_vertex(v =>
      try H.add(tree, idom(v), v) catch {
      | Not_found => ()
      } /* s0 doesn't have an idom */
    , cfg)

    H.find_all(tree)
  }

  let idom_to_idoms = (idom: idom, x, y) =>
    try G.V.equal(x, idom(y)) catch {
    | Not_found => false
    } /* s0 doesn't have an idom */

  @ocaml.doc(" Computes the dominance frontier.
      As specified in section 19.1 of Modern Compiler Implementation in ML
      by Andrew Appel.
  ")
  let compute_dom_frontier = (cfg, dom_tree: dom_tree, idom: idom) => {
    let children = dom_tree
    let idoms = idom_to_idoms(idom)
    let df_cache = H.create(57)
    let df_local = n =>
      /* the successors of n that are not strictly dominated by n */
      List.filter(y => !idoms(n, y), G.succ(cfg, n))

    /* written in CPS to prevent stack overflow */
    let rec df = (n, k) =>
      switch try Some(H.find(df_cache, n)) catch {
      | Not_found => None
      } {
      | Some(r) => k(r)
      | None =>
        let s = df_local(n)
        add_df_ups(
          s,
          n,
          res => {
            H.add(df_cache, n, res)
            k(res)
          },
          children(n),
        )
      }
    and add_df_ups = (s, n, k, x) =>
      switch x {
      | list{} => k(s)
      | list{c, ...chl} =>
        df(c, dfc =>
          add_df_ups(List.fold_left(/* the appel errata uses sdom, but Muchnick uses idoms, which
             should be a bit faster and is the same */
            (s, w) =>
              if idoms(n, w) {
                s
              } else {
                list{w, ...s}
              }
            , s, dfc), n, k, chl)
        )
      }

    n => df(n, x => x)
  }

  let idom_to_dominators = (idom, x) => {
    let rec d = (y, list) =>
      try {
        let i = idom(y)
        d(i, list{i, ...list})
      } catch {
      | Not_found => list
      }

    d(x, list{})
  }

  let rec idom_to_dom = (idom, x, y) =>
    try {
      let d = idom(y)
      G.V.equal(x, d) || idom_to_dom(idom, x, d)
    } catch {
    | Not_found => false
    }

  /* There is a nice description of non-trivial dominators with an example
     in Section 2 and Figure 2 of Jaberi 2016, "On computing the
     2-vertex-connected components of directed graphs". */

  let dom_tree_to_nontrivial_dom = (v, dt) => {
    let rec f = (rs, x) =>
      switch x {
      | list{} => rs
      | list{x, ...xs} =>
        switch dt(x) {
        | list{} => f(rs, xs)
        | ys => f(list{x, ...rs}, List.rev_append(ys, xs))
        }
      }

    f(list{}, dt(v))
  }

  let dom_tree_to_snontrivial_dom = (v, dt) => {
    let rec f = (rs, x) =>
      switch x {
      | list{} => rs
      | list{x, ...xs} =>
        switch dt(x) {
        | list{} => f(rs, xs)
        | ys => f(S.add(x, rs), List.rev_append(ys, xs))
        }
      }

    f(S.empty, dt(v))
  }
}

module Make_graph = (G: I) => {
  include Make(G)

  type dom_graph = unit => t

  type dom_functions = {
    idom: idom,
    idoms: idoms,
    dom_tree: dom_tree,
    dominators: dominators,
    dom: dom,
    sdom: sdom,
    dom_frontier: dom_frontier,
    dom_graph: dom_graph,
  }

  let compute_dom_graph = (cfg, dom_tree) => G.fold_vertex((p, g) =>
      try List.fold_left((g, u) => G.add_edge(g, p, u), g, dom_tree(p)) catch {
      | Not_found => g
      }
    , cfg, G.empty())

  @ocaml.doc(" Computes all dominance functions.
      This function computes some things eagerly and some lazily, so don't
      worry about it doing extra work to compute functions you don't need,
      but also don't call it if you aren't going to use anything it returns.
      @return a record containing all dominance functions for the given graph
      and entry node.
  ")
  let compute_all = (cfg, s0) => {
    let idom = compute_idom(cfg, s0)
    let idoms = idom_to_idoms(idom)
    let dom_tree = lazy idom_to_dom_tree(cfg, idom)
    let dominators = idom_to_dominators(idom)
    let dom = idom_to_dom(idom)
    let sdom = dom_to_sdom(dom)
    let dom_frontier = lazy compute_dom_frontier(cfg, Lazy.force(dom_tree), idom)

    {
      idom: idom,
      idoms: idoms,
      dom_tree: x => Lazy.force(dom_tree, x),
      dominators: dominators,
      dom: dom,
      sdom: sdom,
      dom_frontier: x => Lazy.force(dom_frontier, x),
      dom_graph: () => compute_dom_graph(cfg, Lazy.force(dom_tree)),
    }
  }
}
