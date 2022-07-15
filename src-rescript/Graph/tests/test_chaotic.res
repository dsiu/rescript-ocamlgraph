//open Graph

/*
   Analysis with ChaoticIteration of the following program :
   X:=0;
   while X<40 do
     X:=X+1
   done

   The analyses uses the interval abstract domain, with widening, and
   the widening delay given as a parameter in the commande line (0 if
   no parameter is given).

   Executing this program should print:

   W = WTO, delay=40:
   1 -> [-∞; +∞]
   2 -> [0; +∞]
   3 -> [0; 39]
   4 -> [40; +∞]

   W = {3}, delay=39:
   1 -> [-∞; +∞]
   2 -> [0; +∞]
   3 -> [0; +∞]
   4 -> [40; +∞]

   W = WTO, delay=41:
   1 -> [-∞; +∞]
   2 -> [0; 40]
   3 -> [0; 39]
   4 -> [40; 40]

*/

/* Trivial language, only one variable */

module Operator = {
  type rec expr =
    | Var
    | Int(int)
    | Sum(expr, expr)

  type test =
    | Le
    | Ge

  type t =
    | Affect(expr)
    | Test(test, int)

  let compare = compare

  let default = Affect(Var)
}

@@warning("-44")

open Operator

/* Basic interval domain */

module Interval = {
  type num =
    | MinusInfinity
    | Int(int)
    | PlusInfinity

  let print_num = x =>
    switch x {
    | MinusInfinity => print_string("-∞")
    | Int(n) => print_int(n)
    | PlusInfinity => print_string("+∞")
    }

  let \"<%" = (n1, n2) =>
    switch (n1, n2) {
    | (MinusInfinity, MinusInfinity)
    | (PlusInfinity, PlusInfinity) =>
      failwith("<%")
    | (MinusInfinity, _) => true
    | (_, PlusInfinity) => true
    | (Int(a), Int(b)) => a < b
    | (_, _) => false
    }

  let \">=%" = (n1, n2) => !\"<%"(n1, n2)

  let \"<=%" = (n1, n2) => \"<%"(n1, n2) || n1 == n2

  let \">%" = (n1, n2) => \">=%"(n1, n2) && n1 != n2

  let min_ = (n1, n2) =>
    if \"<=%"(n1, n2) {
      n1
    } else {
      n2
    }

  let max_ = (n1, n2) =>
    if \">=%"(n1, n2) {
      n1
    } else {
      n2
    }

  type t =
    | Bottom
    | Bounded(num, num)

  let top = Bounded(MinusInfinity, PlusInfinity)

  let equal = \"="

  let print = x =>
    switch x {
    | Bottom => print_string("⊥")
    | Bounded(a, b) =>
      /* Printf is for the weak */
      print_string("[")
      print_num(a)
      print_string("; ")
      print_num(b)
      print_string("]")
    }

  let join = (i1, i2) =>
    switch (i1, i2) {
    | (Bottom, _) => i2
    | (_, Bottom) => i1
    | (Bounded(a, b), Bounded(c, d)) => Bounded(min_(a, c), max_(b, d))
    }

  let singleton = n => Bounded(Int(n), Int(n))

  let \"+%" = (x, y) =>
    switch (x, y) {
    | (MinusInfinity, PlusInfinity)
    | (PlusInfinity, MinusInfinity) =>
      failwith("+%")
    | (MinusInfinity, _)
    | (_, MinusInfinity) =>
      MinusInfinity
    | (PlusInfinity, _)
    | (_, PlusInfinity) =>
      PlusInfinity
    | (Int(a), Int(b)) => Int(a + b)
    }

  let rec abstr_expr = (interval, x) =>
    switch x {
    | Var => interval
    | Int(n) => singleton(n)
    | Sum(e1, e2) =>
      switch (abstr_expr(interval, e1), abstr_expr(interval, e2)) {
      | (Bottom, _) => Bottom
      | (_, Bottom) => Bottom
      | (Bounded(a, b), Bounded(c, d)) => Bounded(\"+%"(a, c), \"+%"(b, d))
      }
    }

  let abstr_test = (interval, test, c) =>
    switch interval {
    | Bottom => Bottom
    | Bounded(a, b) =>
      switch test {
      | Le =>
        if \">%"(a, c) {
          Bottom
        } else {
          Bounded(a, min_(b, c))
        }
      | Ge =>
        if \"<%"(b, c) {
          Bottom
        } else {
          Bounded(max_(a, c), b)
        }
      }
    }

  let analyze = ((_, op, _), interval) =>
    switch op {
    | Affect(e) => abstr_expr(interval, e)
    | Test(test, n) => abstr_test(interval, test, Int(n))
    }

  let widening = (i1, i2) =>
    switch (i1, i2) {
    | (Bottom, _) => i2
    | (Bounded(_), Bottom) => failwith("widening")
    | (Bounded(a, b), Bounded(c, d)) =>
      Bounded(
        if \"<=%"(a, c) {
          a
        } else {
          MinusInfinity
        },
        if \">=%"(b, d) {
          b
        } else {
          PlusInfinity
        },
      )
    }
}

module Int = {
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = \"="
}

module Data = {
  include Interval
  type edge = (int, Operator.t, int)
}

module G = Persistent.Digraph.ConcreteLabeled(Int, Operator)
module Wto = WeakTopological.Make(G)
module Chaotic = ChaoticIteration.Make(G, Data)

let edges = list{
  (1, 2, Affect(Int(0))),
  (2, 3, Test(Le, 39)),
  (3, 2, Affect(Sum(Var, Int(1)))),
  (2, 4, Test(Ge, 40)),
}

let g = List.fold_left((acc, (v, w, op)) => G.add_edge_e(acc, G.E.create(v, op, w)), G.empty, edges)

let strategy = Wto.recursive_scc(g, 1)

let print_vertex_data = (vertex, interval) => {
  print_int(vertex)
  print_string(" -> ")
  Interval.print(interval)
  print_newline()
}

let init = v =>
  if v == 1 {
    Interval.top
  } else {
    Interval.Bottom
  }

let widening_delay1 = 40
let widening_set1 = ChaoticIteration.FromWto

let widening_delay2 = 39
let widening_set2 = ChaoticIteration.Predicate(\"="(3))

let widening_delay3 = 41
let widening_set3 = ChaoticIteration.FromWto

let result1 = Chaotic.recurse(g, strategy, init, widening_set1, widening_delay1)
let result2 = Chaotic.recurse(g, strategy, init, widening_set2, widening_delay2)
let result3 = Chaotic.recurse(g, strategy, init, widening_set3, widening_delay3)

let () = {
  print_endline("W = WTO, delay=40:")
  Chaotic.M.iter(print_vertex_data, result1)
  print_newline()

  print_endline("W = {3}, delay=39:")
  Chaotic.M.iter(print_vertex_data, result2)
  print_newline()

  print_endline("W = WTO, delay=41:")
  Chaotic.M.iter(print_vertex_data, result3)
  print_newline()
}
