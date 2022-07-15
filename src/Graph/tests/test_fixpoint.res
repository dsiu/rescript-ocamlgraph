/* Test file for Fixpoint */
@@warning("-3")

let id = x => x
module IntOrdered = {
  type t = int
  let compare: (int, int) => int = compare
  let hash: int => int = id
  let equal: (int, int) => bool = \"="
}
module IntSet = Set.Make(IntOrdered)
module G = Persistent.Digraph.Concrete(IntOrdered)
module Divisors = Classic.P(G)
module Analysis = {
  type data = IntSet.t
  type edge = G.edge
  type vertex = G.vertex
  type g = G.t
  let direction = Fixpoint.Backward
  let join = IntSet.union
  let equal = IntSet.equal
  let analyze = (_: edge): (data => data) => id
}
module Fixpoint = Fixpoint.Make(G, Analysis)

let pp_int_set = (pp, set) => {
  let first = ref(true)
  Format.fprintf(pp, "@[<hov>")
  IntSet.iter(x => {
    if first.contents {
      first := false
    } else {
      Format.fprintf(pp, ",@ ")
    }
    Format.pp_print_int(pp, x)
  }, set)
  Format.fprintf(pp, "@]")
}

let () = {
  let n = 15
  let labels = Fixpoint.analyze(IntSet.singleton, Divisors.divisors(n))
  Format.open_vbox(0)
  for i in 2 to n {
    Format.printf("Labels for %d: %a@,", i, pp_int_set, labels(i))
  }
  Format.close_box()
  Format.print_flush()
}
