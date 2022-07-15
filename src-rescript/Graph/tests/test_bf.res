/* Test file for Bellman-Ford */

@@warning("-3")
open Printf
open Pack.Digraph

/* TODO: This could be done a tiny bit better,
 no need for printing, this could use alcotest or equivalent */

let test = (~name, has_cycle, spec) => {
  printf("Running test with name: %s\n", name)
  let v = Array.init(5, V.create)
  let g = create()
  let () = Array.iter(add_vertex(g), v)

  let build = ((s, w, t)) => add_edge_e(g, E.create(v[s], w, v[t]))
  List.iter(build, spec)
  try {
    let cycle = bellman_ford(g, v[1])
    let print_edge = e =>
      printf("%d --(%d)--> %d\n", V.label(E.src(e)), E.label(e), V.label(E.dst(e)))

    List.iter(print_edge, cycle)
    assert has_cycle
  } catch {
  | Not_found =>
    printf("No cycle found\n")
    assert !has_cycle
  }
  print_newline()
  flush(stdout)
}

let () = {
  test(
    ~name="cycle_1",
    true,
    list{(0, -3, 1), (1, 1, 2), (2, 1, 0), (1, 1, 3), (3, 1, 4), (4, 1, 0)},
  )
  test(
    ~name="cycle_2",
    true,
    list{(0, -10, 1), (1, 1, 2), (2, 1, 0), (1, 1, 3), (3, 1, 4), (4, 1, 0)},
  )
  test(~name="cycle_3", true, list{(0, -10, 1), (2, 1, 0), (1, 1, 3), (3, 1, 4), (4, 1, 0)})
  test(~name="cycle_4", true, list{(0, -10, 1), (1, 1, 2), (1, 1, 3), (3, 1, 4), (4, 1, 0)})
  test(~name="cycle_5", true, list{(0, -10, 1), (1, 1, 2), (2, 1, 0), (3, 1, 4), (4, 1, 0)})
  test(~name="cycle_6", true, list{(0, -10, 1), (1, 1, 2), (2, 1, 0), (1, 1, 3), (4, 1, 0)})
  test(~name="nocycle_1", false, list{(1, 1, 2), (2, 1, 0), (1, 1, 3), (3, 1, 4), (4, 1, 0)})
  test(~name="nocycle_2", false, list{(0, -10, 1), (1, 1, 2), (1, 1, 3), (3, 1, 4)})

  printf("All tests succeeded.\n")
}
