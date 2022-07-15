/* Test file for Path.Check */

@@warning("-3")
open Format
//open Graph
open Pack.Digraph

let test = (n, edges) => {
  let v = Array.init(n, V.create)
  let g = create()
  let () = Array.iter(add_vertex(g), v)
  let build = ((s, t)) => add_edge(g, v[s], v[t])
  List.iter(build, edges)
  let path = PathCheck.check_path(PathCheck.create(g))
  for i in 0 to n - 1 {
    let seen = Array.make(n, false)
    let pre = v => seen[V.label(v)] = true
    Dfs.prefix_component(pre, g, v[i])
    for j in 0 to n - 1 {
      assert (seen[j] == path(v[i], v[j]))
    }
  }
}

let () = {
  test(3, list{(0, 1), (1, 2)})
  test(3, list{})
  /* 1-cycle */
  test(1, list{(0, 0)})
  /* 2-cycle */
  test(2, list{(0, 1), (1, 0)})
  test(3, list{(0, 1), (1, 0)})
  /* 2-cycle with out edge */
  test(3, list{(0, 1), (1, 0), (1, 2)})
  test(3, list{(2, 0), (0, 2), (0, 1)})
  test(3, list{(1, 2), (2, 1), (2, 0)})
  /* 2 loops */
  test(5, list{(1, 2), (2, 1), (2, 0), (3, 4), (4, 3)})
  test(5, list{(1, 2), (2, 1), (2, 0), (2, 3), (3, 4), (4, 3)})
  /* 2-cycle with in edge */
  test(3, list{(1, 2), (2, 1), (0, 2)})
  test(3, list{(1, 2), (2, 1), (0, 1)})
  /* 2 cycles connected */
  test(4, list{(0, 1), (1, 0), (2, 3), (3, 2), (2, 1)})
  test(4, list{(0, 1), (1, 0), (2, 3), (3, 2), (1, 2)})
  test(4, list{(0, 1), (1, 0), (2, 3), (3, 2), (1, 2), (2, 1)})
  /* 3-cycle with in and out edges */
  test(5, list{(0, 1), (1, 2), (2, 0), (3, 0), (2, 4)})
  /* 3 cycles in a row */
  test(7, list{(0, 1), (1, 0), (1, 2), (2, 3), (3, 2), (3, 4), (4, 5), (5, 6), (6, 4)})
  /* 3 cycles with 2 cycles in a cycle */
  test(7, list{(0, 1), (1, 0), (1, 2), (2, 3), (3, 2), (3, 4), (4, 5), (5, 6), (6, 4), (5, 2)})
  printf("test check_path: all tests succeeded.@.")
}
