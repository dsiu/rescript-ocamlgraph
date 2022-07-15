/* Test file for topological sort */

@@warning("-3-44")

open Format
//open Graph
open Pack.Digraph

let test = (~check=true, iter, n, edges) => {
  let v = Array.init(n, V.create)
  let g = create()
  let () = Array.iter(add_vertex(g), v)
  let build = ((s, t)) => add_edge(g, v[s], v[t])
  List.iter(build, edges)
  /* run top sort */
  let num = Array.make(n, 0)
  let i = ref(0)
  iter(v => {
    incr(i)
    num[V.label(v)] = i.contents
  }, g)
  let r = Array.init(n, i => i)
  Array.sort((i, j) => num[i] - num[j], r)
  /* if check then for v = 0 to n-1 do printf "%d " r.(v) done; printf "@."; */
  /* check */
  let path = PathCheck.check_path(PathCheck.create(g))
  let check_edge = ((x, y)) => {
    let vx = v[x] and vy = v[y]
    /* printf "x=%d y=%d num(x)=%d num(y)=%d@." x y num.(x) num.(y);
     * printf "x-->y=%b  y-->x=%b@." (path vx vy) (path vy vx); */
    assert (num[x] > 0 && num[y] > 0)
    assert (num[x] >= num[y] || (path(vx, vy) || !path(vy, vx)))
  }
  if check {
    for x in 0 to n - 1 {
      for y in 0 to n - 1 {
        check_edge((x, y))
      }
    }
  }
  /* display_with_gv g; */
  ()
}

let tests = iter => {
  let test = test(iter)
  test(3, list{(0, 1), (1, 2)})
  test(3, list{})
  /* 1-cycle */
  test(1, list{(0, 0)})
  /* 2-cycle */
  test(2, list{(0, 1), (1, 0)})
  /* 2-cycle with out edge */
  test(3, list{(0, 1), (1, 0), (1, 2)})
  test(3, list{(2, 0), (0, 2), (0, 1)})
  test(3, list{(1, 2), (2, 1), (2, 0)})
  /* 2 loops */
  test(5, list{(1, 2), (2, 1), (2, 0), (3, 4), (4, 3)})
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
  printf("test topsort: all tests succeeded.@.")
}

let () = tests(Topological.iter)
/* let () = tests Topological.iter_stable */

let rec pow = (a, x) =>
  switch x {
  | 0 => 1
  | 1 => a
  | n =>
    let b = pow(a, n / 2)
    b *
    b * if mod(n, 2) == 0 {
      1
    } else {
      a
    }
  }

let () = for n_iter in 0 to 5 {
  let n = pow(10, n_iter)
  let el = ref(list{})
  /* linear graph */
  /* for i = 0 to n-2 do el := (i,i+1) :: !el done; */
  /* for i = 0 to n-2 do el := (i+1,i) :: !el done; */
  el := list{(n - 1, 0)}
  for i in 0 to n - 2 {
    el := list{(i, i + 1), ...el.contents}
  }
  test(~check=false, Topological.iter, n, el.contents)
}
