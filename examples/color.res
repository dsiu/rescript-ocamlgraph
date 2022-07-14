/* ************************************************************************ */
/*  */
/* Ocamlgraph: a generic graph library for OCaml */
/* Copyright (C) 2004-2007 */
/* Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles */
/*  */
/* This software is free software; you can redistribute it and/or */
/* modify it under the terms of the GNU Library General Public */
/* License version 2, with the special exception on linking */
/* described in file LICENSE. */
/*  */
/* This software is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. */
/*  */
/* ************************************************************************ */

/* 4-coloring planar graphs */

@@warning("-3")
open Printf

/* command line */
let n_ = ref(30)
let prob_ = ref(0.5)
let seed_ = ref(None)

let arg_spec = list{
  ("-v", Arg.Int(i => n_ := i), " <int>  number of vertices"),
  ("-prob", Arg.Float(f => prob_ := f), " <float>  probability to discrad an edge"),
  ("-seed", Arg.Int(n => seed_ := Some(n)), " <int>  random seed"),
}
let () = Arg.parse(arg_spec, _ => (), "usage: color <options>")

let n = n_.contents
let prob = prob_.contents

let seed = switch seed_.contents {
| None =>
  Random.self_init()
  Random.int(lsl(1, 29))
| Some(s) => s
}
let () = {
  Format.printf("seed = %d@.", seed)
  Random.init(seed)
}

/* undirected graphs with integer coordinates and integer labels on edges */

module IntInt = {
  type t = (int, int)
}
module Int = {
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = \"="
  let default = 0
}
module G = Imperative.Graph.AbstractLabeled(IntInt, Int)
open G

/* a random graph with n vertices */
module R = Rand.Planar.I(G)
let g0 = R.graph(~xrange=(20, 780), ~yrange=(20, 580), ~prob, n)

/* drawing */
let round = f => truncate(f +. 0.5)
let pi = 4.0 *. atan(1.0)

//open Graphics
//let () = open_graph(" 800x600")

let vertex_radius = 5

//let draw_edge = (v1, v2) => {
//  let (xu, yu) = G.V.label(v1)
//  let (xv, yv) = G.V.label(v2)
//  set_color(black)
//  let dx = float(xv - xu)
//  let dy = float(yv - yu)
//  let r = sqrt(dx *. dx +. dy *. dy)
//  let d = float(vertex_radius) +. 3.
//  let (xs, ys) = (float(xu) +. d *. dx /. r, float(yu) +. d *. dy /. r)
//  let (xd, yd) = (float(xv) -. d *. dx /. r, float(yv) -. d *. dy /. r)
//  moveto(round(xs), round(ys))
//  lineto(round(xd), round(yd))
//}
//
//let draw_vertex = v => {
//  let (x, y) = G.V.label(v)
//  set_color(red)
//  draw_circle(x, y, vertex_radius)
//}
//
let color_vertex = v => {
  let (x, y) = G.V.label(v)
  //  set_color(color)
  //  fill_circle(x, y, vertex_radius)
  Js.log(`color_vertex: ${x->string_of_int}, ${y->string_of_int}`)
}
//
//let draw_graph = () => {
//  clear_graph()
//  set_color(red)
//  set_line_width(1)
//  G.iter_vertex(draw_vertex, g0)
//  G.iter_edges(draw_edge, g0)
//}

module Dfs = Traverse.Dfs(G)
module Bfs = Traverse.Bfs(G)

let test_bfs = () => {
  let rec loop = i => {
    let v = Bfs.get(i)
    color_vertex(v)
    //    ignore(Graphics.wait_next_event(list{Key_pressed}))
    loop(Bfs.step(i))
  }

  try loop(Bfs.start(g0)) catch {
  | Exit => ()
  }
}

let test_dfs = () => {
  let rec loop = i => {
    let v = Dfs.get(i)
    color_vertex(v)
    //    ignore(Graphics.wait_next_event(list{Key_pressed}))
    loop(Dfs.step(i))
  }

  try loop(Dfs.start(g0)) catch {
  | Exit => ()
  }
}

//let cols = [white, red, green, blue, yellow, black]
//exception NoColor

/* Algo I. Brute force. */

//module C = Coloring.Mark(G)
//
//let coloring_a = _ => {
//  Mark.clear(g0)
//  C.coloring(g0, 4)
//  iter_vertex(v => color_vertex(v, cols[Mark.get(v)]), g0)
//}

/* Algo II.

   we use marks to color; bits are used as follows:
     0: set if node is discarded at step 1
   1-4: available colors
   5-7: the color (0 = not colored, else color in 1..4
 */

//let print_8_bits = x =>
//  for i in 7 downto 0 {
//    if land(lsr(x, i), 1) == 1 {
//      printf("1")
//    } else {
//      printf("0")
//    }
//  }
//
//let dump = () => {
//  let dump_mark = v => {
//    printf("[")
//    print_8_bits(Mark.get(v))
//    printf("]")
//  }
//  iter_vertex(dump_mark, g0)
//  printf("\n")
//  flush(stdout)
//}
//
//let mask_color = [0, 0b11101, 0b11011, 0b10111, 0b01111]
//
//let coloring_b = () => {
//  /* initially all 4 colors available and every vertex to be colored */
//  iter_vertex(v => Mark.set(v, 0b11110), g0)
//  /* first step: we eliminate vertices with less than 4 successors */
//  let stack = Stack.create()
//  let finish = ref(false)
//  let round = ref(1)
//  let nb_to_color = ref(n)
//  while !finish.contents {
//    let c = ref(0)
//    finish := true
//    let erase = v => {
//      incr(c)
//      finish := false
//      Mark.set(v, 0b11111)
//      Stack.push(v, stack)
//    }
//
//    G.iter_vertex(v =>
//      if Mark.get(v) == 0 && out_degree(g0, v) < 4 {
//        erase(v)
//      }
//    , g0)
//    printf("round %d: removed %d vertices\n", round.contents, c.contents)
//    incr(round)
//    nb_to_color := nb_to_color.contents - c.contents
//  }
//  flush(stdout)
//  /* second step: we 4-color the remaining of the graph */
//  /* [try_color v i] tries to assigne color [i] to vertex [v] */
//  let try_color = (v, i) => {
//    assert (1 <= i && i <= 4)
//    let m = Mark.get(v)
//    assert (lsr(m, 5) == 0)
//    if land(lsr(m, i), 1) == 0 {
//      raise(NoColor)
//    } /* color [i] not available */
//    let remove_color = w => {
//      /* make color [i] unavailable for [w] */
//      let m = Mark.get(w)
//      if lsr(m, 5) > 0 {
//        assert (lsr(m, 5) != i) /* [w] already colored */
//      } else {
//        let m' = land(m, mask_color[i])
//        if m' == 0 {
//          raise(NoColor)
//        } /* no more color available for [w] */
//        Mark.set(w, m')
//      }
//    }
//
//    iter_succ(remove_color, g0, v)
//    Mark.set(v, lor(m, lsl(i, 5)))
//  }
//
//  let uncolor = v => {
//    let m = Mark.get(v)
//    let c = lsr(m, 5)
//    assert (0 <= c && c <= 4)
//    if c > 0 {
//      Mark.set(v, land(m, 0b11111))
//      let update = w =>
//        /* give back color [c] to [w] only when no more succ. has color [c] */
//        try {
//          iter_succ(u =>
//            if lsr(Mark.get(u), 5) == c {
//              raise(Exit)
//            }
//          , g0, w)
//          Mark.set(w, lor(Mark.get(w), lsl(1, c)))
//        } catch {
//        | Exit => ()
//        }
//
//      iter_succ(update, g0, v)
//    }
//  }
//
//  if nb_to_color.contents > 0 {
//    let rec iterate = iter => {
//      let v = Bfs.get(iter)
//      if land(Mark.get(v), 1) == 1 {
//        /* no need to color this vertex */
//        iterate(Bfs.step(iter))
//      } else {
//        for i in 1 to 4 {
//          try {
//            try_color(v, i)
//            iterate(Bfs.step(iter))
//            assert false
//          } catch {
//          | NoColor => uncolor(v)
//          }
//        }
//        raise(NoColor)
//      }
//    }
//
//    try iterate(Bfs.start(g0)) catch {
//    | Exit => ()
//    }
//  }
//  /* third step: we color the eliminated vertices, in reverse order */
//  Stack.iter(v => {
//    assert (land(Mark.get(v), 1) == 1)
//    try {
//      for i in 1 to 4 {
//        try {
//          try_color(v, i)
//          raise(Exit)
//        } catch {
//        | NoColor => uncolor(v)
//        }
//      }
//      assert false /* we must succeed */
//    } catch {
//    | Exit => ()
//    }
//  }, stack)
//  /* finally we display the coloring */
//  iter_vertex(v => {
//    let c = lsr(Mark.get(v), 5)
//    assert (1 <= c && c <= 4)
//    color_vertex(v, cols[c])
//  }, g0)
//}

//open Unix

//let utime = (f, x) => {
//  let u = times().tms_utime
//  let y = f(x)
//  let ut = times().tms_utime -. u
//  (y, ut)
//}
//
//let print_utime = (f, x) => {
//  let (y, ut) = utime(f, x)
//  Format.printf("user time: %2.2f@.", ut)
//  y
//}

let () = {
  //  draw_graph()
  test_bfs()
  test_dfs()
  //  print_utime(coloring_a, 4)
  /* ignore (Graphics.wait_next_event [ Key_pressed ]); */
  /* draw_graph (); */
  //  print_utime(coloring_b, ())
  //  ignore(Graphics.wait_next_event(list{Key_pressed}))
  //  close_graph()
}
