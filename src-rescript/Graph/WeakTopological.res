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

/* Copyright © 2015 Thibault Suzanne <thi.suzanne (@) gmail.com>
 * École Normale Supérieure, Département d'Informatique
 * Paris Sciences et Lettres
 */

/* Original algorithm by François Bourdoncle. See :
 * "Efficient chaotic iteration strategies with widenings",
 * Formal Methods in Programming and their Applications,
 * Springer Berlin Heidelberg, 1993.
 */

module type G = {
  type t
  module V: Sig.COMPARABLE
  let iter_vertex: (V.t => unit, t) => unit
  let iter_succ: (V.t => unit, t, V.t) => unit
}

type rec element<'a> =
  | Vertex('a)
  | Component('a, t<'a>)

and t<'a> = list<element<'a>>

let fold_left = List.fold_left

module Make = (G: G) => {
  module HT = Hashtbl.Make(G.V)

  let recursive_scc = (g, root_g) => {
    /* Straight OCaml implementation of the Section 4.3,
     fig. 4 algorithm in Bourdoncle's paper */
    let stack = Stack.create()
    let dfn = HT.create(1024)
    let num = ref(0)
    let partition = ref(list{})

    G.iter_vertex(v => HT.add(dfn, v, 0), g)

    let rec visit = (vertex, partition) => {
      let head = ref(0)
      let loop = ref(false)
      Stack.push(vertex, stack)
      incr(num)
      HT.replace(dfn, vertex, num.contents)
      head := num.contents
      G.iter_succ(succ => {
        let dfn_succ = HT.find(dfn, succ)
        let min = if dfn_succ == 0 {
          visit(succ, partition)
        } else {
          dfn_succ
        }
        if min <= head.contents {
          head := min
          loop := true
        }
      }, g, vertex)
      if head.contents == HT.find(dfn, vertex) {
        HT.replace(dfn, vertex, max_int)
        let element = ref(Stack.pop(stack))
        if loop.contents {
          while G.V.compare(element.contents, vertex) != 0 {
            HT.replace(dfn, element.contents, 0)
            element := Stack.pop(stack)
          }
          partition := list{component(vertex), ...partition.contents}
        } else {
          partition := list{Vertex(vertex), ...partition.contents}
        }
      }
      head.contents
    }

    and component = vertex => {
      let partition = ref(list{})
      G.iter_succ(succ =>
        if HT.find(dfn, succ) == 0 {
          ignore((visit(succ, partition): int))
        }
      , g, vertex)
      Component(vertex, partition.contents)
    }

    let _: int = visit(root_g, partition)
    partition.contents
  }
}
