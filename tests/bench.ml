(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2007                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format

(* bench program for Ocamlgraph *)

module Time = struct

(*  open Unix*)

  let utime f x =
(*    let u = (times()).tms_utime in*)
    let u = Js.Date.now() in
    let y = f x in
(*    let ut = (times()).tms_utime -. u in*)
    let ut = Js.Date.now() -. u in
    (y,ut)

  let print f x =
    let (y,ut) = utime f x in
    printf "user time: %2.2f@." ut;
    y

end

open Graph

module Bench
  (G : Sig.G with type V.label = int)
  (B : Builder.S with module G = G) =
struct

  module R = Rand.Make(B)

  (*1. Parcours *)

  module Dfs = Traverse.Dfs(G)

  (* d�tection de cycle *)

  let has_cycle v e =
    let g = R.graph ~v ~e () in
    let b,t = Time.utime Dfs.has_cycle g in
    printf "v = %d e = %d cycle = %b time = %2.2f@." v e b t;
    b

  let bench1 () =
    (* on augmente E jusqu'� trouver un cycle *)
    let v = 20000 in
    let e = ref 1 in
    while not (has_cycle v !e) do e := 2 * !e done

  let () =
    Printf.printf "======== BEGIN bench1 =========\n";
    bench1();
    Printf.printf "======== END bench1 =========\n\n"

  (* r�sultats :
     le temps d'une recherche de cycle n�gative ne d�pend pas de
     E et est d'environ 1s pour 10^6 noeuds *)

  (* 2. composantes fortement connexes *)

  module C = Components.Make(G)

  let bench2 () =
    for i = 1 to 100 do
      let v = i * i in
      let d = Random.float 1.0 in
      let e = truncate (let x = d *. (float v -. 1.0) in x *. x) in
      let g = R.graph ~v ~e () in
      let _,t = Time.utime C.scc g in
      printf "i = %d v = %d e = %d d = %1.2f time = %2.2f@." i v e d t
    done

  (* r�sultats : voir results.bench2.txt *)

   let () =
    Printf.printf "======== BEGIN bench2 =========\n";
    bench2();
    Printf.printf "======== END bench2 =========\n\n"

end

module B1 = Bench(Pack.Digraph)(Builder.I(Pack.Digraph))
(*
Local Variables:
compile-command: "make -C .. bench"
End:
*)
