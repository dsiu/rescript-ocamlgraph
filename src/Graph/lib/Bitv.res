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

/* i $Id: bitv.ml,v 1.18 2008/04/01 09:59:03 filliatr Exp $ i */

/* s Bit vectors. The interface and part of the code are borrowed from the
    [Array] module of the ocaml standard library (but things are simplified
    here since we can always initialize a bit vector). This module also
    provides bitwise operations. */

/* s We represent a bit vector by a vector of integers (field [bits]),
    and we keep the information of the size of the bit vector since it
    can not be found out with the size of the array (field [length]). */

type t = {
  length: int,
  bits: array<int>,
}

let length = v => v.length

/* s Each element of the array is an integer containing [bpi] bits, where
    [bpi] is determined according to the machine word size. Since we do not
    use the sign bit, [bpi] is 30 on a 32-bits machine and 62 on a 64-bits
    machines. We maintain the following invariant:
    {\em The unused bits of the last integer are always
    zeros.} This is ensured by [create] and maintained in other functions
    using [normalize]. [bit_j], [bit_not_j], [low_mask] and [up_mask]
    are arrays used to extract and mask bits in a single integer. */

let bpi = Sys.word_size - 2

let max_length = Sys.max_array_length * bpi

let bit_j = Array.init(bpi, j => lsl(1, j))
let bit_not_j = Array.init(bpi, j => max_int - bit_j[j])

let low_mask = Array.make(succ(bpi), 0)
let _ = for i in 1 to bpi {
  low_mask[i] = lor(low_mask[i - 1], bit_j[pred(i)])
}

let keep_lowest_bits = (a, j) => land(a, low_mask[j])

let high_mask = Array.init(succ(bpi), j => lsl(low_mask[j], bpi - j))

let keep_highest_bits = (a, j) => land(a, high_mask[j])

/* s Creating and normalizing a bit vector is easy: it is just a matter of
 taking care of the invariant. Copy is immediate. */

let create = (n, b) => {
  let initv = if b {
    max_int
  } else {
    0
  }
  let r = mod(n, bpi)
  if r == 0 {
    {length: n, bits: Array.make(n / bpi, initv)}
  } else {
    let s = n / bpi
    let b = Array.make(succ(s), initv)
    b[s] = land(b[s], low_mask[r])
    {length: n, bits: b}
  }
}

let normalize = v => {
  let r = mod(v.length, bpi)
  if r > 0 {
    let b = v.bits
    let s = Array.length(b)
    b[s - 1] = land(b[s - 1], low_mask[r])
  }
}

let copy = v => {length: v.length, bits: Array.copy(v.bits)}

/* s Access and assignment. The [n]th bit of a bit vector is the [j]th
    bit of the [i]th integer, where [i = n / bpi] and [j = n mod
    bpi]. Both [i] and [j] and computed by the function [pos].
    Accessing a bit is testing whether the result of the corresponding
    mask operation is non-zero, and assigning it is done with a
    bitwiwe operation: an {\em or} with [bit_j] to set it, and an {\em
    and} with [bit_not_j] to unset it. */

let pos = n => {
  let i = n / bpi and j = mod(n, bpi)
  if j < 0 {
    (i - 1, j + bpi)
  } else {
    (i, j)
  }
}

let unsafe_get = (v, n) => {
  let (i, j) = pos(n)
  land(Array.unsafe_get(v.bits, i), Array.unsafe_get(bit_j, j)) > 0
}

let unsafe_set = (v, n, b) => {
  let (i, j) = pos(n)
  if b {
    Array.unsafe_set(v.bits, i, lor(Array.unsafe_get(v.bits, i), Array.unsafe_get(bit_j, j)))
  } else {
    Array.unsafe_set(v.bits, i, land(Array.unsafe_get(v.bits, i), Array.unsafe_get(bit_not_j, j)))
  }
}

/* s The corresponding safe operations test the validiy of the access. */

let get = (v, n) => {
  if n < 0 || n >= v.length {
    invalid_arg("Bitv.get")
  }
  let (i, j) = pos(n)
  land(Array.unsafe_get(v.bits, i), Array.unsafe_get(bit_j, j)) > 0
}

let set = (v, n, b) => {
  if n < 0 || n >= v.length {
    invalid_arg("Bitv.set")
  }
  let (i, j) = pos(n)
  if b {
    Array.unsafe_set(v.bits, i, lor(Array.unsafe_get(v.bits, i), Array.unsafe_get(bit_j, j)))
  } else {
    Array.unsafe_set(v.bits, i, land(Array.unsafe_get(v.bits, i), Array.unsafe_get(bit_not_j, j)))
  }
}

/* s [init] is implemented naively using [unsafe_set]. */

let init = (n, f) => {
  let v = create(n, false)
  for i in 0 to pred(n) {
    unsafe_set(v, i, f(i))
  }
  v
}

/* s Handling bits by packets is the key for efficiency of functions
    [append], [concat], [sub] and [blit].
    We start by a very general function [blit_bits a i m v n] which blits
    the bits [i] to [i+m-1] of a native integer [a]
    onto the bit vector [v] at index [n]. It assumes that [i..i+m-1] and
    [n..n+m-1] are respectively valid subparts of [a] and [v].
    It is optimized when the bits fit the lowest boundary of an integer
    (case [j == 0]). */

let blit_bits = (a, i, m, v, n) => {
  let (i', j) = pos(n)
  if j === 0 {
    Array.unsafe_set(
      v,
      i',
      lor(keep_lowest_bits(lsr(a, i), m), keep_highest_bits(Array.unsafe_get(v, i'), bpi - m)),
    )
  } else {
    let d = m + j - bpi
    if d > 0 {
      Array.unsafe_set(
        v,
        i',
        lor(
          lsl(keep_lowest_bits(lsr(a, i), bpi - j), j),
          keep_lowest_bits(Array.unsafe_get(v, i'), j),
        ),
      )
      Array.unsafe_set(
        v,
        succ(i'),
        lor(
          keep_lowest_bits(lsr(a, i + bpi - j), d),
          keep_highest_bits(Array.unsafe_get(v, succ(i')), bpi - d),
        ),
      )
    } else {
      Array.unsafe_set(
        v,
        i',
        lor(
          lsl(keep_lowest_bits(lsr(a, i), m), j),
          land(Array.unsafe_get(v, i'), lor(low_mask[j], high_mask[-d])),
        ),
      )
    }
  }
}

/* s [blit_int] implements [blit_bits] in the particular case when
 [i=0] and [m=bpi] i.e. when we blit all the bits of [a]. */

let blit_int = (a, v, n) => {
  let (i, j) = pos(n)
  if j === 0 {
    Array.unsafe_set(v, i, a)
  } else {
    Array.unsafe_set(
      v,
      i,
      lor(keep_lowest_bits(Array.unsafe_get(v, i), j), lsl(keep_lowest_bits(a, bpi - j), j)),
    )
    Array.unsafe_set(
      v,
      succ(i),
      lor(keep_highest_bits(Array.unsafe_get(v, succ(i)), bpi - j), lsr(a, bpi - j)),
    )
  }
}

/* s When blitting a subpart of a bit vector into another bit vector, there
    are two possible cases: (1) all the bits are contained in a single integer
    of the first bit vector, and a single call to [blit_bits] is the
    only thing to do, or (2) the source bits overlap on several integers of
    the source array, and then we do a loop of [blit_int], with two calls
    to [blit_bits] for the two bounds. */

let unsafe_blit = (v1, ofs1, v2, ofs2, len) =>
  if len > 0 {
    let (bi, bj) = pos(ofs1)
    let (ei, ej) = pos(ofs1 + len - 1)
    if bi === ei {
      blit_bits(Array.unsafe_get(v1, bi), bj, len, v2, ofs2)
    } else {
      blit_bits(Array.unsafe_get(v1, bi), bj, bpi - bj, v2, ofs2)
      let n = ref(ofs2 + bpi - bj)
      for i in succ(bi) to pred(ei) {
        blit_int(Array.unsafe_get(v1, i), v2, n.contents)
        n := n.contents + bpi
      }
      blit_bits(Array.unsafe_get(v1, ei), 0, succ(ej), v2, n.contents)
    }
  }

let blit = (v1, ofs1, v2, ofs2, len) => {
  if len < 0 || (ofs1 < 0 || (ofs1 + len > v1.length || (ofs2 < 0 || ofs2 + len > v2.length))) {
    invalid_arg("Bitv.blit")
  }
  unsafe_blit(v1.bits, ofs1, v2.bits, ofs2, len)
}

/* s Extracting the subvector [ofs..ofs+len-1] of [v] is just creating a
 new vector of length [len] and blitting the subvector of [v] inside. */

let sub = (v, ofs, len) => {
  if ofs < 0 || (len < 0 || ofs + len > v.length) {
    invalid_arg("Bitv.sub")
  }
  let r = create(len, false)
  unsafe_blit(v.bits, ofs, r.bits, 0, len)
  r
}

/* s The concatenation of two bit vectors [v1] and [v2] is obtained by
    creating a vector for the result and blitting inside the two vectors.
    [v1] is copied directly. */

let append = (v1, v2) => {
  let l1 = v1.length
  and l2 = v2.length
  let r = create(l1 + l2, false)
  let b1 = v1.bits
  let b2 = v2.bits
  let b = r.bits
  for i in 0 to Array.length(b1) - 1 {
    Array.unsafe_set(b, i, Array.unsafe_get(b1, i))
  }
  unsafe_blit(b2, 0, b, l1, l2)
  r
}

/* s The concatenation of a list of bit vectors is obtained by iterating
 [unsafe_blit]. */

let concat = vl => {
  let size = List.fold_left((sz, v) => sz + v.length, 0, vl)
  let res = create(size, false)
  let b = res.bits
  let pos = ref(0)
  List.iter(v => {
    let n = v.length
    unsafe_blit(v.bits, 0, b, pos.contents, n)
    pos := pos.contents + n
  }, vl)
  res
}

/* s Filling is a particular case of blitting with a source made of all
    ones or all zeros. Thus we instanciate [unsafe_blit], with 0 and
    [max_int]. */

let blit_zeros = (v, ofs, len) =>
  if len > 0 {
    let (bi, bj) = pos(ofs)
    let (ei, ej) = pos(ofs + len - 1)
    if bi === ei {
      blit_bits(0, bj, len, v, ofs)
    } else {
      blit_bits(0, bj, bpi - bj, v, ofs)
      let n = ref(ofs + bpi - bj)
      for _i in succ(bi) to pred(ei) {
        blit_int(0, v, n.contents)
        n := n.contents + bpi
      }
      blit_bits(0, 0, succ(ej), v, n.contents)
    }
  }

let blit_ones = (v, ofs, len) =>
  if len > 0 {
    let (bi, bj) = pos(ofs)
    let (ei, ej) = pos(ofs + len - 1)
    if bi === ei {
      blit_bits(max_int, bj, len, v, ofs)
    } else {
      blit_bits(max_int, bj, bpi - bj, v, ofs)
      let n = ref(ofs + bpi - bj)
      for _i in succ(bi) to pred(ei) {
        blit_int(max_int, v, n.contents)
        n := n.contents + bpi
      }
      blit_bits(max_int, 0, succ(ej), v, n.contents)
    }
  }

let fill = (v, ofs, len, b) => {
  if ofs < 0 || (len < 0 || ofs + len > v.length) {
    invalid_arg("Bitv.fill")
  }
  if b {
    blit_ones(v.bits, ofs, len)
  } else {
    blit_zeros(v.bits, ofs, len)
  }
}

/* s All the iterators are implemented as for traditional arrays, using
    [unsafe_get]. For [iter] and [map], we do not precompute [(f
    true)] and [(f false)] since [f] is likely to have
    side-effects. */

let iter = (f, v) =>
  for i in 0 to v.length - 1 {
    f(unsafe_get(v, i))
  }

let map = (f, v) => {
  let l = v.length
  let r = create(l, false)
  for i in 0 to l - 1 {
    unsafe_set(r, i, f(unsafe_get(v, i)))
  }
  r
}

let iteri = (f, v) =>
  for i in 0 to v.length - 1 {
    f(i, unsafe_get(v, i))
  }

let mapi = (f, v) => {
  let l = v.length
  let r = create(l, false)
  for i in 0 to l - 1 {
    unsafe_set(r, i, f(i, unsafe_get(v, i)))
  }
  r
}

let fold_left = (f, x, v) => {
  let r = ref(x)
  for i in 0 to v.length - 1 {
    r := f(r.contents, unsafe_get(v, i))
  }
  r.contents
}

let fold_right = (f, v, x) => {
  let r = ref(x)
  for i in v.length - 1 downto 0 {
    r := f(unsafe_get(v, i), r.contents)
  }
  r.contents
}

let foldi_left = (f, x, v) => {
  let r = ref(x)
  for i in 0 to v.length - 1 {
    r := f(r.contents, i, unsafe_get(v, i))
  }
  r.contents
}

let foldi_right = (f, v, x) => {
  let r = ref(x)
  for i in v.length - 1 downto 0 {
    r := f(i, unsafe_get(v, i), r.contents)
  }
  r.contents
}

/* s Bitwise operations. It is straigthforward, since bitwise operations
    can be realized by the corresponding bitwise operations over integers.
    However, one has to take care of normalizing the result of [bwnot]
    which introduces ones in highest significant positions. */

let bw_and = (v1, v2) => {
  let l = v1.length
  if l != v2.length {
    invalid_arg("Bitv.bw_and")
  }
  let b1 = v1.bits
  and b2 = v2.bits
  let n = Array.length(b1)
  let a = Array.make(n, 0)
  for i in 0 to n - 1 {
    a[i] = land(b1[i], b2[i])
  }
  {length: l, bits: a}
}

let bw_or = (v1, v2) => {
  let l = v1.length
  if l != v2.length {
    invalid_arg("Bitv.bw_or")
  }
  let b1 = v1.bits
  and b2 = v2.bits
  let n = Array.length(b1)
  let a = Array.make(n, 0)
  for i in 0 to n - 1 {
    a[i] = lor(b1[i], b2[i])
  }
  {length: l, bits: a}
}

let bw_xor = (v1, v2) => {
  let l = v1.length
  if l != v2.length {
    invalid_arg("Bitv.bw_xor")
  }
  let b1 = v1.bits
  and b2 = v2.bits
  let n = Array.length(b1)
  let a = Array.make(n, 0)
  for i in 0 to n - 1 {
    a[i] = lxor(b1[i], b2[i])
  }
  {length: l, bits: a}
}

let bw_not = v => {
  let b = v.bits
  let n = Array.length(b)
  let a = Array.make(n, 0)
  for i in 0 to n - 1 {
    a[i] = land(max_int, lnot(b[i]))
  }
  let r = {length: v.length, bits: a}
  normalize(r)
  r
}

/* s Shift operations. It is easy to reuse [unsafe_blit], although it is
 probably slightly less efficient than a ad-hoc piece of code. */

let rec shiftl = (v, d) =>
  if d === 0 {
    copy(v)
  } else if d < 0 {
    shiftr(v, -d)
  } else {
    let n = v.length
    let r = create(n, false)
    if d < n {
      unsafe_blit(v.bits, 0, r.bits, d, n - d)
    }
    r
  }

and shiftr = (v, d) =>
  if d === 0 {
    copy(v)
  } else if d < 0 {
    shiftl(v, -d)
  } else {
    let n = v.length
    let r = create(n, false)
    if d < n {
      unsafe_blit(v.bits, d, r.bits, 0, n - d)
    }
    r
  }

/* s Testing for all zeros and all ones. */

let all_zeros = v => {
  let b = v.bits
  let n = Array.length(b)
  let rec test = i => i === n || (Array.unsafe_get(b, i) === 0 && test(succ(i)))

  test(0)
}

let all_ones = v => {
  let b = v.bits
  let n = Array.length(b)
  let rec test = i =>
    if i === n - 1 {
      let m = mod(v.length, bpi)
      Array.unsafe_get(b, i) === if m === 0 {
          max_int
        } else {
          low_mask[m]
        }
    } else {
      Array.unsafe_get(b, i) === max_int && test(succ(i))
    }

  test(0)
}

/* s Conversions to and from strings. */

let to_string = v => {
  let n = v.length
  let s = Buffer.create(n)
  for i in 0 to n - 1 {
    Buffer.add_char(
      s,
      if unsafe_get(v, i) {
        '1'
      } else {
        '0'
      },
    )
  }
  Buffer.contents(s)
}

@@warning("-3")
let print = (fmt, v) => Format.pp_print_string(fmt, to_string(v))

let of_string = s => {
  let n = String.length(s)
  let v = create(n, false)
  for i in 0 to n - 1 {
    let c = String.unsafe_get(s, i)
    if c == '1' {
      unsafe_set(v, i, true)
    } else if c != '0' {
      invalid_arg("Bitv.of_string")
    }
  }
  v
}

/* s Iteration on all bit vectors of length [n] using a Gray code. */

let first_set = (v, n) => {
  let rec lookup = i => {
    if i == n {
      raise(Not_found)
    }
    if unsafe_get(v, i) {
      i
    } else {
      lookup(i + 1)
    }
  }

  lookup(0)
}

let gray_iter = (f, n) => {
  let bv = create(n, false)
  let rec iter = () => {
    f(bv)
    unsafe_set(bv, 0, !unsafe_get(bv, 0))
    f(bv)
    let pos = succ(first_set(bv, n))
    if pos < n {
      unsafe_set(bv, pos, !unsafe_get(bv, pos))
      iter()
    }
  }

  if n > 0 {
    iter()
  }
}

/* s Coercions to/from lists of integers */

let of_list = l => {
  let n = List.fold_left(max, 0, l)
  let b = create(succ(n), false)
  let add_element = i => {
    /* negative numbers are invalid */
    if i < 0 {
      invalid_arg("Bitv.of_list")
    }
    unsafe_set(b, i, true)
  }

  List.iter(add_element, l)
  b
}

let of_list_with_length = (l, len) => {
  let b = create(len, false)
  let add_element = i => {
    if i < 0 || i >= len {
      invalid_arg("Bitv.of_list_with_length")
    }
    unsafe_set(b, i, true)
  }

  List.iter(add_element, l)
  b
}

let to_list = b => {
  let n = length(b)
  let rec make = (i, acc) =>
    if i < 0 {
      acc
    } else {
      make(
        pred(i),
        if unsafe_get(b, i) {
          list{i, ...acc}
        } else {
          acc
        },
      )
    }

  make(pred(n), list{})
}

/* s To/from integers. */

/* [int] */
let of_int_us = i => {length: bpi, bits: [land(i, max_int)]}
let to_int_us = v => {
  if v.length < bpi {
    invalid_arg("Bitv.to_int_us")
  }
  v.bits[0]
}

let of_int_s = i => {length: succ(bpi), bits: [land(i, max_int), land(lsr(i, bpi), 1)]}
let to_int_s = v => {
  if v.length < succ(bpi) {
    invalid_arg("Bitv.to_int_s")
  }
  lor(v.bits[0], lsl(v.bits[1], bpi))
}

/* [Int32] */
let of_int32_us = i =>
  switch Sys.word_size {
  | 32 => {
      length: 31,
      bits: [
        land(Int32.to_int(i), max_int),
        {
          let hi = Int32.shift_right_logical(i, 30)
          land(Int32.to_int(hi), 1)
        },
      ],
    }
  | 64 => {length: 31, bits: [land(Int32.to_int(i), 0x7fffffff)]}
  | _ => assert false
  }
let to_int32_us = v => {
  if v.length < 31 {
    invalid_arg("Bitv.to_int32_us")
  }
  switch Sys.word_size {
  | 32 =>
    Int32.logor(Int32.of_int(v.bits[0]), Int32.shift_left(Int32.of_int(land(v.bits[1], 1)), 30))
  | 64 => Int32.of_int(land(v.bits[0], 0x7fffffff))
  | _ => assert false
  }
}

/* this is 0xffffffff (ocaml >= 3.08 checks for literal overflow) */
let ffffffff = lor(lsl(0xffff, 16), 0xffff)

let of_int32_s = i =>
  switch Sys.word_size {
  | 32 => {
      length: 32,
      bits: [
        land(Int32.to_int(i), max_int),
        {
          let hi = Int32.shift_right_logical(i, 30)
          land(Int32.to_int(hi), 3)
        },
      ],
    }
  | 64 => {length: 32, bits: [land(Int32.to_int(i), ffffffff)]}
  | _ => assert false
  }
let to_int32_s = v => {
  if v.length < 32 {
    invalid_arg("Bitv.to_int32_s")
  }
  switch Sys.word_size {
  | 32 =>
    Int32.logor(Int32.of_int(v.bits[0]), Int32.shift_left(Int32.of_int(land(v.bits[1], 3)), 30))
  | 64 => Int32.of_int(land(v.bits[0], ffffffff))
  | _ => assert false
  }
}

/* [Int64] */
let of_int64_us = i =>
  switch Sys.word_size {
  | 32 => {
      length: 63,
      bits: [
        land(Int64.to_int(i), max_int),
        {
          let mi = Int64.shift_right_logical(i, 30)
          land(Int64.to_int(mi), max_int)
        },
        {
          let hi = Int64.shift_right_logical(i, 60)
          land(Int64.to_int(hi), 1)
        },
      ],
    }
  | 64 => {
      length: 63,
      bits: [
        land(Int64.to_int(i), max_int),
        {
          let hi = Int64.shift_right_logical(i, 62)
          land(Int64.to_int(hi), 1)
        },
      ],
    }
  | _ => assert false
  }
let to_int64_us = _ => failwith("todo")

let of_int64_s = _ => failwith("todo")
let to_int64_s = _ => failwith("todo")

/* [Nativeint] */
//let select_of = (f32, f64) =>
//  switch Sys.word_size {
//  | 32 => i => f32(Nativeint.to_int32(i))
//  | 64 => i => f64(Int64.of_nativeint(i))
//  | _ => assert false
//  }
//let of_nativeint_s = select_of(of_int32_s, of_int64_s)
//let of_nativeint_us = select_of(of_int32_us, of_int64_us)
//let select_to = (f32, f64) =>
//  switch Sys.word_size {
//  | 32 => i => Nativeint.of_int32(f32(i))
//  | 64 => i => Int64.to_nativeint(f64(i))
//  | _ => assert false
//  }
//let to_nativeint_s = select_to(to_int32_s, to_int64_s)
//let to_nativeint_us = select_to(to_int32_us, to_int64_us)
