// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Sys from "rescript/lib/es6/sys.js";
import * as List from "rescript/lib/es6/list.js";
import * as $$Array from "rescript/lib/es6/array.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as $$Buffer from "rescript/lib/es6/buffer.js";
import * as Format from "rescript/lib/es6/format.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";

function length(v) {
  return v.length;
}

var bpi = Sys.word_size - 2 | 0;

var max_length = Math.imul(Sys.max_array_length, bpi);

var bit_j = $$Array.init(bpi, (function (j) {
        return (1 << j);
      }));

var bit_not_j = $$Array.init(bpi, (function (j) {
        return Pervasives.max_int - Caml_array.get(bit_j, j) | 0;
      }));

var low_mask = Caml_array.make(bpi + 1 | 0, 0);

for(var i = 1; i <= bpi; ++i){
  Caml_array.set(low_mask, i, Caml_array.get(low_mask, i - 1 | 0) | Caml_array.get(bit_j, i - 1 | 0));
}

function keep_lowest_bits(a, j) {
  return a & Caml_array.get(low_mask, j);
}

var high_mask = $$Array.init(bpi + 1 | 0, (function (j) {
        return (Caml_array.get(low_mask, j) << (bpi - j | 0));
      }));

function keep_highest_bits(a, j) {
  return a & Caml_array.get(high_mask, j);
}

function create(n, b) {
  var initv = b ? Pervasives.max_int : 0;
  var r = Caml_int32.mod_(n, bpi);
  if (r === 0) {
    return {
            length: n,
            bits: Caml_array.make(Caml_int32.div(n, bpi), initv)
          };
  }
  var s = Caml_int32.div(n, bpi);
  var b$1 = Caml_array.make(s + 1 | 0, initv);
  Caml_array.set(b$1, s, Caml_array.get(b$1, s) & Caml_array.get(low_mask, r));
  return {
          length: n,
          bits: b$1
        };
}

function normalize(v) {
  var r = Caml_int32.mod_(v.length, bpi);
  if (r <= 0) {
    return ;
  }
  var b = v.bits;
  var s = b.length;
  return Caml_array.set(b, s - 1 | 0, Caml_array.get(b, s - 1 | 0) & Caml_array.get(low_mask, r));
}

function copy(v) {
  return {
          length: v.length,
          bits: $$Array.copy(v.bits)
        };
}

function pos(n) {
  var i = Caml_int32.div(n, bpi);
  var j = Caml_int32.mod_(n, bpi);
  if (j < 0) {
    return [
            i - 1 | 0,
            j + bpi | 0
          ];
  } else {
    return [
            i,
            j
          ];
  }
}

function unsafe_get(v, n) {
  var match = pos(n);
  return (v.bits[match[0]] & bit_j[match[1]]) > 0;
}

function unsafe_set(v, n, b) {
  var match = pos(n);
  var j = match[1];
  var i = match[0];
  if (b) {
    v.bits[i] = v.bits[i] | bit_j[j];
  } else {
    v.bits[i] = v.bits[i] & bit_not_j[j];
  }
  
}

function get(v, n) {
  if (n < 0 || n >= v.length) {
    Pervasives.invalid_arg("Bitv.get");
  }
  var match = pos(n);
  return (v.bits[match[0]] & bit_j[match[1]]) > 0;
}

function set(v, n, b) {
  if (n < 0 || n >= v.length) {
    Pervasives.invalid_arg("Bitv.set");
  }
  var match = pos(n);
  var j = match[1];
  var i = match[0];
  if (b) {
    v.bits[i] = v.bits[i] | bit_j[j];
  } else {
    v.bits[i] = v.bits[i] & bit_not_j[j];
  }
  
}

function init(n, f) {
  var v = create(n, false);
  for(var i = 0; i < n; ++i){
    unsafe_set(v, i, Curry._1(f, i));
  }
  return v;
}

function blit_bits(a, i, m, v, n) {
  var match = pos(n);
  var j = match[1];
  var i$p = match[0];
  if (j === 0) {
    v[i$p] = keep_lowest_bits((a >>> i) | 0, m) | keep_highest_bits(v[i$p], bpi - m | 0);
    return ;
  }
  var d = (m + j | 0) - bpi | 0;
  if (d > 0) {
    v[i$p] = (keep_lowest_bits((a >>> i) | 0, bpi - j | 0) << j) | keep_lowest_bits(v[i$p], j);
    v[i$p + 1 | 0] = keep_lowest_bits((a >>> ((i + bpi | 0) - j | 0)) | 0, d) | keep_highest_bits(v[i$p + 1 | 0], bpi - d | 0);
  } else {
    v[i$p] = (keep_lowest_bits((a >>> i) | 0, m) << j) | v[i$p] & (Caml_array.get(low_mask, j) | Caml_array.get(high_mask, -d | 0));
  }
  
}

function blit_int(a, v, n) {
  var match = pos(n);
  var j = match[1];
  var i = match[0];
  if (j === 0) {
    v[i] = a;
  } else {
    v[i] = keep_lowest_bits(v[i], j) | (keep_lowest_bits(a, bpi - j | 0) << j);
    v[i + 1 | 0] = keep_highest_bits(v[i + 1 | 0], bpi - j | 0) | (a >>> (bpi - j | 0)) | 0;
  }
  
}

function unsafe_blit(v1, ofs1, v2, ofs2, len) {
  if (len <= 0) {
    return ;
  }
  var match = pos(ofs1);
  var bj = match[1];
  var bi = match[0];
  var match$1 = pos((ofs1 + len | 0) - 1 | 0);
  var ei = match$1[0];
  if (bi === ei) {
    return blit_bits(v1[bi], bj, len, v2, ofs2);
  }
  blit_bits(v1[bi], bj, bpi - bj | 0, v2, ofs2);
  var n = (ofs2 + bpi | 0) - bj | 0;
  for(var i = bi + 1 | 0; i < ei; ++i){
    blit_int(v1[i], v2, n);
    n = n + bpi | 0;
  }
  return blit_bits(v1[ei], 0, match$1[1] + 1 | 0, v2, n);
}

function blit(v1, ofs1, v2, ofs2, len) {
  if (len < 0 || ofs1 < 0 || (ofs1 + len | 0) > v1.length || ofs2 < 0 || (ofs2 + len | 0) > v2.length) {
    Pervasives.invalid_arg("Bitv.blit");
  }
  return unsafe_blit(v1.bits, ofs1, v2.bits, ofs2, len);
}

function sub(v, ofs, len) {
  if (ofs < 0 || len < 0 || (ofs + len | 0) > v.length) {
    Pervasives.invalid_arg("Bitv.sub");
  }
  var r = create(len, false);
  unsafe_blit(v.bits, ofs, r.bits, 0, len);
  return r;
}

function append(v1, v2) {
  var l1 = v1.length;
  var l2 = v2.length;
  var r = create(l1 + l2 | 0, false);
  var b1 = v1.bits;
  var b2 = v2.bits;
  var b = r.bits;
  for(var i = 0 ,i_finish = b1.length; i < i_finish; ++i){
    b[i] = b1[i];
  }
  unsafe_blit(b2, 0, b, l1, l2);
  return r;
}

function concat(vl) {
  var size = List.fold_left((function (sz, v) {
          return sz + v.length | 0;
        }), 0, vl);
  var res = create(size, false);
  var b = res.bits;
  var pos = {
    contents: 0
  };
  List.iter((function (v) {
          var n = v.length;
          unsafe_blit(v.bits, 0, b, pos.contents, n);
          pos.contents = pos.contents + n | 0;
          
        }), vl);
  return res;
}

function fill(v, ofs, len, b) {
  if (ofs < 0 || len < 0 || (ofs + len | 0) > v.length) {
    Pervasives.invalid_arg("Bitv.fill");
  }
  if (b) {
    var v$1 = v.bits;
    if (len <= 0) {
      return ;
    }
    var match = pos(ofs);
    var bj = match[1];
    var bi = match[0];
    var match$1 = pos((ofs + len | 0) - 1 | 0);
    var ei = match$1[0];
    if (bi === ei) {
      return blit_bits(Pervasives.max_int, bj, len, v$1, ofs);
    }
    blit_bits(Pervasives.max_int, bj, bpi - bj | 0, v$1, ofs);
    var n = (ofs + bpi | 0) - bj | 0;
    for(var _i = bi + 1 | 0; _i < ei; ++_i){
      blit_int(Pervasives.max_int, v$1, n);
      n = n + bpi | 0;
    }
    return blit_bits(Pervasives.max_int, 0, match$1[1] + 1 | 0, v$1, n);
  } else {
    var v$2 = v.bits;
    if (len <= 0) {
      return ;
    }
    var match$2 = pos(ofs);
    var bj$1 = match$2[1];
    var bi$1 = match$2[0];
    var match$3 = pos((ofs + len | 0) - 1 | 0);
    var ei$1 = match$3[0];
    if (bi$1 === ei$1) {
      return blit_bits(0, bj$1, len, v$2, ofs);
    }
    blit_bits(0, bj$1, bpi - bj$1 | 0, v$2, ofs);
    var n$1 = (ofs + bpi | 0) - bj$1 | 0;
    for(var _i$1 = bi$1 + 1 | 0; _i$1 < ei$1; ++_i$1){
      blit_int(0, v$2, n$1);
      n$1 = n$1 + bpi | 0;
    }
    return blit_bits(0, 0, match$3[1] + 1 | 0, v$2, n$1);
  }
}

function iter(f, v) {
  for(var i = 0 ,i_finish = v.length; i < i_finish; ++i){
    Curry._1(f, unsafe_get(v, i));
  }
  
}

function map(f, v) {
  var l = v.length;
  var r = create(l, false);
  for(var i = 0; i < l; ++i){
    unsafe_set(r, i, Curry._1(f, unsafe_get(v, i)));
  }
  return r;
}

function iteri(f, v) {
  for(var i = 0 ,i_finish = v.length; i < i_finish; ++i){
    Curry._2(f, i, unsafe_get(v, i));
  }
  
}

function mapi(f, v) {
  var l = v.length;
  var r = create(l, false);
  for(var i = 0; i < l; ++i){
    unsafe_set(r, i, Curry._2(f, i, unsafe_get(v, i)));
  }
  return r;
}

function fold_left(f, x, v) {
  var r = x;
  for(var i = 0 ,i_finish = v.length; i < i_finish; ++i){
    r = Curry._2(f, r, unsafe_get(v, i));
  }
  return r;
}

function fold_right(f, v, x) {
  var r = x;
  for(var i = v.length - 1 | 0; i >= 0; --i){
    r = Curry._2(f, unsafe_get(v, i), r);
  }
  return r;
}

function foldi_left(f, x, v) {
  var r = x;
  for(var i = 0 ,i_finish = v.length; i < i_finish; ++i){
    r = Curry._3(f, r, i, unsafe_get(v, i));
  }
  return r;
}

function foldi_right(f, v, x) {
  var r = x;
  for(var i = v.length - 1 | 0; i >= 0; --i){
    r = Curry._3(f, i, unsafe_get(v, i), r);
  }
  return r;
}

function bw_and(v1, v2) {
  var l = v1.length;
  if (l !== v2.length) {
    Pervasives.invalid_arg("Bitv.bw_and");
  }
  var b1 = v1.bits;
  var b2 = v2.bits;
  var n = b1.length;
  var a = Caml_array.make(n, 0);
  for(var i = 0; i < n; ++i){
    Caml_array.set(a, i, Caml_array.get(b1, i) & Caml_array.get(b2, i));
  }
  return {
          length: l,
          bits: a
        };
}

function bw_or(v1, v2) {
  var l = v1.length;
  if (l !== v2.length) {
    Pervasives.invalid_arg("Bitv.bw_or");
  }
  var b1 = v1.bits;
  var b2 = v2.bits;
  var n = b1.length;
  var a = Caml_array.make(n, 0);
  for(var i = 0; i < n; ++i){
    Caml_array.set(a, i, Caml_array.get(b1, i) | Caml_array.get(b2, i));
  }
  return {
          length: l,
          bits: a
        };
}

function bw_xor(v1, v2) {
  var l = v1.length;
  if (l !== v2.length) {
    Pervasives.invalid_arg("Bitv.bw_xor");
  }
  var b1 = v1.bits;
  var b2 = v2.bits;
  var n = b1.length;
  var a = Caml_array.make(n, 0);
  for(var i = 0; i < n; ++i){
    Caml_array.set(a, i, Caml_array.get(b1, i) ^ Caml_array.get(b2, i));
  }
  return {
          length: l,
          bits: a
        };
}

function bw_not(v) {
  var b = v.bits;
  var n = b.length;
  var a = Caml_array.make(n, 0);
  for(var i = 0; i < n; ++i){
    Caml_array.set(a, i, Pervasives.max_int & Pervasives.lnot(Caml_array.get(b, i)));
  }
  var r_length = v.length;
  var r = {
    length: r_length,
    bits: a
  };
  normalize(r);
  return r;
}

function shiftl(v, d) {
  if (d === 0) {
    return copy(v);
  }
  if (d < 0) {
    return shiftr(v, -d | 0);
  }
  var n = v.length;
  var r = create(n, false);
  if (d < n) {
    unsafe_blit(v.bits, 0, r.bits, d, n - d | 0);
  }
  return r;
}

function shiftr(v, d) {
  if (d === 0) {
    return copy(v);
  }
  if (d < 0) {
    return shiftl(v, -d | 0);
  }
  var n = v.length;
  var r = create(n, false);
  if (d < n) {
    unsafe_blit(v.bits, d, r.bits, 0, n - d | 0);
  }
  return r;
}

function all_zeros(v) {
  var b = v.bits;
  var n = b.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === n) {
      return true;
    }
    if (b[i] !== 0) {
      return false;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function all_ones(v) {
  var b = v.bits;
  var n = b.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === (n - 1 | 0)) {
      var m = Caml_int32.mod_(v.length, bpi);
      return b[i] === (
              m === 0 ? Pervasives.max_int : Caml_array.get(low_mask, m)
            );
    }
    if (b[i] !== Pervasives.max_int) {
      return false;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function to_string(v) {
  var n = v.length;
  var s = $$Buffer.create(n);
  for(var i = 0; i < n; ++i){
    $$Buffer.add_char(s, unsafe_get(v, i) ? /* '1' */49 : /* '0' */48);
  }
  return $$Buffer.contents(s);
}

function print(fmt, v) {
  return Format.pp_print_string(fmt, to_string(v));
}

function of_string(s) {
  var n = s.length;
  var v = create(n, false);
  for(var i = 0; i < n; ++i){
    var c = s.charCodeAt(i);
    if (c === /* '1' */49) {
      unsafe_set(v, i, true);
    } else if (c !== /* '0' */48) {
      Pervasives.invalid_arg("Bitv.of_string");
    }
    
  }
  return v;
}

function first_set(v, n) {
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === n) {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    if (unsafe_get(v, i)) {
      return i;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function gray_iter(f, n) {
  var bv = create(n, false);
  if (n > 0) {
    var _param;
    while(true) {
      Curry._1(f, bv);
      unsafe_set(bv, 0, !unsafe_get(bv, 0));
      Curry._1(f, bv);
      var pos = first_set(bv, n) + 1 | 0;
      if (pos >= n) {
        return ;
      }
      unsafe_set(bv, pos, !unsafe_get(bv, pos));
      _param = undefined;
      continue ;
    };
  }
  
}

function of_list(l) {
  var n = List.fold_left((function (prim0, prim1) {
          if (prim0 > prim1) {
            return prim0;
          } else {
            return prim1;
          }
        }), 0, l);
  var b = create(n + 1 | 0, false);
  var add_element = function (i) {
    if (i < 0) {
      Pervasives.invalid_arg("Bitv.of_list");
    }
    return unsafe_set(b, i, true);
  };
  List.iter(add_element, l);
  return b;
}

function of_list_with_length(l, len) {
  var b = create(len, false);
  var add_element = function (i) {
    if (i < 0 || i >= len) {
      Pervasives.invalid_arg("Bitv.of_list_with_length");
    }
    return unsafe_set(b, i, true);
  };
  List.iter(add_element, l);
  return b;
}

function to_list(b) {
  var n = b.length;
  var _i = n - 1 | 0;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var i = _i;
    if (i < 0) {
      return acc;
    }
    _acc = unsafe_get(b, i) ? ({
          hd: i,
          tl: acc
        }) : acc;
    _i = i - 1 | 0;
    continue ;
  };
}

function of_int_us(i) {
  return {
          length: bpi,
          bits: [i & Pervasives.max_int]
        };
}

function to_int_us(v) {
  if (v.length < bpi) {
    Pervasives.invalid_arg("Bitv.to_int_us");
  }
  return Caml_array.get(v.bits, 0);
}

function of_int_s(i) {
  return {
          length: bpi + 1 | 0,
          bits: [
            i & Pervasives.max_int,
            (i >>> bpi) & 1
          ]
        };
}

function to_int_s(v) {
  if (v.length < (bpi + 1 | 0)) {
    Pervasives.invalid_arg("Bitv.to_int_s");
  }
  return Caml_array.get(v.bits, 0) | (Caml_array.get(v.bits, 1) << bpi);
}

function of_int32_us(i) {
  if (Sys.word_size !== 32) {
    if (Sys.word_size !== 64) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "bitv.ml",
              549,
              9
            ],
            Error: new Error()
          };
    }
    return {
            length: 31,
            bits: [i & 2147483647]
          };
  }
  var hi = (i >>> 30);
  return {
          length: 31,
          bits: [
            i & Pervasives.max_int,
            hi & 1
          ]
        };
}

function to_int32_us(v) {
  if (v.length < 31) {
    Pervasives.invalid_arg("Bitv.to_int32_us");
  }
  if (Sys.word_size === 32) {
    return Caml_array.get(v.bits, 0) | ((Caml_array.get(v.bits, 1) & 1) << 30);
  }
  if (Sys.word_size !== 64) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "bitv.ml",
            558,
            11
          ],
          Error: new Error()
        };
  }
  return Caml_array.get(v.bits, 0) & 2147483647;
}

function of_int32_s(i) {
  if (Sys.word_size !== 32) {
    if (Sys.word_size !== 64) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "bitv.ml",
              569,
              9
            ],
            Error: new Error()
          };
    }
    return {
            length: 32,
            bits: [i & -1]
          };
  }
  var hi = (i >>> 30);
  return {
          length: 32,
          bits: [
            i & Pervasives.max_int,
            hi & 3
          ]
        };
}

function to_int32_s(v) {
  if (v.length < 32) {
    Pervasives.invalid_arg("Bitv.to_int32_s");
  }
  if (Sys.word_size === 32) {
    return Caml_array.get(v.bits, 0) | ((Caml_array.get(v.bits, 1) & 3) << 30);
  }
  if (Sys.word_size !== 64) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "bitv.ml",
            578,
            11
          ],
          Error: new Error()
        };
  }
  return Caml_array.get(v.bits, 0) & -1;
}

function of_int64_us(i) {
  if (Sys.word_size !== 32) {
    if (Sys.word_size !== 64) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "bitv.ml",
              592,
              9
            ],
            Error: new Error()
          };
    }
    var hi = Caml_int64.lsr_(i, 62);
    return {
            length: 63,
            bits: [
              Caml_int64.to_int32(i) & Pervasives.max_int,
              Caml_int64.to_int32(hi) & 1
            ]
          };
  }
  var mi = Caml_int64.lsr_(i, 30);
  var hi$1 = Caml_int64.lsr_(i, 60);
  return {
          length: 63,
          bits: [
            Caml_int64.to_int32(i) & Pervasives.max_int,
            Caml_int64.to_int32(mi) & Pervasives.max_int,
            Caml_int64.to_int32(hi$1) & 1
          ]
        };
}

function to_int64_us(param) {
  return Pervasives.failwith("todo");
}

function of_int64_s(param) {
  return Pervasives.failwith("todo");
}

function to_int64_s(param) {
  return Pervasives.failwith("todo");
}

export {
  create ,
  init ,
  set ,
  get ,
  length ,
  max_length ,
  copy ,
  append ,
  concat ,
  sub ,
  fill ,
  blit ,
  iter ,
  map ,
  iteri ,
  mapi ,
  fold_left ,
  fold_right ,
  foldi_left ,
  foldi_right ,
  gray_iter ,
  bw_and ,
  bw_or ,
  bw_xor ,
  bw_not ,
  shiftl ,
  shiftr ,
  all_zeros ,
  all_ones ,
  to_string ,
  of_string ,
  print ,
  to_list ,
  of_list ,
  of_list_with_length ,
  of_int_s ,
  to_int_s ,
  of_int_us ,
  to_int_us ,
  of_int32_s ,
  to_int32_s ,
  of_int32_us ,
  to_int32_us ,
  of_int64_s ,
  to_int64_s ,
  of_int64_us ,
  to_int64_us ,
  unsafe_set ,
  unsafe_get ,
  
}
/* bit_j Not a pure module */
