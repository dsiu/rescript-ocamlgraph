// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Array from "rescript/lib/es6/array.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

var EmptyHeap = /* @__PURE__ */Caml_exceptions.create("Heap-RescriptOcamlgraph.EmptyHeap");

function Imperative(funarg) {
  var create = function (n) {
    if (n <= 0) {
      Pervasives.invalid_arg("create");
    }
    return {
            size: -n | 0,
            data: []
          };
  };
  var is_empty = function (h) {
    return h.size <= 0;
  };
  var resize = function (h) {
    var n = h.size;
    if (n <= 0) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "Heap.res",
              49,
              4
            ],
            Error: new Error()
          };
    }
    var n$p = (n << 1);
    var d = h.data;
    var d$p = Caml_array.make(n$p, Caml_array.get(d, 0));
    $$Array.blit(d, 0, d$p, 0, n);
    h.data = d$p;
    
  };
  var add = function (h, x) {
    if (h.size < 0) {
      h.data = Caml_array.make(-h.size | 0, x);
      h.size = 0;
    }
    var n = h.size;
    if (n === h.data.length) {
      resize(h);
    }
    var d = h.data;
    var moveup = function (_i) {
      while(true) {
        var i = _i;
        var fi = (i - 1 | 0) / 2 | 0;
        if (!(i > 0 && Curry._2(funarg.compare, Caml_array.get(d, fi), x) < 0)) {
          return Caml_array.set(d, i, x);
        }
        Caml_array.set(d, i, Caml_array.get(d, fi));
        _i = fi;
        continue ;
      };
    };
    moveup(n);
    h.size = n + 1 | 0;
    
  };
  var maximum = function (h) {
    if (h.size <= 0) {
      throw {
            RE_EXN_ID: EmptyHeap,
            Error: new Error()
          };
    }
    return Caml_array.get(h.data, 0);
  };
  var remove = function (h) {
    if (h.size <= 0) {
      throw {
            RE_EXN_ID: EmptyHeap,
            Error: new Error()
          };
    }
    var n = h.size - 1 | 0;
    h.size = n;
    var d = h.data;
    var x = Caml_array.get(d, n);
    var _i = 0;
    while(true) {
      var i = _i;
      var j = (i << 1) + 1 | 0;
      if (j >= n) {
        return Caml_array.set(d, i, x);
      }
      var j$p = j + 1 | 0;
      var j$1 = j$p < n && Curry._2(funarg.compare, Caml_array.get(d, j$p), Caml_array.get(d, j)) > 0 ? j$p : j;
      if (Curry._2(funarg.compare, Caml_array.get(d, j$1), x) <= 0) {
        return Caml_array.set(d, i, x);
      }
      Caml_array.set(d, i, Caml_array.get(d, j$1));
      _i = j$1;
      continue ;
    };
  };
  var pop_maximum = function (h) {
    var m = maximum(h);
    remove(h);
    return m;
  };
  var iter = function (f, h) {
    var d = h.data;
    for(var i = 0 ,i_finish = h.size; i < i_finish; ++i){
      Curry._1(f, Caml_array.get(d, i));
    }
    
  };
  var fold = function (f, h, x0) {
    var n = h.size;
    var d = h.data;
    var _x = x0;
    var _i = 0;
    while(true) {
      var i = _i;
      var x = _x;
      if (i >= n) {
        return x;
      }
      _i = i + 1 | 0;
      _x = Curry._2(f, Caml_array.get(d, i), x);
      continue ;
    };
  };
  return {
          create: create,
          is_empty: is_empty,
          add: add,
          maximum: maximum,
          remove: remove,
          pop_maximum: pop_maximum,
          iter: iter,
          fold: fold
        };
}

export {
  EmptyHeap ,
  Imperative ,
  
}
/* No side effect */
