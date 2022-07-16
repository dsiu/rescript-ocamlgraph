// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

function Make(funarg) {
  var rev = function (e) {
    return Curry._3(funarg.E.create, Curry._1(funarg.E.dst, e), Curry._1(funarg.E.label, e), Curry._1(funarg.E.src, e));
  };
  var $$let = funarg.V;
  var H = Hashtbl.Make({
        equal: $$let.equal,
        hash: $$let.hash
      });
  var add_out_edge = function (out, x, y, e) {
    var s;
    try {
      s = Curry._2(H.find, out, x);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        var s$1 = Curry._1(H.create, 4);
        Curry._3(H.add, out, x, s$1);
        s = s$1;
      } else {
        throw exn;
      }
    }
    return Curry._3(H.add, s, y, e);
  };
  var setup = function (g) {
    var nbe = {
      contents: 0
    };
    var out = Curry._1(H.create, 16);
    var add = function (e) {
      nbe.contents = nbe.contents + 1 | 0;
      var x = Curry._1(funarg.E.src, e);
      var y = Curry._1(funarg.E.dst, e);
      add_out_edge(out, x, y, e);
      if (!funarg.is_directed && !Curry._2(funarg.V.equal, x, y)) {
        return add_out_edge(out, y, x, rev(e));
      }
      
    };
    Curry._2(funarg.iter_edges_e, add, g);
    return [
            nbe.contents,
            out
          ];
  };
  var Found = /* @__PURE__ */Caml_exceptions.create("Eulerian-Graph.Make(G).Found");
  var any = function (h) {
    try {
      Curry._2(H.iter, (function (v, param) {
              throw {
                    RE_EXN_ID: Found,
                    _1: v,
                    Error: new Error()
                  };
            }), h);
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "eulerian.ml",
              79,
              47
            ],
            Error: new Error()
          };
    }
    catch (raw_v){
      var v = Caml_js_exceptions.internalToOCamlException(raw_v);
      if (v.RE_EXN_ID === Found) {
        var v$1 = v._1;
        return [
                v$1,
                Curry._2(H.find, h, v$1)
              ];
      }
      throw v;
    }
  };
  var remove_edge = function (out, e) {
    var v = Curry._1(funarg.E.src, e);
    var w = Curry._1(funarg.E.dst, e);
    var s = Curry._2(H.find, out, v);
    if (!Curry._2(H.mem, s, w)) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "eulerian.ml",
              87,
              6
            ],
            Error: new Error()
          };
    }
    Curry._2(H.remove, s, w);
    if (Curry._1(H.length, s) === 0) {
      return Curry._2(H.remove, out, v);
    }
    
  };
  var self = function (e) {
    return Curry._2(funarg.V.equal, Curry._1(funarg.E.src, e), Curry._1(funarg.E.dst, e));
  };
  var remove_edge$1 = function (edges, e) {
    remove_edge(edges, e);
    if (!funarg.is_directed && !self(e)) {
      return remove_edge(edges, rev(e));
    }
    
  };
  var any_out_edge = function (out, v) {
    if (!Curry._2(H.mem, out, v)) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "eulerian.ml",
              100,
              4
            ],
            Error: new Error()
          };
    }
    var s = Curry._2(H.find, out, v);
    if (Curry._1(H.length, s) <= 0) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "eulerian.ml",
              102,
              4
            ],
            Error: new Error()
          };
    }
    var match = any(s);
    var e = match[1];
    remove_edge$1(out, e);
    return e;
  };
  var round_trip = function (edges, start) {
    var e = any_out_edge(edges, start);
    var path = {};
    path.prev = path;
    path.edge = e;
    path.next = path;
    var _e = path;
    while(true) {
      var e$1 = _e;
      var v = Curry._1(funarg.E.dst, e$1.edge);
      if (Curry._2(funarg.V.equal, v, start)) {
        path.prev = e$1;
        return path;
      }
      var e$p = {
        prev: e$1,
        edge: any_out_edge(edges, v),
        next: path
      };
      e$1.next = e$p;
      _e = e$p;
      continue ;
    };
  };
  var connect = function (e, e$p) {
    e.next = e$p;
    e$p.prev = e;
    
  };
  var eulerian_cycle = function (out, start) {
    var todos = Curry._1(H.create, 8);
    var todo = function (e) {
      var v = Curry._1(funarg.E.src, e.edge);
      if (Curry._2(H.mem, out, v)) {
        return Curry._3(H.replace, todos, v, e);
      } else {
        return Curry._2(H.remove, todos, v);
      }
    };
    var update = function (start, _e) {
      while(true) {
        var e = _e;
        todo(e);
        if (Curry._2(funarg.V.equal, Curry._1(funarg.E.dst, e.edge), start)) {
          return ;
        }
        _e = e.next;
        continue ;
      };
    };
    var path = round_trip(out, start);
    update(start, path);
    while(Curry._1(H.length, todos) > 0) {
      var match = any(todos);
      var e = match[1];
      var v = match[0];
      Curry._2(H.remove, todos, v);
      if (!Curry._2(H.mem, out, v)) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "eulerian.ml",
                141,
                6
              ],
              Error: new Error()
            };
      }
      var e$p = round_trip(out, v);
      update(v, e$p);
      var p = e.prev;
      if (p.next !== e) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "eulerian.ml",
                145,
                6
              ],
              Error: new Error()
            };
      }
      var p$p = e$p.prev;
      if (p$p.next !== e$p) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "eulerian.ml",
                147,
                6
              ],
              Error: new Error()
            };
      }
      connect(p, e$p);
      connect(p$p, e);
    };
    return path;
  };
  var list_of = function (path) {
    var _acc = {
      hd: path.edge,
      tl: /* [] */0
    };
    var _e = path.next;
    while(true) {
      var e = _e;
      var acc = _acc;
      if (e === path) {
        return List.rev(acc);
      }
      _e = e.next;
      _acc = {
        hd: e.edge,
        tl: acc
      };
      continue ;
    };
  };
  var mem_edge = function (out, x, y) {
    try {
      return Curry._2(H.mem, Curry._2(H.find, out, x), y);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        return false;
      }
      throw exn;
    }
  };
  var out_degree = function (out, x) {
    try {
      return Curry._1(H.length, Curry._2(H.find, out, x));
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        return 0;
      }
      throw exn;
    }
  };
  var undirected = function (g) {
    var match = setup(g);
    var out = match[1];
    var odds = Curry._1(H.create, 2);
    var check = function (v, s) {
      var d = Curry._1(H.length, s);
      var d$1 = Curry._2(H.mem, s, v) ? d - 1 | 0 : d;
      if (d$1 % 2 === 1) {
        return Curry._3(H.add, odds, v, undefined);
      }
      
    };
    Curry._2(H.iter, check, out);
    var n = Curry._1(H.length, odds);
    if (n !== 0 && n !== 2) {
      Pervasives.invalid_arg("Eulerian.path (bad degrees)");
    }
    var cycle = n === 0;
    var path;
    if (cycle) {
      if (match[0] === 0) {
        path = /* [] */0;
      } else {
        var match$1 = any(out);
        path = list_of(eulerian_cycle(out, match$1[0]));
      }
    } else {
      var match$2 = any(odds);
      var x = match$2[0];
      Curry._2(H.remove, odds, x);
      var match$3 = any(odds);
      var y = match$3[0];
      if (mem_edge(out, x, y)) {
        var xy = Curry._2(H.find, Curry._2(H.find, out, x), y);
        remove_edge$1(out, xy);
        var match$4 = out_degree(out, x);
        var match$5 = out_degree(out, y);
        var exit = 0;
        if (match$4 !== 0 || match$5 !== 0) {
          exit = 1;
        } else {
          path = {
            hd: xy,
            tl: /* [] */0
          };
        }
        if (exit === 1) {
          if (match$5 !== 0) {
            if (match$4 !== 0) {
              var py = eulerian_cycle(out, y);
              path = out_degree(out, x) === 0 ? ({
                    hd: xy,
                    tl: list_of(py)
                  }) : Pervasives.$at(list_of(eulerian_cycle(out, x)), {
                      hd: xy,
                      tl: list_of(py)
                    });
            } else {
              path = {
                hd: xy,
                tl: list_of(eulerian_cycle(out, y))
              };
            }
          } else {
            path = {
              hd: rev(xy),
              tl: list_of(eulerian_cycle(out, x))
            };
          }
        }
        
      } else {
        var dummy = Curry._1(funarg.E.label, any(Curry._2(H.find, out, x))[1]);
        var xy$1 = Curry._3(funarg.E.create, x, dummy, y);
        Curry._3(H.add, Curry._2(H.find, out, x), y, xy$1);
        Curry._3(H.add, Curry._2(H.find, out, y), x, rev(xy$1));
        var p = eulerian_cycle(out, x);
        var find = function (_e) {
          while(true) {
            var e = _e;
            var v = Curry._1(funarg.E.src, e.edge);
            var w = Curry._1(funarg.E.dst, e.edge);
            if (Curry._2(funarg.V.equal, v, x) && Curry._2(funarg.V.equal, w, y) || Curry._2(funarg.V.equal, v, y) && Curry._2(funarg.V.equal, w, x)) {
              return e;
            }
            _e = e.next;
            continue ;
          };
        };
        var start = find(p);
        path = List.tl(list_of(start));
      }
    }
    if (Curry._1(H.length, out) > 0) {
      Pervasives.invalid_arg("Eulerian.path (not connected)");
    }
    return [
            path,
            cycle
          ];
  };
  var directed = function (g) {
    var delta = Curry._1(H.create, 16);
    var add = function (v, d) {
      var tmp;
      try {
        tmp = Curry._2(H.find, delta, v);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          tmp = 0;
        } else {
          throw exn;
        }
      }
      return Curry._3(H.replace, delta, v, d + tmp | 0);
    };
    var add$1 = function (e) {
      add(Curry._1(funarg.E.src, e), 1);
      return add(Curry._1(funarg.E.dst, e), -1);
    };
    Curry._2(funarg.iter_edges_e, add$1, g);
    var start = {
      contents: undefined
    };
    var finish = {
      contents: undefined
    };
    var check = function (v, param) {
      switch (param) {
        case -1 :
            if (finish.contents === undefined) {
              finish.contents = Caml_option.some(v);
              return ;
            } else {
              return Pervasives.invalid_arg("Eulerian.path (bad degrees)");
            }
        case 0 :
            return ;
        case 1 :
            if (start.contents === undefined) {
              start.contents = Caml_option.some(v);
              return ;
            } else {
              return Pervasives.invalid_arg("Eulerian.path (bad degrees)");
            }
        default:
          return Pervasives.invalid_arg("Eulerian.path (bad degrees)");
      }
    };
    Curry._2(H.iter, check, delta);
    var match = setup(g);
    var out = match[1];
    var match$1 = start.contents;
    var match$2 = finish.contents;
    var match$3;
    if (match$1 !== undefined) {
      if (match$2 !== undefined) {
        var f = Caml_option.valFromOption(match$2);
        var s = Caml_option.valFromOption(match$1);
        var dummy = Curry._1(funarg.E.label, any(Curry._2(H.find, out, s))[1]);
        var fs = Curry._3(funarg.E.create, f, dummy, s);
        add_out_edge(out, f, s, fs);
        var p = eulerian_cycle(out, s);
        var find = function (_e) {
          while(true) {
            var e = _e;
            if (e.edge === fs) {
              return e;
            }
            _e = e.next;
            continue ;
          };
        };
        var start$1 = find(p);
        match$3 = [
          List.tl(list_of(start$1)),
          false
        ];
      } else {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "eulerian.ml",
                255,
                10
              ],
              Error: new Error()
            };
      }
    } else {
      if (match$2 !== undefined) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "eulerian.ml",
                255,
                10
              ],
              Error: new Error()
            };
      }
      if (match[0] === 0) {
        match$3 = [
          /* [] */0,
          true
        ];
      } else {
        var match$4 = any(out);
        match$3 = [
          list_of(eulerian_cycle(out, match$4[0])),
          true
        ];
      }
    }
    if (Curry._1(H.length, out) > 0) {
      Pervasives.invalid_arg("Eulerian.path (not connected)");
    }
    return [
            match$3[0],
            match$3[1]
          ];
  };
  var path = funarg.is_directed ? directed : undirected;
  var cycle = function (g) {
    var match = Curry._1(path, g);
    if (!match[1]) {
      Pervasives.invalid_arg("Eulerian.cycle");
    }
    return match[0];
  };
  return {
          path: path,
          cycle: cycle
        };
}

export {
  Make ,
  
}
/* No side effect */