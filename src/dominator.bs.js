// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Set from "rescript/lib/es6/set.js";
import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Stack from "rescript/lib/es6/stack.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as CamlinternalLazy from "rescript/lib/es6/camlinternalLazy.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

var Unreachable = /* @__PURE__ */Caml_exceptions.create("Dominator-Graph.Unreachable");

function Make(funarg) {
  var $$let = funarg.V;
  var H = Hashtbl.Make({
        equal: $$let.equal,
        hash: $$let.hash
      });
  var S = $$Set.Make(funarg.V);
  var compute_idom = function (cfg, s0) {
    var size = Curry._1(funarg.nb_vertex, cfg);
    var bucket = Curry._1(H.create, size);
    var dfnum_h = Curry._1(H.create, size);
    var parent = Curry._1(H.create, size);
    var semi_h = Curry._1(H.create, size);
    var ancestor = Curry._1(H.create, size);
    var best = Curry._1(H.create, size);
    var samedom = Curry._1(H.create, size);
    var idom = Curry._1(H.create, size);
    var vertex = Caml_array.make(size, s0);
    var nn = {
      contents: 0
    };
    var dfnum = function (x) {
      try {
        return Curry._2(H.find, dfnum_h, x);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          throw {
                RE_EXN_ID: Unreachable,
                Error: new Error()
              };
        }
        throw exn;
      }
    };
    var semi = Curry._1(H.find, semi_h);
    var dfs = function (n0) {
      var stack = Stack.create(undefined);
      Stack.push([
            n0,
            undefined
          ], stack);
      while(!Stack.is_empty(stack)) {
        var match = Stack.pop(stack);
        var p = match[1];
        var n = match[0];
        if (!Curry._2(H.mem, dfnum_h, n)) {
          var enn = nn.contents;
          Curry._3(H.add, dfnum_h, n, enn);
          Caml_array.set(vertex, enn, n);
          if (p !== undefined) {
            Curry._3(H.add, parent, n, Caml_option.valFromOption(p));
          }
          nn.contents = enn + 1 | 0;
          Curry._3(funarg.iter_succ, (function(n){
              return function (m) {
                if (!Curry._2(H.mem, dfnum_h, m)) {
                  return Stack.push([
                              m,
                              Caml_option.some(n)
                            ], stack);
                }
                
              }
              }(n)), cfg, n);
        }
        
      };
      
    };
    var ancestor_with_lowest_semi = function (v) {
      try {
        var a = Curry._2(H.find, ancestor, v);
        var b = ancestor_with_lowest_semi(a);
        Curry._3(H.replace, ancestor, v, Curry._2(H.find, ancestor, a));
        var best_v = Curry._2(H.find, best, v);
        if (dfnum(Curry._1(semi, b)) < dfnum(Curry._1(semi, best_v))) {
          Curry._3(H.replace, best, v, b);
          return b;
        } else {
          return best_v;
        }
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          return Curry._2(H.find, best, v);
        }
        throw exn;
      }
    };
    var link = function (p, n) {
      Curry._3(H.replace, ancestor, n, p);
      return Curry._3(H.replace, best, n, n);
    };
    var semidominator = function (n) {
      var s = Curry._2(H.find, parent, n);
      return List.fold_left((function (s, v) {
                    try {
                      var s$p = dfnum(v) <= dfnum(n) ? v : Curry._1(semi, ancestor_with_lowest_semi(v));
                      if (dfnum(s$p) < dfnum(s)) {
                        return s$p;
                      } else {
                        return s;
                      }
                    }
                    catch (raw_exn){
                      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                      if (exn.RE_EXN_ID === Unreachable) {
                        return s;
                      }
                      throw exn;
                    }
                  }), s, Curry._2(funarg.pred, cfg, n));
    };
    dfs(s0);
    var lastn = nn.contents - 1 | 0;
    while(nn.contents = nn.contents - 1 | 0, nn.contents > 0) {
      var i = nn.contents;
      var n = Caml_array.get(vertex, i);
      var p = Curry._2(H.find, parent, n);
      var s = semidominator(n);
      Curry._3(H.add, semi_h, n, s);
      Curry._3(H.add, bucket, s, n);
      link(p, n);
      List.iter((function(p){
          return function (v) {
            var y = ancestor_with_lowest_semi(v);
            if (Curry._2(funarg.V.equal, Curry._1(semi, y), Curry._1(semi, v))) {
              Curry._3(H.add, idom, v, p);
            } else {
              Curry._3(H.add, samedom, v, y);
            }
            return Curry._2(H.remove, bucket, p);
          }
          }(p)), Curry._2(H.find_all, bucket, p));
    };
    for(var i$1 = 1; i$1 <= lastn; ++i$1){
      var n$1 = Caml_array.get(vertex, i$1);
      try {
        Curry._3(H.add, idom, n$1, Curry._2(H.find, idom, Curry._2(H.find, samedom, n$1)));
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID !== "Not_found") {
          throw exn;
        }
        
      }
    }
    return Curry._1(H.find, idom);
  };
  var dominators_to_dom = function (dominators, x, y) {
    return Curry._2(S.mem, x, Curry._1(dominators, y));
  };
  var dominators_to_sdom = function (dominators, x, y) {
    if (Curry._2(funarg.V.equal, x, y)) {
      return false;
    } else {
      return Curry._2(S.mem, x, Curry._1(dominators, y));
    }
  };
  var dom_to_sdom = function (dom, x, y) {
    if (Curry._2(funarg.V.equal, x, y)) {
      return false;
    } else {
      return Curry._2(dom, x, y);
    }
  };
  var dominators_to_sdominators = function (dominators, x) {
    return Curry._2(S.remove, x, Curry._1(dominators, x));
  };
  var dominators_to_idoms = function (dominators) {
    return function (x, y) {
      if (!dominators_to_sdom(dominators, x, y)) {
        return false;
      }
      var sdoms = Curry._2(S.remove, y, Curry._1(dominators, y));
      return Curry._2(S.for_all, (function (w) {
                    if (Curry._2(funarg.V.equal, x, w)) {
                      return true;
                    } else {
                      return !dominators_to_sdom(dominators, x, w);
                    }
                  }), sdoms);
    };
  };
  var dominators_to_dom_tree = function (cfg, predOpt, dominators) {
    var pred = predOpt !== undefined ? predOpt : funarg.pred;
    var idoms = dominators_to_idoms(dominators);
    var tree = Curry._1(H.create, 97);
    Curry._2(funarg.iter_vertex, (function (y) {
            var match = Curry._2(pred, cfg, y);
            if (match && !match.tl) {
              var x = match.hd;
              if (Curry._1(S.is_empty, Curry._1(dominators, x))) {
                return ;
              } else {
                return Curry._3(H.add, tree, x, y);
              }
            }
            return Curry._2(S.iter, (function (x) {
                          if (Curry._2(idoms, x, y)) {
                            return Curry._3(H.add, tree, x, y);
                          }
                          
                        }), Curry._1(dominators, y));
          }), cfg);
    return function (x) {
      var x$1 = Curry._2(H.find_all, tree, x);
      return List.fold_left((function (set, v) {
                    return Curry._2(S.add, v, set);
                  }), S.empty, x$1);
    };
  };
  var idom_to_dom_tree = function (cfg, idom) {
    var tree = Curry._1(H.create, Curry._1(funarg.nb_vertex, cfg));
    Curry._2(funarg.iter_vertex, (function (v) {
            try {
              return Curry._3(H.add, tree, Curry._1(idom, v), v);
            }
            catch (raw_exn){
              var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
              if (exn.RE_EXN_ID === "Not_found") {
                return ;
              }
              throw exn;
            }
          }), cfg);
    return Curry._1(H.find_all, tree);
  };
  var idom_to_idoms = function (idom, x, y) {
    try {
      return Curry._2(funarg.V.equal, x, Curry._1(idom, y));
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        return false;
      }
      throw exn;
    }
  };
  var compute_dom_frontier = function (cfg, dom_tree, idom) {
    var df_cache = Curry._1(H.create, 57);
    var df_local = function (n) {
      return List.filter(function (y) {
                    return !idom_to_idoms(idom, n, y);
                  })(Curry._2(funarg.succ, cfg, n));
    };
    var df = function (n, k) {
      var r;
      try {
        r = Curry._2(H.find, df_cache, n);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          r = undefined;
        } else {
          throw exn;
        }
      }
      if (r !== undefined) {
        return Curry._1(k, r);
      }
      var s = df_local(n);
      return add_df_ups(s, n, (function (res) {
                    Curry._3(H.add, df_cache, n, res);
                    return Curry._1(k, res);
                  }), Curry._1(dom_tree, n));
    };
    var add_df_ups = function (s, n, k, param) {
      if (!param) {
        return Curry._1(k, s);
      }
      var chl = param.tl;
      return df(param.hd, (function (dfc) {
                    return add_df_ups(List.fold_left((function (s, w) {
                                      if (idom_to_idoms(idom, n, w)) {
                                        return s;
                                      } else {
                                        return {
                                                hd: w,
                                                tl: s
                                              };
                                      }
                                    }), s, dfc), n, k, chl);
                  }));
    };
    return function (n) {
      return df(n, (function (x) {
                    return x;
                  }));
    };
  };
  var idom_to_dominators = function (idom, x) {
    var d = function (y, list) {
      try {
        var i = Curry._1(idom, y);
        return d(i, {
                    hd: i,
                    tl: list
                  });
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          return list;
        }
        throw exn;
      }
    };
    return d(x, /* [] */0);
  };
  var idom_to_dom = function (idom, x, y) {
    try {
      var d = Curry._1(idom, y);
      if (Curry._2(funarg.V.equal, x, d)) {
        return true;
      } else {
        return idom_to_dom(idom, x, d);
      }
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        return false;
      }
      throw exn;
    }
  };
  var dom_tree_to_nontrivial_dom = function (v, dt) {
    var _rs = /* [] */0;
    var _param = Curry._1(dt, v);
    while(true) {
      var param = _param;
      var rs = _rs;
      if (!param) {
        return rs;
      }
      var xs = param.tl;
      var x = param.hd;
      var ys = Curry._1(dt, x);
      if (ys) {
        _param = List.rev_append(ys, xs);
        _rs = {
          hd: x,
          tl: rs
        };
        continue ;
      }
      _param = xs;
      continue ;
    };
  };
  var dom_tree_to_snontrivial_dom = function (v, dt) {
    var _rs = S.empty;
    var _param = Curry._1(dt, v);
    while(true) {
      var param = _param;
      var rs = _rs;
      if (!param) {
        return rs;
      }
      var xs = param.tl;
      var x = param.hd;
      var ys = Curry._1(dt, x);
      if (ys) {
        _param = List.rev_append(ys, xs);
        _rs = Curry._2(S.add, x, rs);
        continue ;
      }
      _param = xs;
      continue ;
    };
  };
  return {
          S: S,
          compute_idom: compute_idom,
          dominators_to_dom: dominators_to_dom,
          dominators_to_sdom: dominators_to_sdom,
          dom_to_sdom: dom_to_sdom,
          dominators_to_sdominators: dominators_to_sdominators,
          dominators_to_idoms: dominators_to_idoms,
          dominators_to_dom_tree: dominators_to_dom_tree,
          idom_to_dom_tree: idom_to_dom_tree,
          idom_to_idoms: idom_to_idoms,
          compute_dom_frontier: compute_dom_frontier,
          idom_to_dominators: idom_to_dominators,
          idom_to_dom: idom_to_dom,
          dom_tree_to_nontrivial_dom: dom_tree_to_nontrivial_dom,
          dom_tree_to_snontrivial_dom: dom_tree_to_snontrivial_dom
        };
}

function Make_graph(funarg) {
  var $$let = funarg.V;
  var H = Hashtbl.Make({
        equal: $$let.equal,
        hash: $$let.hash
      });
  var S = $$Set.Make(funarg.V);
  var compute_idom = function (cfg, s0) {
    var size = Curry._1(funarg.nb_vertex, cfg);
    var bucket = Curry._1(H.create, size);
    var dfnum_h = Curry._1(H.create, size);
    var parent = Curry._1(H.create, size);
    var semi_h = Curry._1(H.create, size);
    var ancestor = Curry._1(H.create, size);
    var best = Curry._1(H.create, size);
    var samedom = Curry._1(H.create, size);
    var idom = Curry._1(H.create, size);
    var vertex = Caml_array.make(size, s0);
    var nn = {
      contents: 0
    };
    var dfnum = function (x) {
      try {
        return Curry._2(H.find, dfnum_h, x);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          throw {
                RE_EXN_ID: Unreachable,
                Error: new Error()
              };
        }
        throw exn;
      }
    };
    var semi = Curry._1(H.find, semi_h);
    var dfs = function (n0) {
      var stack = Stack.create(undefined);
      Stack.push([
            n0,
            undefined
          ], stack);
      while(!Stack.is_empty(stack)) {
        var match = Stack.pop(stack);
        var p = match[1];
        var n = match[0];
        if (!Curry._2(H.mem, dfnum_h, n)) {
          var enn = nn.contents;
          Curry._3(H.add, dfnum_h, n, enn);
          Caml_array.set(vertex, enn, n);
          if (p !== undefined) {
            Curry._3(H.add, parent, n, Caml_option.valFromOption(p));
          }
          nn.contents = enn + 1 | 0;
          Curry._3(funarg.iter_succ, (function(n){
              return function (m) {
                if (!Curry._2(H.mem, dfnum_h, m)) {
                  return Stack.push([
                              m,
                              Caml_option.some(n)
                            ], stack);
                }
                
              }
              }(n)), cfg, n);
        }
        
      };
      
    };
    var ancestor_with_lowest_semi = function (v) {
      try {
        var a = Curry._2(H.find, ancestor, v);
        var b = ancestor_with_lowest_semi(a);
        Curry._3(H.replace, ancestor, v, Curry._2(H.find, ancestor, a));
        var best_v = Curry._2(H.find, best, v);
        if (dfnum(Curry._1(semi, b)) < dfnum(Curry._1(semi, best_v))) {
          Curry._3(H.replace, best, v, b);
          return b;
        } else {
          return best_v;
        }
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          return Curry._2(H.find, best, v);
        }
        throw exn;
      }
    };
    var link = function (p, n) {
      Curry._3(H.replace, ancestor, n, p);
      return Curry._3(H.replace, best, n, n);
    };
    var semidominator = function (n) {
      var s = Curry._2(H.find, parent, n);
      return List.fold_left((function (s, v) {
                    try {
                      var s$p = dfnum(v) <= dfnum(n) ? v : Curry._1(semi, ancestor_with_lowest_semi(v));
                      if (dfnum(s$p) < dfnum(s)) {
                        return s$p;
                      } else {
                        return s;
                      }
                    }
                    catch (raw_exn){
                      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                      if (exn.RE_EXN_ID === Unreachable) {
                        return s;
                      }
                      throw exn;
                    }
                  }), s, Curry._2(funarg.pred, cfg, n));
    };
    dfs(s0);
    var lastn = nn.contents - 1 | 0;
    while(nn.contents = nn.contents - 1 | 0, nn.contents > 0) {
      var i = nn.contents;
      var n = Caml_array.get(vertex, i);
      var p = Curry._2(H.find, parent, n);
      var s = semidominator(n);
      Curry._3(H.add, semi_h, n, s);
      Curry._3(H.add, bucket, s, n);
      link(p, n);
      List.iter((function(p){
          return function (v) {
            var y = ancestor_with_lowest_semi(v);
            if (Curry._2(funarg.V.equal, Curry._1(semi, y), Curry._1(semi, v))) {
              Curry._3(H.add, idom, v, p);
            } else {
              Curry._3(H.add, samedom, v, y);
            }
            return Curry._2(H.remove, bucket, p);
          }
          }(p)), Curry._2(H.find_all, bucket, p));
    };
    for(var i$1 = 1; i$1 <= lastn; ++i$1){
      var n$1 = Caml_array.get(vertex, i$1);
      try {
        Curry._3(H.add, idom, n$1, Curry._2(H.find, idom, Curry._2(H.find, samedom, n$1)));
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID !== "Not_found") {
          throw exn;
        }
        
      }
    }
    return Curry._1(H.find, idom);
  };
  var dominators_to_dom = function (dominators, x, y) {
    return Curry._2(S.mem, x, Curry._1(dominators, y));
  };
  var dominators_to_sdom = function (dominators, x, y) {
    if (Curry._2(funarg.V.equal, x, y)) {
      return false;
    } else {
      return Curry._2(S.mem, x, Curry._1(dominators, y));
    }
  };
  var dom_to_sdom = function (dom, x, y) {
    if (Curry._2(funarg.V.equal, x, y)) {
      return false;
    } else {
      return Curry._2(dom, x, y);
    }
  };
  var dominators_to_sdominators = function (dominators, x) {
    return Curry._2(S.remove, x, Curry._1(dominators, x));
  };
  var dominators_to_idoms = function (dominators) {
    return function (x, y) {
      if (!dominators_to_sdom(dominators, x, y)) {
        return false;
      }
      var sdoms = Curry._2(S.remove, y, Curry._1(dominators, y));
      return Curry._2(S.for_all, (function (w) {
                    if (Curry._2(funarg.V.equal, x, w)) {
                      return true;
                    } else {
                      return !dominators_to_sdom(dominators, x, w);
                    }
                  }), sdoms);
    };
  };
  var dominators_to_dom_tree = function (cfg, predOpt, dominators) {
    var pred = predOpt !== undefined ? predOpt : funarg.pred;
    var idoms = dominators_to_idoms(dominators);
    var tree = Curry._1(H.create, 97);
    Curry._2(funarg.iter_vertex, (function (y) {
            var match = Curry._2(pred, cfg, y);
            if (match && !match.tl) {
              var x = match.hd;
              if (Curry._1(S.is_empty, Curry._1(dominators, x))) {
                return ;
              } else {
                return Curry._3(H.add, tree, x, y);
              }
            }
            return Curry._2(S.iter, (function (x) {
                          if (Curry._2(idoms, x, y)) {
                            return Curry._3(H.add, tree, x, y);
                          }
                          
                        }), Curry._1(dominators, y));
          }), cfg);
    return function (x) {
      var x$1 = Curry._2(H.find_all, tree, x);
      return List.fold_left((function (set, v) {
                    return Curry._2(S.add, v, set);
                  }), S.empty, x$1);
    };
  };
  var idom_to_dom_tree = function (cfg, idom) {
    var tree = Curry._1(H.create, Curry._1(funarg.nb_vertex, cfg));
    Curry._2(funarg.iter_vertex, (function (v) {
            try {
              return Curry._3(H.add, tree, Curry._1(idom, v), v);
            }
            catch (raw_exn){
              var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
              if (exn.RE_EXN_ID === "Not_found") {
                return ;
              }
              throw exn;
            }
          }), cfg);
    return Curry._1(H.find_all, tree);
  };
  var idom_to_idoms = function (idom, x, y) {
    try {
      return Curry._2(funarg.V.equal, x, Curry._1(idom, y));
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        return false;
      }
      throw exn;
    }
  };
  var compute_dom_frontier = function (cfg, dom_tree, idom) {
    var df_cache = Curry._1(H.create, 57);
    var df_local = function (n) {
      return List.filter(function (y) {
                    return !idom_to_idoms(idom, n, y);
                  })(Curry._2(funarg.succ, cfg, n));
    };
    var df = function (n, k) {
      var r;
      try {
        r = Curry._2(H.find, df_cache, n);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          r = undefined;
        } else {
          throw exn;
        }
      }
      if (r !== undefined) {
        return Curry._1(k, r);
      }
      var s = df_local(n);
      return add_df_ups(s, n, (function (res) {
                    Curry._3(H.add, df_cache, n, res);
                    return Curry._1(k, res);
                  }), Curry._1(dom_tree, n));
    };
    var add_df_ups = function (s, n, k, param) {
      if (!param) {
        return Curry._1(k, s);
      }
      var chl = param.tl;
      return df(param.hd, (function (dfc) {
                    return add_df_ups(List.fold_left((function (s, w) {
                                      if (idom_to_idoms(idom, n, w)) {
                                        return s;
                                      } else {
                                        return {
                                                hd: w,
                                                tl: s
                                              };
                                      }
                                    }), s, dfc), n, k, chl);
                  }));
    };
    return function (n) {
      return df(n, (function (x) {
                    return x;
                  }));
    };
  };
  var idom_to_dominators = function (idom, x) {
    var d = function (y, list) {
      try {
        var i = Curry._1(idom, y);
        return d(i, {
                    hd: i,
                    tl: list
                  });
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          return list;
        }
        throw exn;
      }
    };
    return d(x, /* [] */0);
  };
  var idom_to_dom = function (idom, x, y) {
    try {
      var d = Curry._1(idom, y);
      if (Curry._2(funarg.V.equal, x, d)) {
        return true;
      } else {
        return idom_to_dom(idom, x, d);
      }
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        return false;
      }
      throw exn;
    }
  };
  var dom_tree_to_nontrivial_dom = function (v, dt) {
    var _rs = /* [] */0;
    var _param = Curry._1(dt, v);
    while(true) {
      var param = _param;
      var rs = _rs;
      if (!param) {
        return rs;
      }
      var xs = param.tl;
      var x = param.hd;
      var ys = Curry._1(dt, x);
      if (ys) {
        _param = List.rev_append(ys, xs);
        _rs = {
          hd: x,
          tl: rs
        };
        continue ;
      }
      _param = xs;
      continue ;
    };
  };
  var dom_tree_to_snontrivial_dom = function (v, dt) {
    var _rs = S.empty;
    var _param = Curry._1(dt, v);
    while(true) {
      var param = _param;
      var rs = _rs;
      if (!param) {
        return rs;
      }
      var xs = param.tl;
      var x = param.hd;
      var ys = Curry._1(dt, x);
      if (ys) {
        _param = List.rev_append(ys, xs);
        _rs = Curry._2(S.add, x, rs);
        continue ;
      }
      _param = xs;
      continue ;
    };
  };
  var compute_dom_graph = function (cfg, dom_tree) {
    return Curry._3(funarg.fold_vertex, (function (p, g) {
                  try {
                    return List.fold_left((function (g, u) {
                                  return Curry._3(funarg.add_edge, g, p, u);
                                }), g, Curry._1(dom_tree, p));
                  }
                  catch (raw_exn){
                    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                    if (exn.RE_EXN_ID === "Not_found") {
                      return g;
                    }
                    throw exn;
                  }
                }), cfg, Curry._1(funarg.empty, undefined));
  };
  var compute_all = function (cfg, s0) {
    var idom = compute_idom(cfg, s0);
    var idoms = function (param, param$1) {
      return idom_to_idoms(idom, param, param$1);
    };
    var dom_tree = {
      LAZY_DONE: false,
      VAL: (function () {
          return idom_to_dom_tree(cfg, idom);
        })
    };
    var dominators = function (param) {
      return idom_to_dominators(idom, param);
    };
    var dom = function (param, param$1) {
      return idom_to_dom(idom, param, param$1);
    };
    var sdom = function (param, param$1) {
      return dom_to_sdom(dom, param, param$1);
    };
    var dom_frontier = {
      LAZY_DONE: false,
      VAL: (function () {
          return compute_dom_frontier(cfg, CamlinternalLazy.force(dom_tree), idom);
        })
    };
    return {
            idom: idom,
            idoms: idoms,
            dom_tree: (function (x) {
                return Curry._1(CamlinternalLazy.force(dom_tree), x);
              }),
            dominators: dominators,
            dom: dom,
            sdom: sdom,
            dom_frontier: (function (x) {
                return Curry._1(CamlinternalLazy.force(dom_frontier), x);
              }),
            dom_graph: (function (param) {
                return compute_dom_graph(cfg, CamlinternalLazy.force(dom_tree));
              })
          };
  };
  return {
          S: S,
          compute_idom: compute_idom,
          dominators_to_dom: dominators_to_dom,
          dominators_to_sdom: dominators_to_sdom,
          dom_to_sdom: dom_to_sdom,
          dominators_to_sdominators: dominators_to_sdominators,
          dominators_to_idoms: dominators_to_idoms,
          dominators_to_dom_tree: dominators_to_dom_tree,
          idom_to_dom_tree: idom_to_dom_tree,
          idom_to_idoms: idom_to_idoms,
          compute_dom_frontier: compute_dom_frontier,
          idom_to_dominators: idom_to_dominators,
          idom_to_dom: idom_to_dom,
          dom_tree_to_nontrivial_dom: dom_tree_to_nontrivial_dom,
          dom_tree_to_snontrivial_dom: dom_tree_to_snontrivial_dom,
          compute_dom_graph: compute_dom_graph,
          compute_all: compute_all
        };
}

export {
  Unreachable ,
  Make ,
  Make_graph ,
  
}
/* No side effect */
