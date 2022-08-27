// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml from "rescript/lib/es6/caml.js";
import * as $$Array from "rescript/lib/es6/array.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Oper$Graph from "./oper.bs.js";
import * as Dominator$Graph from "./dominator.bs.js";
import * as Unionfind$Graph from "./lib/unionfind.bs.js";

function Make(funarg) {
  var $$let = funarg.V;
  var H = Hashtbl.Make({
        equal: $$let.equal,
        hash: $$let.hash
      });
  var scc = function (g) {
    var root = Curry._1(H.create, 997);
    var hashcomp = Curry._1(H.create, 997);
    var stack = {
      contents: /* [] */0
    };
    var numdfs = {
      contents: 0
    };
    var numcomp = {
      contents: 0
    };
    var pop = function (x, _l) {
      while(true) {
        var l = _l;
        if (!l) {
          return l;
        }
        var match = l.hd;
        if (match[0] <= x) {
          return l;
        }
        Curry._3(H.add, hashcomp, match[1], numcomp.contents);
        _l = l.tl;
        continue ;
      };
    };
    var cont = {
      contents: /* [] */0
    };
    var visit = function (v) {
      if (Curry._2(H.mem, root, v)) {
        return ;
      }
      numdfs.contents = numdfs.contents + 1 | 0;
      var n = numdfs.contents;
      Curry._3(H.add, root, v, n);
      cont.contents = {
        hd: {
          TAG: /* Finish */0,
          _0: v,
          _1: n
        },
        tl: cont.contents
      };
      Curry._3(funarg.iter_succ, (function (w) {
              cont.contents = {
                hd: {
                  TAG: /* Visit */1,
                  _0: v,
                  _1: w
                },
                tl: {
                  hd: {
                    TAG: /* Test */2,
                    _0: v,
                    _1: w
                  },
                  tl: cont.contents
                }
              };
            }), g, v);
    };
    var visit_and_finish = function (v) {
      visit(v);
      var _param;
      while(true) {
        var match = cont.contents;
        if (!match) {
          return ;
        }
        var action = match.hd;
        cont.contents = match.tl;
        switch (action.TAG | 0) {
          case /* Finish */0 :
              var n = action._1;
              var v$1 = action._0;
              if (Curry._2(H.find, root, v$1) === n) {
                Curry._3(H.add, hashcomp, v$1, numcomp.contents);
                var s = pop(n, stack.contents);
                stack.contents = s;
                numcomp.contents = numcomp.contents + 1 | 0;
              } else {
                stack.contents = {
                  hd: [
                    n,
                    v$1
                  ],
                  tl: stack.contents
                };
              }
              break;
          case /* Visit */1 :
              visit(action._1);
              break;
          case /* Test */2 :
              var w = action._1;
              var v$2 = action._0;
              if (!Curry._2(H.mem, hashcomp, w)) {
                Curry._3(H.replace, root, v$2, Caml.int_min(Curry._2(H.find, root, v$2), Curry._2(H.find, root, w)));
              }
              break;
          
        }
        _param = undefined;
        continue ;
      };
    };
    Curry._2(funarg.iter_vertex, visit_and_finish, g);
    return [
            numcomp.contents,
            (function (v) {
                return Curry._2(H.find, hashcomp, v);
              })
          ];
  };
  var scc_array = function (g) {
    var match = scc(g);
    var f = match[1];
    var t = Caml_array.make(match[0], /* [] */0);
    Curry._2(funarg.iter_vertex, (function (v) {
            var i = Curry._1(f, v);
            Caml_array.set(t, i, {
                  hd: v,
                  tl: Caml_array.get(t, i)
                });
          }), g);
    return t;
  };
  var scc_list = function (g) {
    var a = scc_array(g);
    return $$Array.fold_right((function (l, acc) {
                  return {
                          hd: l,
                          tl: acc
                        };
                }), a, /* [] */0);
  };
  return {
          scc: scc,
          scc_array: scc_array,
          scc_list: scc_list
        };
}

function Connectivity(funarg) {
  var MOper = Oper$Graph.Make(funarg);
  var $$let = funarg.G;
  var Choose = Oper$Graph.Choose({
        iter_vertex: $$let.iter_vertex,
        iter_edges_e: $$let.iter_edges_e
      });
  var $$let$1 = funarg.G;
  var Dom = Dominator$Graph.Make({
        V: $$let$1.V,
        pred: $$let$1.pred,
        succ: $$let$1.succ,
        fold_vertex: $$let$1.fold_vertex,
        iter_vertex: $$let$1.iter_vertex,
        iter_succ: $$let$1.iter_succ,
        nb_vertex: $$let$1.nb_vertex
      });
  var sstrong_articulation_points = function (g) {
    var s = Curry._1(Choose.choose_vertex, g);
    var V = funarg.G.V;
    var iter_vertex = function (f) {
      return Curry._1(funarg.G.iter_vertex, (function (v) {
                    if (!Curry._2(V.equal, s, v)) {
                      return Curry._1(f, v);
                    }
                    
                  }));
    };
    var iter_succ = function (f) {
      return Curry._1(funarg.G.iter_succ, (function (v) {
                    if (!Curry._2(V.equal, s, v)) {
                      return Curry._1(f, v);
                    }
                    
                  }));
    };
    var H = Hashtbl.Make({
          equal: V.equal,
          hash: V.hash
        });
    var scc = function (g) {
      var root = Curry._1(H.create, 997);
      var hashcomp = Curry._1(H.create, 997);
      var stack = {
        contents: /* [] */0
      };
      var numdfs = {
        contents: 0
      };
      var numcomp = {
        contents: 0
      };
      var pop = function (x, _l) {
        while(true) {
          var l = _l;
          if (!l) {
            return l;
          }
          var match = l.hd;
          if (match[0] <= x) {
            return l;
          }
          Curry._3(H.add, hashcomp, match[1], numcomp.contents);
          _l = l.tl;
          continue ;
        };
      };
      var cont = {
        contents: /* [] */0
      };
      var visit = function (v) {
        if (Curry._2(H.mem, root, v)) {
          return ;
        }
        numdfs.contents = numdfs.contents + 1 | 0;
        var n = numdfs.contents;
        Curry._3(H.add, root, v, n);
        cont.contents = {
          hd: {
            TAG: /* Finish */0,
            _0: v,
            _1: n
          },
          tl: cont.contents
        };
        Curry._2(iter_succ(function (w) {
                  cont.contents = {
                    hd: {
                      TAG: /* Visit */1,
                      _0: v,
                      _1: w
                    },
                    tl: {
                      hd: {
                        TAG: /* Test */2,
                        _0: v,
                        _1: w
                      },
                      tl: cont.contents
                    }
                  };
                }), g, v);
      };
      var visit_and_finish = function (v) {
        visit(v);
        var _param;
        while(true) {
          var match = cont.contents;
          if (!match) {
            return ;
          }
          var action = match.hd;
          cont.contents = match.tl;
          switch (action.TAG | 0) {
            case /* Finish */0 :
                var n = action._1;
                var v$1 = action._0;
                if (Curry._2(H.find, root, v$1) === n) {
                  Curry._3(H.add, hashcomp, v$1, numcomp.contents);
                  var s = pop(n, stack.contents);
                  stack.contents = s;
                  numcomp.contents = numcomp.contents + 1 | 0;
                } else {
                  stack.contents = {
                    hd: [
                      n,
                      v$1
                    ],
                    tl: stack.contents
                  };
                }
                break;
            case /* Visit */1 :
                visit(action._1);
                break;
            case /* Test */2 :
                var w = action._1;
                var v$2 = action._0;
                if (!Curry._2(H.mem, hashcomp, w)) {
                  Curry._3(H.replace, root, v$2, Caml.int_min(Curry._2(H.find, root, v$2), Curry._2(H.find, root, w)));
                }
                break;
            
          }
          _param = undefined;
          continue ;
        };
      };
      Curry._1(iter_vertex(visit_and_finish), g);
      return [
              numcomp.contents,
              (function (v) {
                  return Curry._2(H.find, hashcomp, v);
                })
            ];
    };
    var s_is_sap = scc(g)[0] > 1;
    var dt_s = Curry._2(Dom.idom_to_dom_tree, g, Curry._2(Dom.compute_idom, g, s));
    var d_s = Curry._2(Dom.dom_tree_to_snontrivial_dom, s, dt_s);
    var g_r = Curry._1(MOper.mirror, g);
    var dtr_s = Curry._2(Dom.idom_to_dom_tree, g_r, Curry._2(Dom.compute_idom, g_r, s));
    var dr_s = Curry._2(Dom.dom_tree_to_snontrivial_dom, s, dtr_s);
    var d = Curry._2(Dom.S.union, d_s, dr_s);
    if (s_is_sap) {
      return Curry._2(Dom.S.add, s, d);
    } else {
      return d;
    }
  };
  var strong_articulation_points = function (g) {
    return Curry._1(Dom.S.elements, sstrong_articulation_points(g));
  };
  return {
          S: Dom.S,
          strong_articulation_points: strong_articulation_points,
          sstrong_articulation_points: sstrong_articulation_points
        };
}

function BiConnectivity(funarg) {
  var Choose = Oper$Graph.Choose({
        iter_vertex: funarg.iter_vertex,
        iter_edges_e: funarg.iter_edges_e
      });
  var Dom = Dominator$Graph.Make({
        V: funarg.V,
        pred: funarg.pred,
        succ: funarg.succ,
        fold_vertex: funarg.fold_vertex,
        iter_vertex: funarg.iter_vertex,
        iter_succ: funarg.iter_succ,
        nb_vertex: funarg.nb_vertex
      });
  var RDom = Dominator$Graph.Make({
        V: funarg.V,
        pred: funarg.succ,
        succ: funarg.pred,
        fold_vertex: funarg.fold_vertex,
        iter_vertex: funarg.iter_vertex,
        iter_succ: funarg.iter_pred,
        nb_vertex: funarg.nb_vertex
      });
  var sstrong_articulation_points = function (g) {
    var s = Curry._1(Choose.choose_vertex, g);
    var V = funarg.V;
    var iter_vertex = function (f) {
      return Curry._1(funarg.iter_vertex, (function (v) {
                    if (!Curry._2(V.equal, s, v)) {
                      return Curry._1(f, v);
                    }
                    
                  }));
    };
    var iter_succ = function (f) {
      return Curry._1(funarg.iter_succ, (function (v) {
                    if (!Curry._2(V.equal, s, v)) {
                      return Curry._1(f, v);
                    }
                    
                  }));
    };
    var H = Hashtbl.Make({
          equal: V.equal,
          hash: V.hash
        });
    var scc = function (g) {
      var root = Curry._1(H.create, 997);
      var hashcomp = Curry._1(H.create, 997);
      var stack = {
        contents: /* [] */0
      };
      var numdfs = {
        contents: 0
      };
      var numcomp = {
        contents: 0
      };
      var pop = function (x, _l) {
        while(true) {
          var l = _l;
          if (!l) {
            return l;
          }
          var match = l.hd;
          if (match[0] <= x) {
            return l;
          }
          Curry._3(H.add, hashcomp, match[1], numcomp.contents);
          _l = l.tl;
          continue ;
        };
      };
      var cont = {
        contents: /* [] */0
      };
      var visit = function (v) {
        if (Curry._2(H.mem, root, v)) {
          return ;
        }
        numdfs.contents = numdfs.contents + 1 | 0;
        var n = numdfs.contents;
        Curry._3(H.add, root, v, n);
        cont.contents = {
          hd: {
            TAG: /* Finish */0,
            _0: v,
            _1: n
          },
          tl: cont.contents
        };
        Curry._2(iter_succ(function (w) {
                  cont.contents = {
                    hd: {
                      TAG: /* Visit */1,
                      _0: v,
                      _1: w
                    },
                    tl: {
                      hd: {
                        TAG: /* Test */2,
                        _0: v,
                        _1: w
                      },
                      tl: cont.contents
                    }
                  };
                }), g, v);
      };
      var visit_and_finish = function (v) {
        visit(v);
        var _param;
        while(true) {
          var match = cont.contents;
          if (!match) {
            return ;
          }
          var action = match.hd;
          cont.contents = match.tl;
          switch (action.TAG | 0) {
            case /* Finish */0 :
                var n = action._1;
                var v$1 = action._0;
                if (Curry._2(H.find, root, v$1) === n) {
                  Curry._3(H.add, hashcomp, v$1, numcomp.contents);
                  var s = pop(n, stack.contents);
                  stack.contents = s;
                  numcomp.contents = numcomp.contents + 1 | 0;
                } else {
                  stack.contents = {
                    hd: [
                      n,
                      v$1
                    ],
                    tl: stack.contents
                  };
                }
                break;
            case /* Visit */1 :
                visit(action._1);
                break;
            case /* Test */2 :
                var w = action._1;
                var v$2 = action._0;
                if (!Curry._2(H.mem, hashcomp, w)) {
                  Curry._3(H.replace, root, v$2, Caml.int_min(Curry._2(H.find, root, v$2), Curry._2(H.find, root, w)));
                }
                break;
            
          }
          _param = undefined;
          continue ;
        };
      };
      Curry._1(iter_vertex(visit_and_finish), g);
      return [
              numcomp.contents,
              (function (v) {
                  return Curry._2(H.find, hashcomp, v);
                })
            ];
    };
    var s_is_sap = scc(g)[0] > 1;
    var dt_s = Curry._2(Dom.idom_to_dom_tree, g, Curry._2(Dom.compute_idom, g, s));
    var d_s = Curry._2(Dom.dom_tree_to_snontrivial_dom, s, dt_s);
    var dtr_s = Curry._2(RDom.idom_to_dom_tree, g, Curry._2(RDom.compute_idom, g, s));
    var dr_s = Curry._2(Dom.dom_tree_to_snontrivial_dom, s, dtr_s);
    var d = Curry._2(Dom.S.union, d_s, dr_s);
    if (s_is_sap) {
      return Curry._2(Dom.S.add, s, d);
    } else {
      return d;
    }
  };
  var strong_articulation_points = function (g) {
    return Curry._1(Dom.S.elements, sstrong_articulation_points(g));
  };
  return {
          S: Dom.S,
          strong_articulation_points: strong_articulation_points,
          sstrong_articulation_points: sstrong_articulation_points
        };
}

function Undirected(funarg) {
  var $$let = funarg.V;
  var UF = Unionfind$Graph.Make({
        equal: $$let.equal,
        hash: $$let.hash,
        compare: $$let.compare
      });
  var $$let$1 = funarg.V;
  var H = Hashtbl.Make({
        equal: $$let$1.equal,
        hash: $$let$1.hash
      });
  var components = function (g) {
    var vertices = {
      contents: /* [] */0
    };
    Curry._2(funarg.iter_vertex, (function (v) {
            vertices.contents = {
              hd: v,
              tl: vertices.contents
            };
          }), g);
    var uf = Curry._1(UF.init, vertices.contents);
    var visit = function (u, v) {
      Curry._3(UF.union, u, v, uf);
    };
    Curry._2(funarg.iter_edges, visit, g);
    var count = {
      contents: 0
    };
    var comp = Curry._1(H.create, 5003);
    var visit$1 = function (v) {
      var v$1 = Curry._2(UF.find, v, uf);
      if (!Curry._2(H.mem, comp, v$1)) {
        Curry._3(H.add, comp, v$1, count.contents);
        count.contents = count.contents + 1 | 0;
        return ;
      }
      
    };
    Curry._2(funarg.iter_vertex, visit$1, g);
    return [
            count.contents,
            (function (v) {
                return Curry._2(H.find, comp, Curry._2(UF.find, v, uf));
              })
          ];
  };
  var components_array = function (g) {
    var match = components(g);
    var f = match[1];
    var t = Caml_array.make(match[0], /* [] */0);
    Curry._2(funarg.iter_vertex, (function (v) {
            var i = Curry._1(f, v);
            Caml_array.set(t, i, {
                  hd: v,
                  tl: Caml_array.get(t, i)
                });
          }), g);
    return t;
  };
  var components_list = function (g) {
    var a = components_array(g);
    return $$Array.fold_right((function (l, acc) {
                  return {
                          hd: l,
                          tl: acc
                        };
                }), a, /* [] */0);
  };
  return {
          components: components,
          components_array: components_array,
          components_list: components_list
        };
}

export {
  Make ,
  Connectivity ,
  BiConnectivity ,
  Undirected ,
}
/* No side effect */
