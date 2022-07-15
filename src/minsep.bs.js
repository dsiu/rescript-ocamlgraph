// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Set from "rescript/lib/es6/set.js";
import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Oper$RescriptOcamlgraph from "./oper.bs.js";
import * as Components$RescriptOcamlgraph from "./components.bs.js";

function P(funarg) {
  var fold_vertex = funarg.fold_vertex;
  var remove_vertex = funarg.remove_vertex;
  var CC = Components$RescriptOcamlgraph.Make({
        V: funarg.V,
        iter_vertex: funarg.iter_vertex,
        iter_succ: funarg.iter_succ
      });
  var cc = function (g, l) {
    return Curry._1(CC.scc_list, List.fold_left(remove_vertex, g, l));
  };
  var N = Oper$RescriptOcamlgraph.Neighbourhood({
        V: funarg.V,
        fold_succ: funarg.fold_succ,
        succ: funarg.succ
      });
  var Vertex_Set = N.Vertex_Set;
  var VSetset = $$Set.Make({
        compare: N.Vertex_Set.compare
      });
  var initialisation = function (g) {
    var neighbourhood = Curry._1(N.list_from_vertex, g);
    var neighbourhoods = Curry._1(N.set_from_vertices, g);
    return Curry._3(fold_vertex, (function (v, s) {
                  return List.fold_left((function (s, l) {
                                return {
                                        hd: Curry._1(neighbourhoods, l),
                                        tl: s
                                      };
                              }), s, cc(g, {
                                  hd: v,
                                  tl: Curry._1(neighbourhood, v)
                                }));
                }), g, /* [] */0);
  };
  var generation = function (g) {
    var neighbourhood = Curry._1(N.list_from_vertex, g);
    var neighbourhoods = Curry._1(N.set_from_vertices, g);
    return function (bigs) {
      var _seen = VSetset.empty;
      var _bigs = bigs;
      var _param = bigs;
      while(true) {
        var param = _param;
        var bigs$1 = _bigs;
        var seen = _seen;
        if (!param) {
          return bigs$1;
        }
        var tl = param.tl;
        var s = param.hd;
        var l = Curry._1(Vertex_Set.elements, s);
        var seen$1 = Curry._2(VSetset.add, s, seen);
        var match = Curry._3(Vertex_Set.fold, (function(bigs$1,tl,l,seen$1){
            return function (v, param) {
              var add_neighbourhoods = function (param, l) {
                var tl = param[1];
                var s = Curry._1(neighbourhoods, l);
                return [
                        {
                          hd: s,
                          tl: param[0]
                        },
                        Curry._2(VSetset.mem, s, seen$1) ? tl : ({
                              hd: s,
                              tl: tl
                            })
                      ];
              };
              return List.fold_left(add_neighbourhoods, [
                          bigs$1,
                          tl
                        ], cc(g, Pervasives.$at(l, Curry._1(neighbourhood, v))));
            }
            }(bigs$1,tl,l,seen$1)), s, [
              bigs$1,
              tl
            ]);
        _param = match[1];
        _bigs = match[0];
        _seen = seen$1;
        continue ;
      };
    };
  };
  var allminsep = function (g) {
    return generation(g)(initialisation(g));
  };
  var set_of_allminsep = function (g) {
    return List.fold_left((function (bigs, s) {
                  return Curry._2(VSetset.add, s, bigs);
                }), VSetset.empty, generation(g)(initialisation(g)));
  };
  var list_of_allminsep = function (g) {
    return List.map(Vertex_Set.elements, generation(g)(initialisation(g)));
  };
  return {
          G: funarg,
          Vertex_Set: Vertex_Set,
          VSetset: VSetset,
          allminsep: allminsep,
          list_of_allminsep: list_of_allminsep,
          set_of_allminsep: set_of_allminsep
        };
}

function I(funarg) {
  var fold_vertex = funarg.fold_vertex;
  var iter_vertex = funarg.iter_vertex;
  var iter_vertex$1 = function (f) {
    return Curry._1(iter_vertex, (function (v) {
                  if (Curry._1(funarg.Mark.get, v) === 0) {
                    return Curry._1(f, v);
                  }
                  
                }));
  };
  var CC = Components$RescriptOcamlgraph.Make({
        V: funarg.V,
        iter_vertex: iter_vertex$1,
        iter_succ: funarg.iter_succ
      });
  var cc = function (g, l) {
    Curry._1(funarg.Mark.clear, g);
    List.iter((function (v) {
            return Curry._2(funarg.Mark.set, v, 1);
          }), l);
    return Curry._1(CC.scc_list, g);
  };
  var N = Oper$RescriptOcamlgraph.Neighbourhood({
        V: funarg.V,
        fold_succ: funarg.fold_succ,
        succ: funarg.succ
      });
  var Vertex_Set = N.Vertex_Set;
  var VSetset = $$Set.Make({
        compare: N.Vertex_Set.compare
      });
  var initialisation = function (g) {
    var neighbourhood = Curry._1(N.list_from_vertex, g);
    var neighbourhoods = Curry._1(N.set_from_vertices, g);
    return Curry._3(fold_vertex, (function (v, s) {
                  return List.fold_left((function (s, l) {
                                return {
                                        hd: Curry._1(neighbourhoods, l),
                                        tl: s
                                      };
                              }), s, cc(g, {
                                  hd: v,
                                  tl: Curry._1(neighbourhood, v)
                                }));
                }), g, /* [] */0);
  };
  var generation = function (g) {
    var neighbourhood = Curry._1(N.list_from_vertex, g);
    var neighbourhoods = Curry._1(N.set_from_vertices, g);
    return function (bigs) {
      var _seen = VSetset.empty;
      var _bigs = bigs;
      var _param = bigs;
      while(true) {
        var param = _param;
        var bigs$1 = _bigs;
        var seen = _seen;
        if (!param) {
          return bigs$1;
        }
        var tl = param.tl;
        var s = param.hd;
        var l = Curry._1(Vertex_Set.elements, s);
        var seen$1 = Curry._2(VSetset.add, s, seen);
        var match = Curry._3(Vertex_Set.fold, (function(bigs$1,tl,l,seen$1){
            return function (v, param) {
              var add_neighbourhoods = function (param, l) {
                var tl = param[1];
                var s = Curry._1(neighbourhoods, l);
                return [
                        {
                          hd: s,
                          tl: param[0]
                        },
                        Curry._2(VSetset.mem, s, seen$1) ? tl : ({
                              hd: s,
                              tl: tl
                            })
                      ];
              };
              return List.fold_left(add_neighbourhoods, [
                          bigs$1,
                          tl
                        ], cc(g, Pervasives.$at(l, Curry._1(neighbourhood, v))));
            }
            }(bigs$1,tl,l,seen$1)), s, [
              bigs$1,
              tl
            ]);
        _param = match[1];
        _bigs = match[0];
        _seen = seen$1;
        continue ;
      };
    };
  };
  var allminsep = function (g) {
    return generation(g)(initialisation(g));
  };
  var set_of_allminsep = function (g) {
    return List.fold_left((function (bigs, s) {
                  return Curry._2(VSetset.add, s, bigs);
                }), VSetset.empty, generation(g)(initialisation(g)));
  };
  var list_of_allminsep = function (g) {
    return List.map(Vertex_Set.elements, generation(g)(initialisation(g)));
  };
  return {
          G: funarg,
          Vertex_Set: Vertex_Set,
          VSetset: VSetset,
          allminsep: allminsep,
          list_of_allminsep: list_of_allminsep,
          set_of_allminsep: set_of_allminsep
        };
}

export {
  P ,
  I ,
  
}
/* No side effect */
