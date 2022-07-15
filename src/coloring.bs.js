// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Stack from "rescript/lib/es6/stack.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";
import * as Traverse$RescriptOcamlgraph from "./traverse.bs.js";

var NoColoring = /* @__PURE__ */Caml_exceptions.create("Coloring-RescriptOcamlgraph.NoColoring");

function Mark(funarg) {
  var Bfs = Traverse$RescriptOcamlgraph.Bfs({
        is_directed: funarg.is_directed,
        V: funarg.V,
        iter_vertex: funarg.iter_vertex,
        fold_vertex: funarg.fold_vertex,
        iter_succ: funarg.iter_succ,
        fold_succ: funarg.fold_succ
      });
  var coloring = function (g, k) {
    if (funarg.is_directed) {
      Pervasives.invalid_arg("coloring: directed graph");
    }
    var stack = Stack.create(undefined);
    var nb_to_color = Curry._1(funarg.nb_vertex, g);
    var count = {
      contents: 1
    };
    while(count.contents > 0) {
      count.contents = 0;
      Curry._2(funarg.iter_vertex, (function (v) {
              if (Curry._1(funarg.Mark.get, v) === 0 && Curry._2(funarg.out_degree, g, v) < k) {
                count.contents = count.contents + 1 | 0;
                Curry._2(funarg.Mark.set, v, k + 1 | 0);
                return Stack.push(v, stack);
              }
              
            }), g);
      nb_to_color = nb_to_color - count.contents | 0;
    };
    var try_color = function (v, i) {
      Curry._2(funarg.Mark.set, v, i);
      return Curry._3(funarg.iter_succ, (function (w) {
                    if (Curry._1(funarg.Mark.get, w) !== i) {
                      return ;
                    }
                    throw {
                          RE_EXN_ID: NoColoring,
                          Error: new Error()
                        };
                  }), g, v);
    };
    if (nb_to_color > 0) {
      var iterate = function (_iter) {
        while(true) {
          var iter = _iter;
          var v = Curry._1(Bfs.get, iter);
          var m = Curry._1(funarg.Mark.get, v);
          if (m > 0) {
            _iter = Curry._1(Bfs.step, iter);
            continue ;
          }
          for(var i = 1; i <= k; ++i){
            try {
              try_color(v, i);
              iterate(Curry._1(Bfs.step, iter));
            }
            catch (raw_exn){
              var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
              if (exn.RE_EXN_ID !== NoColoring) {
                throw exn;
              }
              
            }
          }
          Curry._2(funarg.Mark.set, v, 0);
          throw {
                RE_EXN_ID: NoColoring,
                Error: new Error()
              };
        };
      };
      try {
        iterate(Curry._1(Bfs.start, g));
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID !== Pervasives.Exit) {
          throw exn;
        }
        
      }
    }
    return Stack.iter((function (v) {
                  try {
                    for(var i = 1; i <= k; ++i){
                      try {
                        try_color(v, i);
                        throw {
                              RE_EXN_ID: Pervasives.Exit,
                              Error: new Error()
                            };
                      }
                      catch (raw_exn){
                        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                        if (exn.RE_EXN_ID !== NoColoring) {
                          throw exn;
                        }
                        
                      }
                    }
                    throw {
                          RE_EXN_ID: NoColoring,
                          Error: new Error()
                        };
                  }
                  catch (raw_exn$1){
                    var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
                    if (exn$1.RE_EXN_ID === Pervasives.Exit) {
                      return ;
                    }
                    throw exn$1;
                  }
                }), stack);
  };
  var two_color = function (g) {
    if (funarg.is_directed) {
      Pervasives.invalid_arg("coloring: directed graph");
    }
    var erase = function (v) {
      return Curry._2(funarg.Mark.set, v, 0);
    };
    Curry._2(funarg.iter_vertex, erase, g);
    var dfs = function (c, v) {
      var cv = Curry._1(funarg.Mark.get, v);
      if (cv === 2 || cv === 1) {
        if (cv === c) {
          return ;
        }
        throw {
              RE_EXN_ID: NoColoring,
              Error: new Error()
            };
      }
      Curry._2(funarg.Mark.set, v, c);
      var partial_arg = 1 - c | 0;
      return Curry._3(funarg.iter_succ, (function (param) {
                    return dfs(partial_arg, param);
                  }), g, v);
    };
    var start = function (v) {
      var match = Curry._1(funarg.Mark.get, v);
      if (!(match === 2 || match === 1)) {
        return dfs(1, v);
      }
      
    };
    return Curry._2(funarg.iter_vertex, start, g);
  };
  return {
          coloring: coloring,
          two_color: two_color
        };
}

function Make(funarg) {
  var $$let = funarg.V;
  var H = Hashtbl.Make({
        equal: $$let.equal,
        hash: $$let.hash
      });
  var add_marks = function (param) {
    var h = Curry._1(H.create, 97);
    var get = function (v) {
      try {
        return Curry._2(H.find, h, v);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          return 0;
        }
        throw exn;
      }
    };
    var set = function (v, n) {
      return Curry._3(H.replace, h, v, n);
    };
    var Mark = {
      get: get,
      set: set
    };
    return [
            h,
            {
              is_directed: funarg.is_directed,
              nb_vertex: funarg.nb_vertex,
              V: funarg.V,
              out_degree: funarg.out_degree,
              iter_vertex: funarg.iter_vertex,
              fold_vertex: funarg.fold_vertex,
              iter_succ: funarg.iter_succ,
              fold_succ: funarg.fold_succ,
              Mark: Mark
            }
          ];
  };
  var coloring = function (g, k) {
    var match = add_marks(undefined);
    var GM = match[1];
    var Bfs = Traverse$RescriptOcamlgraph.Bfs({
          is_directed: GM.is_directed,
          V: GM.V,
          iter_vertex: GM.iter_vertex,
          fold_vertex: GM.fold_vertex,
          iter_succ: GM.iter_succ,
          fold_succ: GM.fold_succ
        });
    var coloring$1 = function (g, k) {
      if (GM.is_directed) {
        Pervasives.invalid_arg("coloring: directed graph");
      }
      var stack = Stack.create(undefined);
      var nb_to_color = Curry._1(GM.nb_vertex, g);
      var count = {
        contents: 1
      };
      while(count.contents > 0) {
        count.contents = 0;
        Curry._2(GM.iter_vertex, (function (v) {
                if (Curry._1(GM.Mark.get, v) === 0 && Curry._2(GM.out_degree, g, v) < k) {
                  count.contents = count.contents + 1 | 0;
                  Curry._2(GM.Mark.set, v, k + 1 | 0);
                  return Stack.push(v, stack);
                }
                
              }), g);
        nb_to_color = nb_to_color - count.contents | 0;
      };
      var try_color = function (v, i) {
        Curry._2(GM.Mark.set, v, i);
        return Curry._3(GM.iter_succ, (function (w) {
                      if (Curry._1(GM.Mark.get, w) !== i) {
                        return ;
                      }
                      throw {
                            RE_EXN_ID: NoColoring,
                            Error: new Error()
                          };
                    }), g, v);
      };
      if (nb_to_color > 0) {
        var iterate = function (_iter) {
          while(true) {
            var iter = _iter;
            var v = Curry._1(Bfs.get, iter);
            var m = Curry._1(GM.Mark.get, v);
            if (m > 0) {
              _iter = Curry._1(Bfs.step, iter);
              continue ;
            }
            for(var i = 1; i <= k; ++i){
              try {
                try_color(v, i);
                iterate(Curry._1(Bfs.step, iter));
              }
              catch (raw_exn){
                var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                if (exn.RE_EXN_ID !== NoColoring) {
                  throw exn;
                }
                
              }
            }
            Curry._2(GM.Mark.set, v, 0);
            throw {
                  RE_EXN_ID: NoColoring,
                  Error: new Error()
                };
          };
        };
        try {
          iterate(Curry._1(Bfs.start, g));
        }
        catch (raw_exn){
          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
          if (exn.RE_EXN_ID !== Pervasives.Exit) {
            throw exn;
          }
          
        }
      }
      return Stack.iter((function (v) {
                    try {
                      for(var i = 1; i <= k; ++i){
                        try {
                          try_color(v, i);
                          throw {
                                RE_EXN_ID: Pervasives.Exit,
                                Error: new Error()
                              };
                        }
                        catch (raw_exn){
                          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                          if (exn.RE_EXN_ID !== NoColoring) {
                            throw exn;
                          }
                          
                        }
                      }
                      throw {
                            RE_EXN_ID: NoColoring,
                            Error: new Error()
                          };
                    }
                    catch (raw_exn$1){
                      var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
                      if (exn$1.RE_EXN_ID === Pervasives.Exit) {
                        return ;
                      }
                      throw exn$1;
                    }
                  }), stack);
    };
    coloring$1(g, k);
    return match[0];
  };
  var two_color = function (g) {
    var match = add_marks(undefined);
    var GM = match[1];
    Traverse$RescriptOcamlgraph.Bfs({
          is_directed: GM.is_directed,
          V: GM.V,
          iter_vertex: GM.iter_vertex,
          fold_vertex: GM.fold_vertex,
          iter_succ: GM.iter_succ,
          fold_succ: GM.fold_succ
        });
    var two_color$1 = function (g) {
      if (GM.is_directed) {
        Pervasives.invalid_arg("coloring: directed graph");
      }
      var erase = function (v) {
        return Curry._2(GM.Mark.set, v, 0);
      };
      Curry._2(GM.iter_vertex, erase, g);
      var dfs = function (c, v) {
        var cv = Curry._1(GM.Mark.get, v);
        if (cv === 2 || cv === 1) {
          if (cv === c) {
            return ;
          }
          throw {
                RE_EXN_ID: NoColoring,
                Error: new Error()
              };
        }
        Curry._2(GM.Mark.set, v, c);
        var partial_arg = 1 - c | 0;
        return Curry._3(GM.iter_succ, (function (param) {
                      return dfs(partial_arg, param);
                    }), g, v);
      };
      var start = function (v) {
        var match = Curry._1(GM.Mark.get, v);
        if (!(match === 2 || match === 1)) {
          return dfs(1, v);
        }
        
      };
      return Curry._2(GM.iter_vertex, start, g);
    };
    two_color$1(g);
    return match[0];
  };
  return {
          H: H,
          coloring: coloring,
          two_color: two_color
        };
}

export {
  NoColoring ,
  Mark ,
  Make ,
  
}
/* No side effect */
