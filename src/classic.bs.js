// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Sys from "rescript/lib/es6/sys.js";
import * as $$Array from "rescript/lib/es6/array.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Builder$Graph from "./builder.bs.js";

function P(funarg) {
  var B = Builder$Graph.P(funarg);
  var divisors = function (n) {
    if (n < 2) {
      Pervasives.invalid_arg("divisors");
    }
    var v = $$Array.init(n + 1 | 0, (function (i) {
            return Curry._1(B.G.V.create, i);
          }));
    var _g = Curry._1(B.empty, undefined);
    var _i = 2;
    while(true) {
      var i = _i;
      var g = _g;
      var sqrt_i = Math.sqrt(i) | 0;
      var loop_i = (function(i,sqrt_i){
      return function loop_i(_g, _d) {
        while(true) {
          var d = _d;
          var g = _g;
          if (d > sqrt_i) {
            return g;
          }
          if (Caml_int32.mod_(i, d) === 0) {
            _d = d + 1 | 0;
            _g = Curry._3(B.add_edge, Curry._3(B.add_edge, g, Caml_array.get(v, Caml_int32.div(i, d)), Caml_array.get(v, i)), Caml_array.get(v, d), Caml_array.get(v, i));
            continue ;
          }
          _d = d + 1 | 0;
          continue ;
        };
      }
      }(i,sqrt_i));
      if (i > n) {
        return g;
      }
      _i = i + 1 | 0;
      _g = loop_i(Curry._2(B.add_vertex, g, Caml_array.get(v, i)), 2);
      continue ;
    };
  };
  var fold_for = function (i0, i1, f) {
    return function (param) {
      var _i = i0;
      var _v = param;
      while(true) {
        var v = _v;
        var i = _i;
        if (i > i1) {
          return v;
        }
        _v = Curry._2(f, v, i);
        _i = i + 1 | 0;
        continue ;
      };
    };
  };
  var de_bruijn = function (n) {
    if (n < 1 || n > (Sys.word_size - 1 | 0)) {
      Pervasives.invalid_arg("de_bruijn");
    }
    var v = $$Array.init((1 << n), (function (i) {
            return Curry._1(B.G.V.create, i);
          }));
    var all_1 = (1 << n) - 1 | 0;
    var g = fold_for(0, all_1, (function (g, i) {
              return Curry._2(B.add_vertex, g, Caml_array.get(v, i));
            }))(Curry._1(B.empty, undefined));
    var _g = g;
    var _i = 0;
    while(true) {
      var i = _i;
      var g$1 = _g;
      if (i > all_1) {
        return g$1;
      }
      var si = (i << 1) & all_1;
      var g$2 = Curry._3(B.add_edge, g$1, Caml_array.get(v, i), Caml_array.get(v, si));
      var g$3 = Curry._3(B.add_edge, g$2, Caml_array.get(v, i), Caml_array.get(v, si | 1));
      _i = i + 1 | 0;
      _g = g$3;
      continue ;
    };
  };
  var vertex_only = function (n) {
    return fold_for(1, n, (function (g, i) {
                    return Curry._2(B.add_vertex, g, Curry._1(B.G.V.create, i));
                  }))(Curry._1(B.empty, undefined));
  };
  var full = function (selfOpt, n) {
    var self = selfOpt !== undefined ? selfOpt : true;
    var v = $$Array.init(n + 1 | 0, (function (i) {
            return Curry._1(B.G.V.create, i);
          }));
    return fold_for(1, n, (function (g, i) {
                    return fold_for(1, n, (function (g, j) {
                                    if (self || i !== j) {
                                      return Curry._3(B.add_edge, g, Caml_array.get(v, i), Caml_array.get(v, j));
                                    } else {
                                      return g;
                                    }
                                  }))(g);
                  }))(fold_for(1, n, (function (g, i) {
                        return Curry._2(B.add_vertex, g, Caml_array.get(v, i));
                      }))(Curry._1(B.empty, undefined)));
  };
  var cycle = function (n) {
    if (n < 0) {
      Pervasives.invalid_arg("cycle");
    }
    var v = $$Array.init(n, (function (i) {
            return Curry._1(B.G.V.create, i);
          }));
    var g = $$Array.fold_left(B.add_vertex, Curry._1(B.empty, undefined), v);
    var loop = function (_g, _i) {
      while(true) {
        var i = _i;
        var g = _g;
        if (i === n) {
          return g;
        }
        var g$1 = Curry._3(B.add_edge, g, Caml_array.get(v, i), Caml_array.get(v, Caml_int32.mod_(i + 1 | 0, n)));
        _i = i + 1 | 0;
        _g = g$1;
        continue ;
      };
    };
    return [
            loop(g, 0),
            v
          ];
  };
  var grid = function (n, m) {
    if (n < 0 || m < 0) {
      Pervasives.invalid_arg("grid");
    }
    var v = $$Array.init(n, (function (i) {
            return $$Array.init(m, (function (j) {
                          return Curry._1(B.G.V.create, Math.imul(m, i) + j | 0);
                        }));
          }));
    var g = $$Array.fold_left((function (param, param$1) {
            return $$Array.fold_left(B.add_vertex, param, param$1);
          }), Curry._1(B.empty, undefined), v);
    var loop = function (_g, _i, _j) {
      while(true) {
        var j = _j;
        var i = _i;
        var g = _g;
        if (i === n) {
          return g;
        }
        if (j === m) {
          _j = 0;
          _i = i + 1 | 0;
          continue ;
        }
        var g$1 = j < (m - 1 | 0) ? Curry._3(B.add_edge, g, Caml_array.get(Caml_array.get(v, i), j), Caml_array.get(Caml_array.get(v, i), j + 1 | 0)) : g;
        var g$2 = i < (n - 1 | 0) ? Curry._3(B.add_edge, g$1, Caml_array.get(Caml_array.get(v, i), j), Caml_array.get(Caml_array.get(v, i + 1 | 0), j)) : g$1;
        _j = j + 1 | 0;
        _g = g$2;
        continue ;
      };
    };
    return [
            loop(g, 0, 0),
            v
          ];
  };
  return {
          divisors: divisors,
          de_bruijn: de_bruijn,
          vertex_only: vertex_only,
          full: full,
          cycle: cycle,
          grid: grid
        };
}

function I(funarg) {
  var B = Builder$Graph.I(funarg);
  var divisors = function (n) {
    if (n < 2) {
      Pervasives.invalid_arg("divisors");
    }
    var v = $$Array.init(n + 1 | 0, (function (i) {
            return Curry._1(B.G.V.create, i);
          }));
    var _g = Curry._1(B.empty, undefined);
    var _i = 2;
    while(true) {
      var i = _i;
      var g = _g;
      var sqrt_i = Math.sqrt(i) | 0;
      var loop_i = (function(i,sqrt_i){
      return function loop_i(_g, _d) {
        while(true) {
          var d = _d;
          var g = _g;
          if (d > sqrt_i) {
            return g;
          }
          if (Caml_int32.mod_(i, d) === 0) {
            _d = d + 1 | 0;
            _g = Curry._3(B.add_edge, Curry._3(B.add_edge, g, Caml_array.get(v, Caml_int32.div(i, d)), Caml_array.get(v, i)), Caml_array.get(v, d), Caml_array.get(v, i));
            continue ;
          }
          _d = d + 1 | 0;
          continue ;
        };
      }
      }(i,sqrt_i));
      if (i > n) {
        return g;
      }
      _i = i + 1 | 0;
      _g = loop_i(Curry._2(B.add_vertex, g, Caml_array.get(v, i)), 2);
      continue ;
    };
  };
  var fold_for = function (i0, i1, f) {
    return function (param) {
      var _i = i0;
      var _v = param;
      while(true) {
        var v = _v;
        var i = _i;
        if (i > i1) {
          return v;
        }
        _v = Curry._2(f, v, i);
        _i = i + 1 | 0;
        continue ;
      };
    };
  };
  var de_bruijn = function (n) {
    if (n < 1 || n > (Sys.word_size - 1 | 0)) {
      Pervasives.invalid_arg("de_bruijn");
    }
    var v = $$Array.init((1 << n), (function (i) {
            return Curry._1(B.G.V.create, i);
          }));
    var all_1 = (1 << n) - 1 | 0;
    var g = fold_for(0, all_1, (function (g, i) {
              return Curry._2(B.add_vertex, g, Caml_array.get(v, i));
            }))(Curry._1(B.empty, undefined));
    var _g = g;
    var _i = 0;
    while(true) {
      var i = _i;
      var g$1 = _g;
      if (i > all_1) {
        return g$1;
      }
      var si = (i << 1) & all_1;
      var g$2 = Curry._3(B.add_edge, g$1, Caml_array.get(v, i), Caml_array.get(v, si));
      var g$3 = Curry._3(B.add_edge, g$2, Caml_array.get(v, i), Caml_array.get(v, si | 1));
      _i = i + 1 | 0;
      _g = g$3;
      continue ;
    };
  };
  var vertex_only = function (n) {
    return fold_for(1, n, (function (g, i) {
                    return Curry._2(B.add_vertex, g, Curry._1(B.G.V.create, i));
                  }))(Curry._1(B.empty, undefined));
  };
  var full = function (selfOpt, n) {
    var self = selfOpt !== undefined ? selfOpt : true;
    var v = $$Array.init(n + 1 | 0, (function (i) {
            return Curry._1(B.G.V.create, i);
          }));
    return fold_for(1, n, (function (g, i) {
                    return fold_for(1, n, (function (g, j) {
                                    if (self || i !== j) {
                                      return Curry._3(B.add_edge, g, Caml_array.get(v, i), Caml_array.get(v, j));
                                    } else {
                                      return g;
                                    }
                                  }))(g);
                  }))(fold_for(1, n, (function (g, i) {
                        return Curry._2(B.add_vertex, g, Caml_array.get(v, i));
                      }))(Curry._1(B.empty, undefined)));
  };
  var cycle = function (n) {
    if (n < 0) {
      Pervasives.invalid_arg("cycle");
    }
    var v = $$Array.init(n, (function (i) {
            return Curry._1(B.G.V.create, i);
          }));
    var g = $$Array.fold_left(B.add_vertex, Curry._1(B.empty, undefined), v);
    var loop = function (_g, _i) {
      while(true) {
        var i = _i;
        var g = _g;
        if (i === n) {
          return g;
        }
        var g$1 = Curry._3(B.add_edge, g, Caml_array.get(v, i), Caml_array.get(v, Caml_int32.mod_(i + 1 | 0, n)));
        _i = i + 1 | 0;
        _g = g$1;
        continue ;
      };
    };
    return [
            loop(g, 0),
            v
          ];
  };
  var grid = function (n, m) {
    if (n < 0 || m < 0) {
      Pervasives.invalid_arg("grid");
    }
    var v = $$Array.init(n, (function (i) {
            return $$Array.init(m, (function (j) {
                          return Curry._1(B.G.V.create, Math.imul(m, i) + j | 0);
                        }));
          }));
    var g = $$Array.fold_left((function (param, param$1) {
            return $$Array.fold_left(B.add_vertex, param, param$1);
          }), Curry._1(B.empty, undefined), v);
    var loop = function (_g, _i, _j) {
      while(true) {
        var j = _j;
        var i = _i;
        var g = _g;
        if (i === n) {
          return g;
        }
        if (j === m) {
          _j = 0;
          _i = i + 1 | 0;
          continue ;
        }
        var g$1 = j < (m - 1 | 0) ? Curry._3(B.add_edge, g, Caml_array.get(Caml_array.get(v, i), j), Caml_array.get(Caml_array.get(v, i), j + 1 | 0)) : g;
        var g$2 = i < (n - 1 | 0) ? Curry._3(B.add_edge, g$1, Caml_array.get(Caml_array.get(v, i), j), Caml_array.get(Caml_array.get(v, i + 1 | 0), j)) : g$1;
        _j = j + 1 | 0;
        _g = g$2;
        continue ;
      };
    };
    return [
            loop(g, 0, 0),
            v
          ];
  };
  return {
          divisors: divisors,
          de_bruijn: de_bruijn,
          vertex_only: vertex_only,
          full: full,
          cycle: cycle,
          grid: grid
        };
}

export {
  P ,
  I ,
}
/* No side effect */
