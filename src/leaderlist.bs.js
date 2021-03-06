// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Set from "rescript/lib/es6/set.js";
import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";

function Make(funarg) {
  var S = $$Set.Make(funarg.V);
  var leader_lists = function (g, root) {
    var partition_vertices = function (f, g) {
      return Curry._3(funarg.fold_vertex, (function (n, param) {
                    var s2 = param[1];
                    var s1 = param[0];
                    if (Curry._1(f, n)) {
                      return [
                              Curry._2(S.add, n, s1),
                              s2
                            ];
                    } else {
                      return [
                              s1,
                              Curry._2(S.add, n, s2)
                            ];
                    }
                  }), g, [
                  S.empty,
                  S.empty
                ]);
    };
    var is_leader = function (n) {
      if (Curry._2(funarg.V.equal, n, root)) {
        return true;
      }
      var match = Curry._2(funarg.pred, g, n);
      if (!match) {
        return true;
      }
      if (match.tl) {
        return true;
      }
      var match$1 = Curry._2(funarg.succ, g, match.hd);
      if (match$1) {
        if (match$1.tl) {
          return true;
        } else {
          return false;
        }
      }
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "leaderlist.ml",
              57,
              14
            ],
            Error: new Error()
          };
    };
    var match = partition_vertices(is_leader, g);
    var entourage = match[1];
    var basic_block = function (x) {
      var basic_block$1 = function (_x, _bb) {
        while(true) {
          var bb = _bb;
          var x = _x;
          var match = Curry._2(funarg.succ, g, x);
          if (!match) {
            return {
                    hd: x,
                    tl: bb
                  };
          }
          var y = match.hd;
          if (!Curry._2(S.mem, y, entourage)) {
            return {
                    hd: x,
                    tl: bb
                  };
          }
          _bb = {
            hd: x,
            tl: bb
          };
          _x = y;
          continue ;
        };
      };
      return List.rev(basic_block$1(x, /* [] */0));
    };
    return List.rev(Curry._3(S.fold, (function (x, ss) {
                      return {
                              hd: basic_block(x),
                              tl: ss
                            };
                    }), match[0], /* [] */0));
  };
  return {
          leader_lists: leader_lists
        };
}

export {
  Make ,
  
}
/* No side effect */
