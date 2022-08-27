// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";

function Make(funarg) {
  var H = Hashtbl.Make(funarg);
  var init = function (l) {
    var h = Curry._1(H.create, 997);
    List.iter((function (x) {
            var cell = {};
            cell.c = 0;
            cell.data = x;
            cell.father = cell;
            Curry._3(H.add, h, x, cell);
          }), l);
    return h;
  };
  var find_aux = function (cell) {
    if (cell.father === cell) {
      return cell;
    }
    var r = find_aux(cell.father);
    cell.father = r;
    return r;
  };
  var find = function (x, h) {
    return find_aux(Curry._2(H.find, h, x)).data;
  };
  var union = function (x, y, h) {
    var rx = find_aux(Curry._2(H.find, h, x));
    var ry = find_aux(Curry._2(H.find, h, y));
    if (rx !== ry) {
      if (rx.c > ry.c) {
        ry.father = rx;
      } else if (rx.c < ry.c) {
        rx.father = ry;
      } else {
        rx.c = rx.c + 1 | 0;
        ry.father = rx;
      }
      return ;
    }
    
  };
  return {
          init: init,
          find: find,
          union: union
        };
}

export {
  Make ,
}
/* No side effect */
