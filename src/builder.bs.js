// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";

function P(G) {
  var empty = function (param) {
    return G.empty;
  };
  var copy = function (g) {
    return g;
  };
  return {
          G: G,
          empty: empty,
          copy: copy,
          add_vertex: G.add_vertex,
          add_edge: G.add_edge,
          add_edge_e: G.add_edge_e,
          remove_vertex: G.remove_vertex,
          remove_edge: G.remove_edge,
          remove_edge_e: G.remove_edge_e
        };
}

function I(G) {
  var empty = function (param) {
    return Curry._2(G.create, 97, undefined);
  };
  var add_vertex = function (g, v) {
    Curry._2(G.add_vertex, g, v);
    return g;
  };
  var add_edge = function (g, v1, v2) {
    Curry._3(G.add_edge, g, v1, v2);
    return g;
  };
  var add_edge_e = function (g, e) {
    Curry._2(G.add_edge_e, g, e);
    return g;
  };
  var remove_vertex = function (g, v) {
    Curry._2(G.remove_vertex, g, v);
    return g;
  };
  var remove_edge = function (g, v1, v2) {
    Curry._3(G.remove_edge, g, v1, v2);
    return g;
  };
  var remove_edge_e = function (g, e) {
    Curry._2(G.remove_edge_e, g, e);
    return g;
  };
  return {
          G: G,
          empty: empty,
          copy: G.copy,
          add_vertex: add_vertex,
          add_edge: add_edge,
          add_edge_e: add_edge_e,
          remove_vertex: remove_vertex,
          remove_edge: remove_edge,
          remove_edge_e: remove_edge_e
        };
}

export {
  P ,
  I ,
  
}
/* No side effect */
