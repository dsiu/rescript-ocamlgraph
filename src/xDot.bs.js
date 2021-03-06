// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Map from "rescript/lib/es6/map.js";
import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Scanf from "rescript/lib/es6/scanf.js";
import * as Printf from "rescript/lib/es6/printf.js";
import * as $$String from "rescript/lib/es6/string.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Caml_sys from "rescript/lib/es6/caml_sys.js";
import * as Filename from "rescript/lib/es6/filename.js";
import * as Dot$Graph from "./dot.bs.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Util$Graph from "./util.bs.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Caml_string from "rescript/lib/es6/caml_string.js";
import * as XDotDraw$Graph from "./xDotDraw.bs.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";
import * as Caml_external_polyfill from "rescript/lib/es6/caml_external_polyfill.js";

function mk_node_layout(name, pos, bbox, draw, ldraw) {
  return {
          n_name: name,
          n_pos: pos,
          n_bbox: bbox,
          n_draw: draw,
          n_ldraw: ldraw
        };
}

function mk_cluster_layout(pos, bbox, draw, ldraw) {
  return {
          c_pos: pos,
          c_bbox: bbox,
          c_draw: draw,
          c_ldraw: ldraw
        };
}

function mk_edge_layout(draw, ldraw, hdraw, tdraw, hldraw, tldraw) {
  return {
          e_draw: draw,
          e_ldraw: ldraw,
          e_hdraw: hdraw,
          e_tdraw: tdraw,
          e_hldraw: hldraw,
          e_tldraw: tldraw
        };
}

var ParseError = /* @__PURE__ */Caml_exceptions.create("XDot-Graph.ParseError");

function read_pos(s) {
  return Curry._1(Scanf.sscanf(s, /* Format */{
                  _0: {
                    TAG: /* Float */8,
                    _0: /* Float_f */0,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Char_literal */12,
                      _0: /* ',' */44,
                      _1: {
                        TAG: /* Float */8,
                        _0: /* Float_f */0,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      }
                    }
                  },
                  _1: "%f,%f"
                }), (function (x, y) {
                return [
                        x,
                        -y
                      ];
              }));
}

function bounding_box(param, w, h) {
  var y = param[1];
  var x = param[0];
  var lower_left_0 = x - w;
  var lower_left_1 = y - h;
  var lower_left = [
    lower_left_0,
    lower_left_1
  ];
  var upper_right_0 = x + w;
  var upper_right_1 = y + h;
  var upper_right = [
    upper_right_0,
    upper_right_1
  ];
  return [
          lower_left,
          upper_right
        ];
}

function read_common_layout(mk_layout, attr_list) {
  var fold = function (attrs, param) {
    var match = param[0];
    var ldraw = attrs[4];
    var draw = attrs[3];
    var h = attrs[2];
    var w = attrs[1];
    var p = attrs[0];
    if (match.TAG !== /* Ident */0) {
      return attrs;
    }
    switch (match._0) {
      case "_draw_" :
          var match$1 = param[1];
          if (match$1 !== undefined && match$1.TAG === /* String */2) {
            return [
                    p,
                    w,
                    h,
                    XDotDraw$Graph.parse(match$1._0),
                    ldraw
                  ];
          } else {
            return attrs;
          }
      case "_ldraw_" :
          var match$2 = param[1];
          if (match$2 !== undefined && match$2.TAG === /* String */2) {
            return [
                    p,
                    w,
                    h,
                    draw,
                    XDotDraw$Graph.parse(match$2._0)
                  ];
          } else {
            return attrs;
          }
      case "height" :
          var match$3 = param[1];
          if (match$3 !== undefined && match$3.TAG === /* String */2) {
            return [
                    p,
                    w,
                    match$3._0,
                    draw,
                    ldraw
                  ];
          } else {
            return attrs;
          }
      case "pos" :
          var match$4 = param[1];
          if (match$4 !== undefined && match$4.TAG === /* String */2) {
            return [
                    match$4._0,
                    w,
                    h,
                    draw,
                    ldraw
                  ];
          } else {
            return attrs;
          }
      case "width" :
          var match$5 = param[1];
          if (match$5 !== undefined && match$5.TAG === /* String */2) {
            return [
                    p,
                    match$5._0,
                    h,
                    draw,
                    ldraw
                  ];
          } else {
            return attrs;
          }
      default:
        return attrs;
    }
  };
  var fold_attr = function (acc, attr_list) {
    return List.fold_left(fold, acc, attr_list);
  };
  var attrs = List.fold_left(fold_attr, [
        undefined,
        undefined,
        undefined,
        /* [] */0,
        /* [] */0
      ], attr_list);
  var pos = attrs[0];
  if (pos !== undefined) {
    var w = attrs[1];
    if (w !== undefined) {
      var h = attrs[2];
      if (h !== undefined) {
        var pos$1 = read_pos(pos);
        var coord = bounding_box(pos$1, Caml_format.caml_float_of_string(w), -Caml_format.caml_float_of_string(h));
        return Curry._4(mk_layout, pos$1, coord, attrs[3], attrs[4]);
      }
      
    }
    
  }
  return Curry._4(mk_layout, [
              0,
              0
            ], [
              [
                0,
                0
              ],
              [
                0,
                0
              ]
            ], attrs[3], attrs[4]);
}

function read_node_layout(param, attrs) {
  var f = read_common_layout((function (pos, bbox, draw, ldraw) {
          return function (param) {
            return mk_node_layout(param, pos, bbox, draw, ldraw);
          };
        }), attrs);
  return Curry._1(f, param[0]._0);
}

function read_cluster_layout(param) {
  return read_common_layout(mk_cluster_layout, param);
}

function read_edge_layout(attr_list) {
  var draw = {
    contents: /* [] */0
  };
  var ldraw = {
    contents: /* [] */0
  };
  var hdraw = {
    contents: /* [] */0
  };
  var tdraw = {
    contents: /* [] */0
  };
  var hldraw = {
    contents: /* [] */0
  };
  var tldraw = {
    contents: /* [] */0
  };
  var fill_draw_ops = function (param) {
    var match = param[0];
    if (match.TAG !== /* Ident */0) {
      return ;
    }
    switch (match._0) {
      case "_draw_" :
          var match$1 = param[1];
          if (match$1 !== undefined && match$1.TAG === /* String */2) {
            draw.contents = XDotDraw$Graph.parse(match$1._0);
            return ;
          } else {
            return ;
          }
      case "_hdraw_" :
          var match$2 = param[1];
          if (match$2 !== undefined && match$2.TAG === /* String */2) {
            hdraw.contents = XDotDraw$Graph.parse(match$2._0);
            return ;
          } else {
            return ;
          }
      case "_hldraw_" :
          var match$3 = param[1];
          if (match$3 !== undefined && match$3.TAG === /* String */2) {
            hldraw.contents = XDotDraw$Graph.parse(match$3._0);
            return ;
          } else {
            return ;
          }
      case "_ldraw_" :
          var match$4 = param[1];
          if (match$4 !== undefined && match$4.TAG === /* String */2) {
            ldraw.contents = XDotDraw$Graph.parse(match$4._0);
            return ;
          } else {
            return ;
          }
      case "_tdraw_" :
          var match$5 = param[1];
          if (match$5 !== undefined && match$5.TAG === /* String */2) {
            tdraw.contents = XDotDraw$Graph.parse(match$5._0);
            return ;
          } else {
            return ;
          }
      case "_tldraw_" :
          var match$6 = param[1];
          if (match$6 !== undefined && match$6.TAG === /* String */2) {
            tldraw.contents = XDotDraw$Graph.parse(match$6._0);
            return ;
          } else {
            return ;
          }
      default:
        return ;
    }
  };
  List.iter((function (param) {
          return List.iter(fill_draw_ops, param);
        }), attr_list);
  var draw$1 = draw.contents;
  var ldraw$1 = ldraw.contents;
  var hdraw$1 = hdraw.contents;
  var tdraw$1 = tdraw.contents;
  var hldraw$1 = hldraw.contents;
  var tldraw$1 = tldraw.contents;
  return mk_edge_layout(draw$1, ldraw$1, hdraw$1, tdraw$1, hldraw$1, tldraw$1);
}

function read_bounding_box(str) {
  var match = Curry._1(Scanf.sscanf(str, /* Format */{
            _0: {
              TAG: /* Float */8,
              _0: /* Float_f */0,
              _1: /* No_padding */0,
              _2: /* No_precision */0,
              _3: {
                TAG: /* Char_literal */12,
                _0: /* ',' */44,
                _1: {
                  TAG: /* Float */8,
                  _0: /* Float_f */0,
                  _1: /* No_padding */0,
                  _2: /* No_precision */0,
                  _3: {
                    TAG: /* Char_literal */12,
                    _0: /* ',' */44,
                    _1: {
                      TAG: /* Float */8,
                      _0: /* Float_f */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* Char_literal */12,
                        _0: /* ',' */44,
                        _1: {
                          TAG: /* Float */8,
                          _0: /* Float_f */0,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: /* End_of_format */0
                        }
                      }
                    }
                  }
                }
              }
            },
            _1: "%f,%f,%f,%f"
          }), (function (a, b, c, d) {
          return [
                  a,
                  b,
                  c,
                  d
                ];
        }));
  var lower_left_0 = match[0];
  var lower_left_1 = -match[3];
  var lower_left = [
    lower_left_0,
    lower_left_1
  ];
  var upper_right_0 = match[2];
  var upper_right_1 = -match[1];
  var upper_right = [
    upper_right_0,
    upper_right_1
  ];
  return [
          lower_left,
          upper_right
        ];
}

function Make(funarg) {
  var $$let = funarg.V;
  var HV = Hashtbl.Make({
        equal: $$let.equal,
        hash: $$let.hash
      });
  var compare = funarg.E.compare;
  var HE = $$Map.Make({
        compare: compare
      });
  var $$let$1 = funarg.V;
  var partial_arg_hash = $$let$1.hash;
  var partial_arg_equal = $$let$1.equal;
  var partial_arg = {
    hash: partial_arg_hash,
    equal: partial_arg_equal
  };
  var partial_arg$1 = Util$Graph.HTProduct;
  var $$let$2 = funarg.V;
  var partial_arg$2 = (function (param) {
        return partial_arg$1(partial_arg, param);
      })({
        hash: $$let$2.hash,
        equal: $$let$2.equal
      });
  var partial_arg$3 = Util$Graph.HTProduct;
  var equal = Caml_obj.caml_equal;
  var $$let$3 = (function (param) {
        return partial_arg$3(partial_arg$2, param);
      })({
        hash: Hashtbl.hash,
        equal: equal
      });
  var HT = Hashtbl.Make({
        equal: $$let$3.equal,
        hash: $$let$3.hash
      });
  var Found = /* @__PURE__ */Caml_exceptions.create("XDot-Graph.Make(G).Found");
  var get_edge_comment = function (e) {
    var al = Curry._1(funarg.edge_attributes, e);
    try {
      List.iter((function (param) {
              if (typeof param !== "object") {
                return ;
              }
              if (param.NAME !== "Comment") {
                return ;
              }
              throw {
                    RE_EXN_ID: Found,
                    _1: param.VAL,
                    Error: new Error()
                  };
            }), al);
      return ;
    }
    catch (raw_c){
      var c = Caml_js_exceptions.internalToOCamlException(raw_c);
      if (c.RE_EXN_ID === Found) {
        return c._1;
      }
      throw c;
    }
  };
  var get_dot_comment = function (al) {
    try {
      List.iter((function (param) {
              return List.iter((function (param) {
                            var match = param[0];
                            if (match.TAG !== /* Ident */0) {
                              return ;
                            }
                            if (match._0 !== "comment") {
                              return ;
                            }
                            var c = param[1];
                            if (c !== undefined) {
                              throw {
                                    RE_EXN_ID: Found,
                                    _1: c._0,
                                    Error: new Error()
                                  };
                            }
                            
                          }), param);
            }), al);
      return "";
    }
    catch (raw_c){
      var c = Caml_js_exceptions.internalToOCamlException(raw_c);
      if (c.RE_EXN_ID === Found) {
        return c._1;
      }
      throw c;
    }
  };
  var strip_quotes = function (s) {
    if (s === "") {
      return "";
    }
    var len = s.length;
    if (Caml_string.get(s, 0) === /* '"' */34 && Caml_string.get(s, len - 1 | 0) === /* '"' */34) {
      return $$String.sub(s, 1, len - 2 | 0);
    } else {
      return s;
    }
  };
  var parse_graph_attr = function (id, conv, stmts) {
    var read_attr = function (param) {
      var ident = param[0];
      if (ident.TAG !== /* Ident */0) {
        return ;
      }
      var match = param[1];
      if (match === undefined) {
        return ;
      }
      if (match.TAG !== /* String */2) {
        return ;
      }
      if (ident._0 === id) {
        throw {
              RE_EXN_ID: Found,
              _1: match._0,
              Error: new Error()
            };
      }
      
    };
    var read_stmt = function (attrs) {
      if (attrs.TAG === /* Attr_graph */2) {
        return List.iter((function (param) {
                      return List.iter(read_attr, param);
                    }), attrs._0);
      }
      
    };
    try {
      List.iter(read_stmt, stmts);
      return Pervasives.failwith("Could not find the graph attribute named " + id);
    }
    catch (raw_attr){
      var attr = Caml_js_exceptions.internalToOCamlException(raw_attr);
      if (attr.RE_EXN_ID === Found) {
        return Curry._1(conv, attr._1);
      }
      throw attr;
    }
  };
  var parse_layouts = function (g, stmts) {
    var name_to_vertex = Hashtbl.create(undefined, 97);
    var vertices_comment_to_edge = Curry._1(HT.create, 97);
    var vertex_layouts = Curry._1(HV.create, 97);
    var edge_layouts = {
      contents: HE.empty
    };
    var cluster_layouts = Hashtbl.create(undefined, 97);
    Curry._2(funarg.iter_vertex, (function (v) {
            var name = strip_quotes(Curry._1(funarg.vertex_name, v));
            return Hashtbl.add(name_to_vertex, name, v);
          }), g);
    Curry._2(funarg.iter_edges_e, (function (e) {
            var c = get_edge_comment(e);
            var comment = c !== undefined ? strip_quotes(c) : "";
            var vs_0 = Curry._1(funarg.E.src, e);
            var vs_1 = Curry._1(funarg.E.dst, e);
            var vs = [
              vs_0,
              vs_1
            ];
            return Curry._3(HT.add, vertices_comment_to_edge, [
                        vs,
                        comment
                      ], e);
          }), g);
    var find_vertex = function (param) {
      var name = param[0]._0;
      try {
        return Hashtbl.find(name_to_vertex, name);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          return Pervasives.failwith("Could not find vertex named " + name);
        }
        throw exn;
      }
    };
    var find_edge = function (v, v$p, comment) {
      try {
        return Curry._2(HT.find, vertices_comment_to_edge, [
                    [
                      v,
                      v$p
                    ],
                    comment
                  ]);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          throw {
                RE_EXN_ID: "Not_found",
                Error: new Error()
              };
        }
        throw exn;
      }
    };
    var collect_layouts = function (cluster, stmt) {
      try {
        switch (stmt.TAG | 0) {
          case /* Node_stmt */0 :
              var node_id = stmt._0;
              var v = find_vertex(node_id);
              return Curry._3(HV.add, vertex_layouts, v, read_node_layout(node_id, stmt._1));
          case /* Edge_stmt */1 :
              var id = stmt._0;
              if (id.TAG !== /* NodeId */0) {
                return ;
              }
              var match = stmt._1;
              if (!match) {
                return ;
              }
              var id$p = match.hd;
              if (id$p.TAG !== /* NodeId */0) {
                return ;
              }
              if (match.tl) {
                return ;
              }
              var al = stmt._2;
              var v$1 = find_vertex(id._0);
              var v$p = find_vertex(id$p._0);
              var comment = get_dot_comment(al);
              var e = find_edge(v$1, v$p, comment);
              edge_layouts.contents = Curry._3(HE.add, e, read_edge_layout(al), edge_layouts.contents);
              return ;
          case /* Attr_graph */2 :
              if (cluster !== undefined) {
                return Hashtbl.add(cluster_layouts, cluster, read_cluster_layout(stmt._0));
              } else {
                return ;
              }
          case /* Subgraph */6 :
              var match$1 = stmt._0;
              if (match$1.TAG === /* SubgraphId */0) {
                return ;
              }
              var id$1 = match$1._0;
              if (id$1 === undefined) {
                return List.iter((function (param) {
                              return collect_layouts(cluster, param);
                            }), match$1._1);
              }
              var cluster$1 = id$1._0;
              var partial_arg = cluster$1;
              return List.iter((function (param) {
                            return collect_layouts(partial_arg, param);
                          }), match$1._1);
          default:
            return ;
        }
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Not_found") {
          return ;
        }
        throw exn;
      }
    };
    List.iter((function (param) {
            return collect_layouts(undefined, param);
          }), stmts);
    return [
            vertex_layouts,
            edge_layouts,
            cluster_layouts
          ];
  };
  var DotError = /* @__PURE__ */Caml_exceptions.create("XDot-Graph.Make(G).DotError");
  var layout_of_xdot = function (xdot_file, g) {
    var dot_ast = Dot$Graph.parse_dot_ast(xdot_file);
    var match = parse_layouts(g, dot_ast.stmts);
    var bbox = parse_graph_attr("bb", read_bounding_box, dot_ast.stmts);
    return {
            vertex_layouts: match[0],
            edge_layouts: match[1].contents,
            cluster_layouts: match[2],
            bbox: bbox
          };
  };
  var layout_of_dot = function (cmdOpt, dot_file, g) {
    var cmd = cmdOpt !== undefined ? cmdOpt : "dot";
    var base_name;
    try {
      base_name = Curry._1(Filename.basename, Filename.chop_extension(dot_file));
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Invalid_argument") {
        base_name = dot_file;
      } else {
        throw exn;
      }
    }
    var xdot_file = Filename.temp_file(undefined, base_name, ".xdot");
    var dot_cmd = Curry._3(Printf.sprintf(/* Format */{
              _0: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* String_literal */11,
                  _0: " -Txdot ",
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: " > ",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: "%s -Txdot %s > %s"
            }), cmd, dot_file, xdot_file);
    var match = Caml_sys.caml_sys_system_command(dot_cmd);
    if (match !== 0) {
      Caml_external_polyfill.resolve("caml_sys_remove")(xdot_file);
      throw {
            RE_EXN_ID: DotError,
            _1: "Error during dot execution",
            Error: new Error()
          };
    }
    var l = layout_of_xdot(xdot_file, g);
    Caml_external_polyfill.resolve("caml_sys_remove")(xdot_file);
    return l;
  };
  return {
          HV: HV,
          HE: HE,
          DotError: DotError,
          layout_of_xdot: layout_of_xdot,
          layout_of_dot: layout_of_dot
        };
}

export {
  mk_node_layout ,
  mk_cluster_layout ,
  mk_edge_layout ,
  ParseError ,
  Make ,
  bounding_box ,
  read_bounding_box ,
  read_node_layout ,
  read_edge_layout ,
  read_cluster_layout ,
  
}
/* Scanf Not a pure module */
