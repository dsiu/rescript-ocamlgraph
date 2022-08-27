// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Parsing from "rescript/lib/es6/parsing.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

function compass_pt(param) {
  if (param.TAG !== /* Ident */0) {
    return Pervasives.invalid_arg("compass_pt");
  }
  switch (param._0) {
    case "e" :
        return /* E */2;
    case "n" :
        return /* N */0;
    case "ne" :
        return /* Ne */1;
    case "nw" :
        return /* Nw */7;
    case "s" :
        return /* S */4;
    case "se" :
        return /* Se */3;
    case "sw" :
        return /* Sw */5;
    case "w" :
        return /* W */6;
    default:
      return Pervasives.invalid_arg("compass_pt");
  }
}

var yytransl_const = [
  258,
  259,
  260,
  261,
  262,
  263,
  264,
  265,
  266,
  267,
  268,
  269,
  270,
  271,
  272,
  0,
  0
];

var yytransl_block = [
  257,
  0
];

var yyact = [
  (function (param) {
      return Pervasives.failwith("parser");
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 6);
      var _2 = Parsing.peek_val(__caml_parser_env, 5);
      var _3 = Parsing.peek_val(__caml_parser_env, 4);
      var _5 = Parsing.peek_val(__caml_parser_env, 2);
      return {
              strict: _1,
              digraph: _2,
              id: _3,
              stmts: _5
            };
    }),
  (function (__caml_parser_env) {
      return false;
    }),
  (function (__caml_parser_env) {
      return true;
    }),
  (function (__caml_parser_env) {
      return false;
    }),
  (function (__caml_parser_env) {
      return true;
    }),
  (function (__caml_parser_env) {
      return /* [] */0;
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: /* [] */0
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      Parsing.peek_val(__caml_parser_env, 1);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: _3
            };
    }),
  (function (__caml_parser_env) {
      
    }),
  (function (__caml_parser_env) {
      
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Equal */5,
              _0: _1,
              _1: _3
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Subgraph */6,
              _0: _1
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Node_stmt */0,
              _0: _1,
              _1: _2
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _2 = Parsing.peek_val(__caml_parser_env, 1);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Edge_stmt */1,
              _0: _1,
              _1: _2,
              _2: _3
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Attr_graph */2,
              _0: _2
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Attr_node */3,
              _0: _2
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* Attr_edge */4,
              _0: _2
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 1);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _2,
              tl: _3
            };
    }),
  (function (__caml_parser_env) {
      return /* [] */0;
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 1);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _2,
              tl: _3
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* NodeId */0,
              _0: _1
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* NodeSub */1,
              _0: _1
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return [
              _1,
              _2
            ];
    }),
  (function (__caml_parser_env) {
      
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0);
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      try {
        return {
                TAG: /* PortC */1,
                _0: compass_pt(_2)
              };
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Invalid_argument") {
          return {
                  TAG: /* PortId */0,
                  _0: _2,
                  _1: undefined
                };
        }
        throw exn;
      }
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 2);
      var _4 = Parsing.peek_val(__caml_parser_env, 0);
      var cp;
      try {
        cp = compass_pt(_4);
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === "Invalid_argument") {
          throw {
                RE_EXN_ID: Parsing.Parse_error,
                Error: new Error()
              };
        }
        throw exn;
      }
      return {
              TAG: /* PortId */0,
              _0: _2,
              _1: cp
            };
    }),
  (function (__caml_parser_env) {
      return /* [] */0;
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0);
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 1);
      return {
              hd: _2,
              tl: /* [] */0
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 2);
      var _4 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _2,
              tl: _4
            };
    }),
  (function (__caml_parser_env) {
      
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 0);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 1);
      Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: /* [] */0
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      Parsing.peek_val(__caml_parser_env, 1);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              hd: _1,
              tl: _3
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return [
              _1,
              undefined
            ];
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return [
              _1,
              _3
            ];
    }),
  (function (__caml_parser_env) {
      
    }),
  (function (__caml_parser_env) {
      
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: /* SubgraphId */0,
              _0: _2
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 3);
      var _4 = Parsing.peek_val(__caml_parser_env, 1);
      return {
              TAG: /* SubgraphDef */1,
              _0: _2,
              _1: _4
            };
    }),
  (function (__caml_parser_env) {
      var _3 = Parsing.peek_val(__caml_parser_env, 1);
      return {
              TAG: /* SubgraphDef */1,
              _0: undefined,
              _1: _3
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 1);
      return {
              TAG: /* SubgraphDef */1,
              _0: undefined,
              _1: _2
            };
    }),
  (function (__caml_parser_env) {
      throw {
            RE_EXN_ID: Parsing.YYexit,
            _1: Parsing.peek_val(__caml_parser_env, 0),
            Error: new Error()
          };
    })
];

var yytables = {
  actions: yyact,
  transl_const: yytransl_const,
  transl_block: yytransl_block,
  lhs: "\xff\xff\x01\0\x02\0\x02\0\x03\0\x03\0\x05\0\x05\0\x06\0\x06\0\b\0\b\0\x07\0\x07\0\x07\0\x07\0\x07\0\t\0\n\0\x0b\0\x0b\0\x0b\0\x10\0\x12\0\x12\0\x0f\0\x0f\0\r\0\x13\0\x13\0\x14\0\x14\0\x0e\0\x0e\0\x11\0\x11\0\x04\0\x04\0\x15\0\x15\0\x16\0\x16\0\x17\0\x17\0\f\0\f\0\f\0\f\0\0\0",
  len: "\x02\0\x07\0\0\0\x01\0\x01\0\x01\0\0\0\x01\0\x02\0\x03\0\0\0\x01\0\x01\0\x01\0\x01\0\x03\0\x01\0\x02\0\x03\0\x02\0\x02\0\x02\0\x03\0\0\0\x03\0\x01\0\x01\0\x02\0\0\0\x01\0\x02\0\x04\0\0\0\x01\0\x03\0\x04\0\0\0\x01\0\x02\0\x03\0\x01\0\x03\0\0\0\x01\0\x02\0\x05\0\x04\0\x03\0\x02\0",
  defred: "\0\0\0\0\0\0\x03\x000\0\0\0\x04\0\x05\0\0\0%\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x07\0\0\0\f\0\r\0\x0e\0\0\0\0\0\0\0\0\0\0\0\x1b\0\x1d\0\0\0\x13\0\0\0\x14\0\x15\0\0\0\0\0\0\0\x0b\0\0\0\x11\0!\0\0\0\0\0\0\0\x0f\0\0\0\0\0\0\0/\0\0\0\0\0\x01\0\t\0\0\0\x1a\0\x19\0\0\0\x12\0\0\0\0\0\0\0+\0\0\0\0\0.\0\0\0\x16\0\x1f\0)\0#\0'\0-\0\0\0\x18\0",
  dgoto: "\x02\0\x04\0\x05\0\b\0\n\0\x12\0\x13\0\x14\0(\0\x15\0\x16\0\x17\0\x18\0\x19\0)\0\x1a\0,\0*\0D\0\x1d\0\x1e\x000\x001\0@\0",
  sindex: "\t\0\x18\xff\0\0\0\0\0\0\0\xff\0\0\0\0\x1f\xff\0\0\x1d\xff\x83\xff\x0b\xff!\xff\x83\xff!\xff!\xff3\xff(\xff\0\x000\xff\0\0\0\0\0\0\0\0!\xff2\xff9\xffC\xff\0\0\0\0E\xff\0\0>\xff\0\0\0\0F\xff\x83\xff[\0\0\0\x83\xff\0\0\0\0\x12\xff!\xffZ\xff\0\0c\xffQ\xffe\xff\0\0\x83\xff_\xff\0\0\0\0k\xff\0\0\0\0n\xff\0\0r\xffu\xff!\xff\0\0E\xffo\xff\0\0\x12\xff\0\0\0\0\0\0\0\0\0\0\0\0n\xff\0\0",
  rindex: "\0\0J\xff\0\0\0\0\0\0\0\0\0\0\0\0t\xff\0\0\0\0v\xff\x06\xff\0\0v\xff\0\0\0\0\0\0\0\0\0\0x\xff\0\0\0\0\0\x001\xff=\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0I\xffv\xff\0\0\0\0z\xff\0\0\0\0\0\0a\xff \xff\0\0\x17\xff\0\0\x16\xff\0\0v\xff\0\0\0\0\0\0\x06\xff\0\0\0\0U\xff\0\0\0\0\0\0m\xff\0\0|\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0U\xff\0\0",
  gindex: "\0\0\0\0\0\0\0\0\0\0\xf6\xffW\0\0\0\0\0\0\0\0\0\0\0\xd6\xff\xda\xff^\0\xdb\xff\0\0\xf3\xffB\0\0\0\0\0N\0\0\0\0\0",
  tablesize: 147,
  table: " \x008\0\"\0#\0!\x009\0:\0\x1c\0\x06\0\x07\0\x01\0\x1c\0\x1c\0\x1b\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\x007\0\x1c\0\x1c\0\x1c\0*\0(\x008\0(\x004\0\x0e\x009\0J\0\x03\0\t\0\x1e\0\x11\0*\0(\0\x1e\0\x1e\0\x0b\0\x1e\0A\0\x1e\0\x1e\0\x1e\0\x1f\0\x1e\0\x1e\0\x1e\0G\0\x10\0&\0$\0'\0\x10\0\x1a\0+\0\x10\0-\0\x10\0\x10\0%\0 \0\x10\0\x10\0\x10\0 \0\x19\0.\0 \0/\0 \0 \x002\0,\0 \0 \0 \0,\0,\x003\0,\0\x02\0\x02\0,\0,\0\x17\0,\0,\0,\0\x17\x005\0<\0\x17\0>\0\x17\0\x17\0\x17\0 \0\x17\0\x17\0\x17\0 \0=\0?\0 \0B\0 \0 \0\x1b\0\"\0 \0 \0 \0\"\0E\0C\0\"\0F\0\"\0\"\0\n\0I\0\"\0\"\0\"\0$\x006\0\n\0\x06\0\n\0\n\0\f\0\b\0\n\0\n\0\n\0&\0;\0\r\0K\0\x0e\0H\0\0\0\0\0\x0f\0\x10\0\x11\0",
  check: "\r\0+\0\x0f\0\x10\0\x0e\0+\0+\0\x01\x01\b\x01\t\x01\x01\0\x05\x01\x06\x01\x02\x01\b\x01\x04\x01\n\x01\x0b\x01\f\x01\x01\x01\x0e\x01\x0f\x01\x10\x01\x01\x01\x01\x01C\0\x03\x01%\0\n\x01C\0C\0\x07\x01\x01\x01\x01\x01\x10\x01\r\x01\r\x01\x05\x01\x06\x01\n\x01\b\x013\0\n\x01\x0b\x01\f\x01\f\x01\x0e\x01\x0f\x01\x10\x01>\0\x01\x01\x0b\x01\x01\x01\x05\x01\x05\x01\x06\x01\x06\x01\b\x01\x01\x01\n\x01\x0b\x01\n\x01\x01\x01\x0e\x01\x0f\x01\x10\x01\x05\x01\x06\x01\x01\x01\b\x01\x01\x01\n\x01\x0b\x01\x0b\x01\x01\x01\x0e\x01\x0f\x01\x10\x01\x05\x01\x06\x01\n\x01\b\x01\b\x01\t\x01\x0b\x01\f\x01\x01\x01\x0e\x01\x0f\x01\x10\x01\x05\x01\0\0\x02\x01\b\x01\r\x01\n\x01\x0b\x01\f\x01\x01\x01\x0e\x01\x0f\x01\x10\x01\x05\x01\x04\x01\x03\x01\b\x01\x0b\x01\n\x01\x0b\x01\x02\x01\x01\x01\x0e\x01\x0f\x01\x10\x01\x05\x01\x01\x01\x06\x01\b\x01\x01\x01\n\x01\x0b\x01\x01\x01\x0b\x01\x0e\x01\x0f\x01\x10\x01\n\x01(\0\b\x01\x0b\x01\n\x01\x0b\x01\x01\x01\x0b\x01\x0e\x01\x0f\x01\x10\x01\r\x01,\0\b\x01J\0\n\x01@\0\xff\xff\xff\xff\x0e\x01\x0f\x01\x10\x01",
  error_function: Parsing.parse_error,
  names_const: "COLON\0COMMA\0EQUAL\0SEMICOLON\0EDGEOP\0STRICT\0GRAPH\0DIGRAPH\0LBRA\0RBRA\0LSQ\0RSQ\0NODE\0EDGE\0SUBGRAPH\0EOF\0",
  names_block: "ID\0"
};

function file(lexfun, lexbuf) {
  return Parsing.yyparse(yytables, 1, lexfun, lexbuf);
}

export {
  file ,
}
/* No side effect */
