// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as $$Array from "rescript/lib/es6/array.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Printf from "rescript/lib/es6/printf.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";
import * as Pack$RescriptOcamlgraph from "../Pack.bs.js";

function test(name, has_cycle, spec) {
  Curry._1(Printf.printf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "Running test with name: ",
              _1: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* '\n' */10,
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "Running test with name: %s\n"
          }), name);
  var v = $$Array.init(5, Pack$RescriptOcamlgraph.Digraph.V.create);
  var g = Curry._2(Pack$RescriptOcamlgraph.Digraph.create, undefined, undefined);
  $$Array.iter(Curry._1(Pack$RescriptOcamlgraph.Digraph.add_vertex, g), v);
  var build = function (param) {
    return Curry._2(Pack$RescriptOcamlgraph.Digraph.add_edge_e, g, Curry._3(Pack$RescriptOcamlgraph.Digraph.E.create, Caml_array.get(v, param[0]), param[1], Caml_array.get(v, param[2])));
  };
  List.iter(build, spec);
  try {
    var cycle = Curry._2(Pack$RescriptOcamlgraph.Digraph.bellman_ford, g, Caml_array.get(v, 1));
    var print_edge = function (e) {
      return Curry._3(Printf.printf(/* Format */{
                      _0: {
                        TAG: /* Int */4,
                        _0: /* Int_d */0,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: {
                          TAG: /* String_literal */11,
                          _0: " --(",
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* String_literal */11,
                              _0: ")--> ",
                              _1: {
                                TAG: /* Int */4,
                                _0: /* Int_d */0,
                                _1: /* No_padding */0,
                                _2: /* No_precision */0,
                                _3: {
                                  TAG: /* Char_literal */12,
                                  _0: /* '\n' */10,
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        }
                      },
                      _1: "%d --(%d)--> %d\n"
                    }), Curry._1(Pack$RescriptOcamlgraph.Digraph.V.label, Curry._1(Pack$RescriptOcamlgraph.Digraph.E.src, e)), Curry._1(Pack$RescriptOcamlgraph.Digraph.E.label, e), Curry._1(Pack$RescriptOcamlgraph.Digraph.V.label, Curry._1(Pack$RescriptOcamlgraph.Digraph.E.dst, e)));
    };
    List.iter(print_edge, cycle);
    if (!has_cycle) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "test_bf.res",
              24,
              4
            ],
            Error: new Error()
          };
    }
    
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      Printf.printf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "No cycle found\n",
              _1: /* End_of_format */0
            },
            _1: "No cycle found\n"
          });
      if (has_cycle) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "test_bf.res",
                28,
                4
              ],
              Error: new Error()
            };
      }
      
    } else {
      throw exn;
    }
  }
  Pervasives.print_newline(undefined);
  return Pervasives.flush(Pervasives.stdout);
}

test("cycle_1", true, {
      hd: [
        0,
        -3,
        1
      ],
      tl: {
        hd: [
          1,
          1,
          2
        ],
        tl: {
          hd: [
            2,
            1,
            0
          ],
          tl: {
            hd: [
              1,
              1,
              3
            ],
            tl: {
              hd: [
                3,
                1,
                4
              ],
              tl: {
                hd: [
                  4,
                  1,
                  0
                ],
                tl: /* [] */0
              }
            }
          }
        }
      }
    });

test("cycle_2", true, {
      hd: [
        0,
        -10,
        1
      ],
      tl: {
        hd: [
          1,
          1,
          2
        ],
        tl: {
          hd: [
            2,
            1,
            0
          ],
          tl: {
            hd: [
              1,
              1,
              3
            ],
            tl: {
              hd: [
                3,
                1,
                4
              ],
              tl: {
                hd: [
                  4,
                  1,
                  0
                ],
                tl: /* [] */0
              }
            }
          }
        }
      }
    });

test("cycle_3", true, {
      hd: [
        0,
        -10,
        1
      ],
      tl: {
        hd: [
          2,
          1,
          0
        ],
        tl: {
          hd: [
            1,
            1,
            3
          ],
          tl: {
            hd: [
              3,
              1,
              4
            ],
            tl: {
              hd: [
                4,
                1,
                0
              ],
              tl: /* [] */0
            }
          }
        }
      }
    });

test("cycle_4", true, {
      hd: [
        0,
        -10,
        1
      ],
      tl: {
        hd: [
          1,
          1,
          2
        ],
        tl: {
          hd: [
            1,
            1,
            3
          ],
          tl: {
            hd: [
              3,
              1,
              4
            ],
            tl: {
              hd: [
                4,
                1,
                0
              ],
              tl: /* [] */0
            }
          }
        }
      }
    });

test("cycle_5", true, {
      hd: [
        0,
        -10,
        1
      ],
      tl: {
        hd: [
          1,
          1,
          2
        ],
        tl: {
          hd: [
            2,
            1,
            0
          ],
          tl: {
            hd: [
              3,
              1,
              4
            ],
            tl: {
              hd: [
                4,
                1,
                0
              ],
              tl: /* [] */0
            }
          }
        }
      }
    });

test("cycle_6", true, {
      hd: [
        0,
        -10,
        1
      ],
      tl: {
        hd: [
          1,
          1,
          2
        ],
        tl: {
          hd: [
            2,
            1,
            0
          ],
          tl: {
            hd: [
              1,
              1,
              3
            ],
            tl: {
              hd: [
                4,
                1,
                0
              ],
              tl: /* [] */0
            }
          }
        }
      }
    });

test("nocycle_1", false, {
      hd: [
        1,
        1,
        2
      ],
      tl: {
        hd: [
          2,
          1,
          0
        ],
        tl: {
          hd: [
            1,
            1,
            3
          ],
          tl: {
            hd: [
              3,
              1,
              4
            ],
            tl: {
              hd: [
                4,
                1,
                0
              ],
              tl: /* [] */0
            }
          }
        }
      }
    });

test("nocycle_2", false, {
      hd: [
        0,
        -10,
        1
      ],
      tl: {
        hd: [
          1,
          1,
          2
        ],
        tl: {
          hd: [
            1,
            1,
            3
          ],
          tl: {
            hd: [
              3,
              1,
              4
            ],
            tl: /* [] */0
          }
        }
      }
    });

Printf.printf(/* Format */{
      _0: {
        TAG: /* String_literal */11,
        _0: "All tests succeeded.\n",
        _1: /* End_of_format */0
      },
      _1: "All tests succeeded.\n"
    });

export {
  test ,
  
}
/*  Not a pure module */
