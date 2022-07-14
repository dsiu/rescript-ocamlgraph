/* ************************************************************************ */
/*  */
/* Ocamlgraph: a generic graph library for OCaml */
/* Copyright (C) 2004-2010 */
/* Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles */
/*  */
/* This software is free software; you can redistribute it and/or */
/* modify it under the terms of the GNU Library General Public */
/* License version 2.1, with the special exception on linking */
/* described in file LICENSE. */
/*  */
/* This software is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. */
/*  */
/* ************************************************************************ */

@@warning("-3")

open Format
@@ocaml.text(
  /* ************************************************************************* */
  " {2 Common stuff} "
)

@@ocaml.text(" Because the neato and dot engines present a lot of common points -
    in particular in the graph description language, large parts of
    the code is shared.  The [CommonAttributes] module defines
    attributes of graphs, vertices and edges that are understood by the
    two engines.  Then module [DotAttributes] and [NeatoAttributes]
    define attributes specific to dot and neato respectively. ")

@@ocaml.text(
  /* ------------------------------------------------------------------------- */
  " {3 Common types and signatures} "
)

type color = int

@ocaml.doc(" The two least significant bytes encode the transparency information;
    the six most signification are the standard RGB color ")
type color_with_transparency = int32

let color_to_color_with_transparency: color => color_with_transparency

type arrow_style = [#None | #Normal | #Onormal | #Inv | #Dot | #Odot | #Invdot | #Invodot]

@ocaml.doc(" The [ATTRIBUTES] module type defines the interface for the engines. ")
module type ATTRIBUTES = {
  @ocaml.doc(" Attributes of graphs. ")
  type graph

  @ocaml.doc(" Attributes of vertices. ")
  type vertex

  @ocaml.doc(" Attributes of edges. ")
  type edge

  @ocaml.doc(" Attributes of (optional) boxes around vertices. ")
  type subgraph = {
    @ocaml.doc(" Box name. ")
    sg_name: string,
    @ocaml.doc(" Box attributes. ")
    sg_attributes: list<vertex>,
    @ocaml.doc(" Nested subgraphs. ")
    sg_parent: option<string>,
  }
}

@@ocaml.text(
  /* ------------------------------------------------------------------------- */
  " {3 Common attributes} "
)

@ocaml.doc(" The [CommonAttributes] module defines attributes for graphs, vertices and
    edges that are available in the two engines, dot and neato. ")
module CommonAttributes: {
  @ocaml.doc(" Attributes of graphs. ")
  type graph = [
    | @ocaml.doc(" Centers the drawing on the page.  Default value is [false]. ") #Center(bool)
    | @ocaml.doc(" Sets the font color.  Default value is [black]. ") #Fontcolor(color)
    | @ocaml.doc(" Sets the font family name.  Default value is [\"Times-Roman\"]. ")
    #Fontname(string)
    | @ocaml.doc(" Sets the type size (in points).  Default value is [14]. ") #Fontsize(int)
    | @ocaml.doc(" Caption for graph drawing. ") #Label(string)
    | @ocaml.doc(" Caption for graph drawing. In HTML strings, angle brackets must occur in
        matched pairs, and newlines and other formatting whitespace characters
        are allowed. In addition, the content must be legal XML, so that the
        special XML escape sequences for \", &, <, and > may be necessary in
        order to embed these characters in attribute values or raw text.\"  ")
    #HtmlLabel(string)
    | @ocaml.doc(" Sets the page orientation.  Default value is [`Portrait]. ")
    #Orientation([#Portrait | #Landscape])
    | @ocaml.doc(" Sets the PostScript pagination unit, e.g [8.5, 11.0]. ") #Page(float, float)
    | @ocaml.doc(" Traversal order of pages.  Default value is [`TopToBottom]. ")
    #Pagedir([#TopToBottom | #LeftToRight])
    | @ocaml.doc(" Sets the bounding box of drawing (in inches). ") #Size(float, float)
    | @ocaml.doc(" Constrains  order of out-edges in a subgraph according to
          their file sequence ")
    #OrderingOut
  ]

  @ocaml.doc(" Attributes of vertices. ")
  type vertex = [
    | @ocaml.doc(" Sets the color of the border of the vertex.
        Default value is [black] ")
    #Color(color)
    | @ocaml.doc(" Sets the color of the border of the vertex with a transparency
        component. Default value is fully opaque [black] ")
    #ColorWithTransparency(color_with_transparency)
    | @ocaml.doc(" Sets the label font color.  Default value is [black]. ") #Fontcolor(color)
    | @ocaml.doc(" Sets the label font family name.  Default value is
        [\"Times-Roman\"]. ")
    #Fontname(string)
    | @ocaml.doc(" Sets the label type size (in points).  Default value is [14].
    ")
    #Fontsize(int)
    | @ocaml.doc(" Sets the minimum height.  Default value is [0.5]. ") #Height(float)
    | @ocaml.doc(" Sets the label printed in the vertex.
        The string may include escaped
        newlines [\n], [\l], or [\r] for center, left, and right justified
        lines.
        Record labels may contain recursive box lists delimited by { | }.
    ")
    #Label(string)
    | @ocaml.doc(" Like label, in html style. In HTML strings, angle brackets must occur in
        matched pairs, and newlines and other formatting whitespace characters
        are allowed. In addition, the content must be legal XML, so that the
        special XML escape sequences for \", &, <, and > may be necessary in
        order to embed these characters in attribute values or raw text.\"  ")
    #HtmlLabel(string)
    | @ocaml.doc(" Vertex rotation angle, in degrees.  Default value is [0.0]. ")
    #Orientation(float)
    | @ocaml.doc(" Width of the pen (in points) used to draw the border of the node.
        Default value is [1.0]. ")
    #Penwidth(float)
    | @ocaml.doc(" Sets  the  number  of periphery lines drawn around the polygon. ")
    #Peripheries(int)
    | @ocaml.doc(" If [true], then the polygon is made regular, i.e. symmetric about
        the x and y axis, otherwise  the polygon   takes   on   the  aspect
        ratio of the label.  Default value is [false]. ")
    #Regular(bool)
    | @ocaml.doc(" Sets the shape of the vertex.  Default value is [`Ellipse].
            [`Polygon (i, f)] draws a polygon with [n] sides and a skewing
            of [f]. ")
    #Shape([
      | #Ellipse
      | #Box
      | #Circle
      | #Doublecircle
      | #Diamond
      | #Plaintext
      | #Record
      | #Oval
      | #Egg
      | #Triangle
      | #Invtriangle
      | #Trapezium
      | #Invtrapezium
      | #House
      | #Invhouse
      | #Parallelogram
      | #Doubleoctagon
      | #Tripleoctagon
      | #Mdiamond
      | #Mcircle
      | #Msquare
      | #Star
      | #Underline
      | #Note
      | #Tab
      | #Folder
      | #Box3d
      | #Component
      | #Promoter
      | #Cds
      | #Terminator
      | #Utr
      | #Primersite
      | #Restrictionsite
      | #Fivepoverhang
      | #Threepoverhang
      | #Noverhang
      | #Assembly
      | #Signature
      | #Insulator
      | #Ribosite
      | #Rnastab
      | #Proteasesite
      | #Proteinstab
      | #Rpromoter
      | #Rarrow
      | #Larrow
      | #Lpromoter
      | #Polygon(int, float)
    ])
    | @ocaml.doc(" Sets the layout style of the vertex.
        Several styles may be combined simultaneously. ")
    #Style([#Rounded | #Filled | #Solid | #Dashed | #Dotted | #Bold | #Invis])
    | @ocaml.doc(" Sets the minimum width.  Default value is [0.75]. ") #Width(float)
  ]

  @ocaml.doc(" Attributes of edges. ")
  type edge = [
    | @ocaml.doc(" Sets the edge stroke color.  Default value is [black]. ") #Color(color)
    | @ocaml.doc(" Sets the edge stroke color with a transparency
        component. Default value is fully opaque [black] ")
    #ColorWithTransparency(color_with_transparency)
    | @ocaml.doc(" If [true], draws a line connecting labels with their edges. ") #Decorate(bool)
    | @ocaml.doc(" Sets arrow direction.  Default value is [`Forward]. ")
    #Dir([#Forward | #Back | #Both | #None])
    | @ocaml.doc(" Sets the label font color.  Default value is [black]. ") #Fontcolor(color)
    | @ocaml.doc(" Sets the label font family name.  Default value is
        [\"Times-Roman\"]. ")
    #Fontname(string)
    | @ocaml.doc(" Sets the label type size (in points).  Default value is [14]. ") #Fontsize(int)
    | @ocaml.doc(" Sets the label to be attached to the edge.  The string may include
        escaped newlines [\n], [\l], or [\r] for centered, left, or right
        justified lines. ")
    #Label(string)
    | @ocaml.doc(" Like label, in html style. In HTML strings, angle brackets must occur in
        matched pairs, and newlines and other formatting whitespace characters
        are allowed. In addition, the content must be legal XML, so that the
        special XML escape sequences for \", &, <, and > may be necessary in
        order to embed these characters in attribute values or raw text.\"  ")
    #HtmlLabel(string)
    | @ocaml.doc(" Sets the font color for head and tail labels.  Default value is
        [black]. ")
    #Labelfontcolor(color)
    | @ocaml.doc(" Sets the font family name for head and tail labels.  Default
        value is [\"Times-Roman\"]. ")
    #Labelfontname(string)
    | @ocaml.doc(" Sets the font size for head and tail labels (in points).
        Default value is [14]. ")
    #Labelfontsize(int)
    | @ocaml.doc(" Width of the pen (in points) used to draw the edge. Default value
        is [1.0].  ")
    #Penwidth(float)
    | @ocaml.doc(" Sets the layout style of the edge.  Several styles may be combined
          simultaneously. ")
    #Style([#Solid | #Dashed | #Dotted | #Bold | #Invis])
  ]
}

@@ocaml.text(
  /* ************************************************************************* */
  " {2 Interface with the dot engine} "
)

@ocaml.doc(" [DotAttributes] extends [CommonAttributes] and implements [ATTRIBUTES]. ")
module DotAttributes: {
  @ocaml.doc(" Attributes of graphs.  They include all common graph attributes and
      several specific ones.  All attributes described in the \"dot User's
      Manual, February 4, 2002\" are handled, excepted: clusterank, color,
      compound, labeljust, labelloc, ordering, rank, remincross, rotate,
      searchsize and style. ")
  type graph = [
    | CommonAttributes.graph
    | @ocaml.doc(" Sets the background color and the inital fill color. ") #Bgcolor(color)
    | @ocaml.doc(" Sets the background color and the inital fill color with
        a transparency component. ")
    #BgcolorWithTransparency(color_with_transparency)
    | @ocaml.doc(" Comment string. ") #Comment(string)
    | @ocaml.doc(" If [true], enables edge concentrators.  Default value is [false]. ")
    #Concentrate(bool)
    | @ocaml.doc(" List of directories for fonts. ") #Fontpath(string)
    | @ocaml.doc(" List of layers. ") #Layers(list<string>)
    | @ocaml.doc(" Sets the page margin (included in the page size).  Default value is
        [0.5]. ")
    #Margin(float)
    | @ocaml.doc(" Scale factor for mincross iterations.  Default value is [1.0]. ") #Mclimit(float)
    | @ocaml.doc(" Sets the minimum separation between nodes, in inches.  Default
        value is [0.25]. ")
    #Nodesep(float)
    | @ocaml.doc(" If set of [f], bounds network simplex iterations by [f *
        <number of nodes>] when ranking nodes. ")
    #Nslimit(int)
    | @ocaml.doc(" If set of [f], bounds network simplex iterations by [f *
        <number of nodes>] when setting x-coordinates. ")
    #Nslimit1(int)
    | @ocaml.doc(" Sets the minimum separation between ranks. ") #Ranksep(float)
    | @ocaml.doc(" If not [0.0], node label dimensions will be rounded to integral
        multiples of it.  Default value is [0.0]. ")
    #Quantum(float)
    | @ocaml.doc(" Direction of rank ordering.  Default value is [`TopToBottom]. ")
    #Rankdir([#TopToBottom | #BottomToTop | #LeftToRight | #RightToLeft])
    | @ocaml.doc(" Sets the aspect ratio. ") #Ratio([#Float(float) | #Fill | #Compress | #Auto])
    | @ocaml.doc(" Number of points used to represent ellipses and circles on output.
        Default value is [8]. ")
    #Samplepoints(int)
    | @ocaml.doc(" URL associated with graph (format-dependent). ") #Url(string)
  ]

  @ocaml.doc(" Attributes of nodes.  They include all common node attributes and
      several specific ones.  All attributes described in the \"dot User's
      Manual, February 4, 2002\" are handled, excepted: bottomlabel, group,
      shapefile and toplabel. ")
  type vertex = [
    | CommonAttributes.vertex
    | @ocaml.doc(" Comment string. ") #Comment(string)
    | #Distortion(float)
    | @ocaml.doc(" Sets the fill color (used when `Style filled).  Default value
        is [lightgrey]. ")
    #Fillcolor(color)
    | @ocaml.doc(" Sets the fill color (used when `Style filled) with a transparency
        component.  Default value is fully opaque [lightgrey]. ")
    #FillcolorWithTransparency(color_with_transparency)
    | @ocaml.doc(" If [true], forces the given dimensions to be the actual ones.
        Default value is [false]. ")
    #Fixedsize(bool)
    | @ocaml.doc(" Overlay. ") #Layer(string)
    | @ocaml.doc(" The  default  url  for  image  map  files; in PostScript files,
        the base URL for all relative URLs, as recognized by Acrobat
        Distiller 3.0 and up. ")
    #Url(string)
    | @ocaml.doc(" z coordinate for VRML output. ") #Z(float)
  ]

  @ocaml.doc(" Attributes of edges.  They include all common edge attributes and
      several specific ones.  All attributes described in the \"dot User's
      Manual, February 4, 2002\" are handled, excepted: lhead and ltail. ")
  type edge = [
    | CommonAttributes.edge
    | @ocaml.doc(" Sets the style of the head arrow.  Default value is [`Normal]. ")
    #Arrowhead(arrow_style)
    | @ocaml.doc(" Sets the scaling factor of arrowheads.  Default value is [1.0]. ")
    #Arrowsize(float)
    | @ocaml.doc(" Sets the style of the tail arrow.  Default value is [`Normal]. ")
    #Arrowtail(arrow_style)
    | @ocaml.doc(" Comment string. ") #Comment(string)
    | @ocaml.doc(" If [false], causes an edge to be ignored for rank assignment.
        Default value is [true]. ")
    #Constraint(bool)
    | @ocaml.doc(" Sets the label attached to the head arrow. ") #Headlabel(string)
    | #Headport([#N | #NE | #E | #SE | #S | #SW | #W | #NW])
    | @ocaml.doc(" Url attached to head label if output format is ismap. ") #Headurl(string)
    | @ocaml.doc(" Angle in degrees which head or tail label is rotated off edge.
        Default value is [-25.0]. ")
    #Labelangle(float)
    | @ocaml.doc(" Scaling factor for distance of head or tail label from node.
        Default value is [1.0]. ")
    #Labeldistance(float)
    | @ocaml.doc(" If [true], lessen constraints on edge label placement.
        Default value is [false]. ")
    #Labelfloat(bool)
    | @ocaml.doc(" Overlay. ") #Layer(string)
    | @ocaml.doc(" Minimum rank distance between head an tail.
        Default value is [1]. ")
    #Minlen(int)
    | @ocaml.doc(" Tag for head node; edge heads with the same tag are merged onto the
        same port. ")
    #Samehead(string)
    | @ocaml.doc(" Tag for tail node; edge tails with the same tag are merged onto the
        same port. ")
    #Sametail(string)
    | @ocaml.doc(" Sets the label attached to the tail arrow. ") #Taillabel(string)
    | #Tailport([#N | #NE | #E | #SE | #S | #SW | #W | #NW])
    | @ocaml.doc(" Url attached to tail label if output format is ismap. ") #Tailurl(string)
    | @ocaml.doc(" Sets the integer cost of stretching the edge.  Default value is
          [1]. ")
    #Weight(int)
  ]

  @ocaml.doc(" Subgraphs have a name and some vertices. ")
  type subgraph = {
    sg_name: string,
    sg_attributes: list<vertex>,
    sg_parent: option<string>,
  }
}

@ocaml.doc(" Graph module with dot attributes ")
module type GraphWithDotAttrs = {
  include Sig.G

  let graph_attributes: t => list<DotAttributes.graph>

  @@ocaml.text(" Vertex attributes ")

  let default_vertex_attributes: t => list<DotAttributes.vertex>
  let vertex_name: V.t => string
  let vertex_attributes: V.t => list<DotAttributes.vertex>

  @@ocaml.text(" Edge attributes ")

  let default_edge_attributes: t => list<DotAttributes.edge>
  let edge_attributes: E.t => list<DotAttributes.edge>

  @ocaml.doc(" The box (if exists) which the vertex belongs to. Boxes with same
         names are not distinguished and so they should have the same
         attributes. ")
  let get_subgraph: V.t => option<DotAttributes.subgraph>
}

module Dot: (
  X: {
    @@ocaml.text(" Graph implementation. Sub-signature of {!Sig.G} ")

    type t
    module V: {
      type t
    }
    module E: {
      type t
      let src: t => V.t
      let dst: t => V.t
    }
    let iter_vertex: (V.t => unit, t) => unit
    let iter_edges_e: (E.t => unit, t) => unit

    @@ocaml.text(" Graph, vertex and edge attributes. ")

    let graph_attributes: t => list<DotAttributes.graph>

    let default_vertex_attributes: t => list<DotAttributes.vertex>
    let vertex_name: V.t => string
    let vertex_attributes: V.t => list<DotAttributes.vertex>

    @ocaml.doc(" The box (if exists) which the vertex belongs to. Boxes with same
           names are not distinguished and so they should have the same
           attributes. ")
    let get_subgraph: V.t => option<DotAttributes.subgraph>

    let default_edge_attributes: t => list<DotAttributes.edge>
    let edge_attributes: E.t => list<DotAttributes.edge>
  },
) =>
{
  @ocaml.doc(" [fprint_graph ppf graph] pretty prints the graph [graph] in
      the CGL language on the formatter [ppf]. ")
  let fprint_graph: (formatter, X.t) => unit

  @ocaml.doc(" [output_graph oc graph] pretty prints the graph [graph] in the dot
      language on the channel [oc]. ")
  let output_graph: (out_channel, X.t) => unit
}

@@ocaml.text(
  /* ************************************************************************* */
  " {2 The neato engine} "
)

module NeatoAttributes: {
  @ocaml.doc(" Attributes of graphs.  They include all common graph attributes and
      several specific ones.  All attributes described in the \"Neato User's
      manual, April 10, 2002\" are handled. ")
  type graph = [
    | CommonAttributes.graph
    | @ocaml.doc(" Sets the page margin (included in the page size).  Default value is
        [0.5, 0.5]. ")
    #Margin(float, float)
    | @ocaml.doc(" Seed for random number generator. ") #Start(int)
    | @ocaml.doc(" Default value is [true]. ") #Overlap(bool)
    | @ocaml.doc(" [true] makes edge splines if nodes don't overlap.
        Default value is [false]. ")
    #Spline(bool)
    | @ocaml.doc(" Edge spline separation factor from nodes.  Default value
          is [0.0]. ")
    #Sep(float)
  ]

  @ocaml.doc(" Attributes of nodes.  They include all common node attributes and
      several specific ones.  All attributes described in the \"Neato User's
      manual, April 10, 2002\" are handled. ")
  type vertex = [
    | CommonAttributes.vertex
    | @ocaml.doc(" Initial coordinates of the vertex. ") #Pos(float, float)
  ]

  @ocaml.doc(" Attributes of edges.  They include all common edge attributes and
      several specific ones.  All attributes described in the \"Neato User's
      manual, April 10, 2002\" are handled. ")
  type edge = [
    | CommonAttributes.edge
    | @ocaml.doc(" Optional value to distinguish multiple edges. ") #Id(string)
    | @ocaml.doc(" Preferred length of edge.  Default value is [1.0]. ") #Len(float)
    | @ocaml.doc(" Strength of edge spring.  Default value is [1.0]. ") #Weight(float)
  ]

  @ocaml.doc(" Subgraphs have a name and some vertices. ")
  type subgraph = {
    sg_name: string,
    sg_attributes: list<vertex>,
    sg_parent: option<string>,
  }
}

module Neato: (
  X: {
    @@ocaml.text(" Graph implementation. Sub-signature of {!Sig.G}. ")

    type t
    module V: {
      type t
    }
    module E: {
      type t
      let src: t => V.t
      let dst: t => V.t
    }

    let iter_vertex: (V.t => unit, t) => unit
    let iter_edges_e: (E.t => unit, t) => unit

    @@ocaml.text(" Graph, vertex and edge attributes. ")

    let graph_attributes: t => list<NeatoAttributes.graph>

    let default_vertex_attributes: t => list<NeatoAttributes.vertex>
    let vertex_name: V.t => string
    let vertex_attributes: V.t => list<NeatoAttributes.vertex>

    @ocaml.doc(" The box (if exists) which the vertex belongs to. Boxes with same
           names are not distinguished and so they should have the same
           attributes. ")
    let get_subgraph: V.t => option<NeatoAttributes.subgraph>

    let default_edge_attributes: t => list<NeatoAttributes.edge>
    let edge_attributes: E.t => list<NeatoAttributes.edge>
  },
) =>
{
  @ocaml.doc(" Several functions provided by this module run the external program
      {i neato}.  By default, this command is supposed to be in the default
      path and is invoked by {i neato}.  The function
      [set_command] allows to set an alternative path at run time. ")
  let set_command: string => unit

  exception Error(string)
  let handle_error: ('a => 'b, 'a) => 'b

  @ocaml.doc(" [fprint_graph ppf graph] pretty prints the graph [graph] in
      the CGL language on the formatter [ppf]. ")
  let fprint_graph: (formatter, X.t) => unit

  @ocaml.doc(" [output_graph oc graph] pretty prints the graph [graph] in the dot
      language on the channel [oc]. ")
  let output_graph: (out_channel, X.t) => unit
}

/*
Local Variables:
compile-command: "make -C .."
End:
*/
