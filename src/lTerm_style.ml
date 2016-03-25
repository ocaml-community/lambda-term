(*
 * lTerm_style.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Core.Std

(* +-----------------------------------------------------------------+
   | Colors                                                          |
   +-----------------------------------------------------------------+ *)

module Color = struct
  (* We encode this type on an integer:

     {[
       type t =
         | Transparent            (* 1                               *)
         | Default                (* 2                               *)
         | RGB of int * int * int (* 3 + r * 0x10000 + g * 0x100 + b *)
         | Index of int           (* 0<=n<=255 -> n + 0x1000002      *)
     ]}

     Colors are used heavily, so it make sense to optimize them.
  *)
  type t = int [@@deriving compare]

  module Kind = struct
    type t =
      | Transparent
      | Default
      | Index
      | RGB
    [@@deriving sexp]
  end

  let transparent   = 0 (* must be zero, this is used for style merging *)
  let default       = transparent + 1
  let first_rgb     = default + 1
  let last_rgb      = first_rgb + 0xff_ff_ff
  let first_indexed = last_rgb + 1

  let index n =
    if n < 0 || n > 255 then failwithf "color index out of 0..255 (%d)" n ();
    n + first_indexed
  ;;

  let rgb r g b =
    if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
      failwiths "RGB componenets out of 0..255"
        (`red r, `green g, `blue b)
        [%sexp_of: [ `red of int ] * [ `green of int ] * [ `blue of int ]];
    first_rgb + ((r lsl 16) lor (g lsl 8) lor b)
  ;;

  let kind t : Kind.t =
    if t < first_rgb then begin
      if t = transparent then
        Transparent
      else
        Default
    end else begin
      if t < first_indexed then
        RGB
      else
        Index
    end
  ;;

  let black    = index  0
  let red      = index  1
  let green    = index  2
  let yellow   = index  3
  let blue     = index  4
  let magenta  = index  5
  let cyan     = index  6
  let white    = index  7
  let lblack   = index  8
  let lred     = index  9
  let lgreen   = index 10
  let lyellow  = index 11
  let lblue    = index 12
  let lmagenta = index 13
  let lcyan    = index 14
  let lwhite   = index 15

  (* +---------------------------------------------------------------+
     | Color names                                                   |
     +---------------------------------------------------------------+ *)

  let color_names =
    [ "transparent"   , transparent
    ; "default"       , default
    ; "black"         , black
    ; "red"           , red
    ; "green"         , green
    ; "yellow"        , yellow
    ; "blue"          , blue
    ; "magenta"       , magenta
    ; "cyan"          , cyan
    ; "white"         , white
    ; "lblack"        , lblack
    ; "lred"          , lred
    ; "lgreen"        , lgreen
    ; "lyellow"       , lyellow
    ; "lblue"         , lblue
    ; "lmagenta"      , lmagenta
    ; "lcyan"         , lcyan
    ; "lwhite"        , lwhite
    ; "light-black"   , lblack
    ; "light-red"     , lred
    ; "light-green"   , lgreen
    ; "light-yellow"  , lyellow
    ; "light-blue"    , lblue
    ; "light-magenta" , lmagenta
    ; "light-cyan"    , lcyan
    ; "light-white"   , lwhite
    ]

  let x11_color_names =
    [ "x-snow"                   , 0xfffafa
    ; "x-ghost-white"            , 0xf8f8ff
    ; "x-ghostwhite"             , 0xf8f8ff
    ; "x-white-smoke"            , 0xf5f5f5
    ; "x-whitesmoke"             , 0xf5f5f5
    ; "x-gainsboro"              , 0xdcdcdc
    ; "x-floral-white"           , 0xfffaf0
    ; "x-floralwhite"            , 0xfffaf0
    ; "x-old-lace"               , 0xfdf5e6
    ; "x-oldlace"                , 0xfdf5e6
    ; "x-linen"                  , 0xfaf0e6
    ; "x-antique-white"          , 0xfaebd7
    ; "x-antiquewhite"           , 0xfaebd7
    ; "x-papaya-whip"            , 0xffefd5
    ; "x-papayawhip"             , 0xffefd5
    ; "x-blanched-almond"        , 0xffebcd
    ; "x-blanchedalmond"         , 0xffebcd
    ; "x-bisque"                 , 0xffe4c4
    ; "x-peach-puff"             , 0xffdab9
    ; "x-peachpuff"              , 0xffdab9
    ; "x-navajo-white"           , 0xffdead
    ; "x-navajowhite"            , 0xffdead
    ; "x-moccasin"               , 0xffe4b5
    ; "x-cornsilk"               , 0xfff8dc
    ; "x-ivory"                  , 0xfffff0
    ; "x-lemon-chiffon"          , 0xfffacd
    ; "x-lemonchiffon"           , 0xfffacd
    ; "x-seashell"               , 0xfff5ee
    ; "x-honeydew"               , 0xf0fff0
    ; "x-mint-cream"             , 0xf5fffa
    ; "x-mintcream"              , 0xf5fffa
    ; "x-azure"                  , 0xf0ffff
    ; "x-alice-blue"             , 0xf0f8ff
    ; "x-aliceblue"              , 0xf0f8ff
    ; "x-lavender"               , 0xe6e6fa
    ; "x-lavender-blush"         , 0xfff0f5
    ; "x-lavenderblush"          , 0xfff0f5
    ; "x-misty-rose"             , 0xffe4e1
    ; "x-mistyrose"              , 0xffe4e1
    ; "x-white"                  , 0xffffff
    ; "x-black"                  , 0x000000
    ; "x-dark-slate-gray"        , 0x2f4f4f
    ; "x-darkslategray"          , 0x2f4f4f
    ; "x-dark-slate-grey"        , 0x2f4f4f
    ; "x-darkslategrey"          , 0x2f4f4f
    ; "x-dim-gray"               , 0x696969
    ; "x-dimgray"                , 0x696969
    ; "x-dim-grey"               , 0x696969
    ; "x-dimgrey"                , 0x696969
    ; "x-slate-gray"             , 0x708090
    ; "x-slategray"              , 0x708090
    ; "x-slate-grey"             , 0x708090
    ; "x-slategrey"              , 0x708090
    ; "x-light-slate-gray"       , 0x778899
    ; "x-lightslategray"         , 0x778899
    ; "x-light-slate-grey"       , 0x778899
    ; "x-lightslategrey"         , 0x778899
    ; "x-gray"                   , 0xbebebe
    ; "x-grey"                   , 0xbebebe
    ; "x-light-grey"             , 0xd3d3d3
    ; "x-lightgrey"              , 0xd3d3d3
    ; "x-light-gray"             , 0xd3d3d3
    ; "x-lightgray"              , 0xd3d3d3
    ; "x-midnight-blue"          , 0x191970
    ; "x-midnightblue"           , 0x191970
    ; "x-navy"                   , 0x000080
    ; "x-navy-blue"              , 0x000080
    ; "x-navyblue"               , 0x000080
    ; "x-cornflower-blue"        , 0x6495ed
    ; "x-cornflowerblue"         , 0x6495ed
    ; "x-dark-slate-blue"        , 0x483d8b
    ; "x-darkslateblue"          , 0x483d8b
    ; "x-slate-blue"             , 0x6a5acd
    ; "x-slateblue"              , 0x6a5acd
    ; "x-medium-slate-blue"      , 0x7b68ee
    ; "x-mediumslateblue"        , 0x7b68ee
    ; "x-light-slate-blue"       , 0x8470ff
    ; "x-lightslateblue"         , 0x8470ff
    ; "x-medium-blue"            , 0x0000cd
    ; "x-mediumblue"             , 0x0000cd
    ; "x-royal-blue"             , 0x4169e1
    ; "x-royalblue"              , 0x4169e1
    ; "x-blue"                   , 0x0000ff
    ; "x-dodger-blue"            , 0x1e90ff
    ; "x-dodgerblue"             , 0x1e90ff
    ; "x-deep-sky-blue"          , 0x00bfff
    ; "x-deepskyblue"            , 0x00bfff
    ; "x-sky-blue"               , 0x87ceeb
    ; "x-skyblue"                , 0x87ceeb
    ; "x-light-sky-blue"         , 0x87cefa
    ; "x-lightskyblue"           , 0x87cefa
    ; "x-steel-blue"             , 0x4682b4
    ; "x-steelblue"              , 0x4682b4
    ; "x-light-steel-blue"       , 0xb0c4de
    ; "x-lightsteelblue"         , 0xb0c4de
    ; "x-light-blue"             , 0xadd8e6
    ; "x-lightblue"              , 0xadd8e6
    ; "x-powder-blue"            , 0xb0e0e6
    ; "x-powderblue"             , 0xb0e0e6
    ; "x-pale-turquoise"         , 0xafeeee
    ; "x-paleturquoise"          , 0xafeeee
    ; "x-dark-turquoise"         , 0x00ced1
    ; "x-darkturquoise"          , 0x00ced1
    ; "x-medium-turquoise"       , 0x48d1cc
    ; "x-mediumturquoise"        , 0x48d1cc
    ; "x-turquoise"              , 0x40e0d0
    ; "x-cyan"                   , 0x00ffff
    ; "x-light-cyan"             , 0xe0ffff
    ; "x-lightcyan"              , 0xe0ffff
    ; "x-cadet-blue"             , 0x5f9ea0
    ; "x-cadetblue"              , 0x5f9ea0
    ; "x-medium-aquamarine"      , 0x66cdaa
    ; "x-mediumaquamarine"       , 0x66cdaa
    ; "x-aquamarine"             , 0x7fffd4
    ; "x-dark-green"             , 0x006400
    ; "x-darkgreen"              , 0x006400
    ; "x-dark-olive-green"       , 0x556b2f
    ; "x-darkolivegreen"         , 0x556b2f
    ; "x-dark-sea-green"         , 0x8fbc8f
    ; "x-darkseagreen"           , 0x8fbc8f
    ; "x-sea-green"              , 0x2e8b57
    ; "x-seagreen"               , 0x2e8b57
    ; "x-medium-sea-green"       , 0x3cb371
    ; "x-mediumseagreen"         , 0x3cb371
    ; "x-light-sea-green"        , 0x20b2aa
    ; "x-lightseagreen"          , 0x20b2aa
    ; "x-pale-green"             , 0x98fb98
    ; "x-palegreen"              , 0x98fb98
    ; "x-spring-green"           , 0x00ff7f
    ; "x-springgreen"            , 0x00ff7f
    ; "x-lawn-green"             , 0x7cfc00
    ; "x-lawngreen"              , 0x7cfc00
    ; "x-green"                  , 0x00ff00
    ; "x-chartreuse"             , 0x7fff00
    ; "x-medium-spring-green"    , 0x00fa9a
    ; "x-mediumspringgreen"      , 0x00fa9a
    ; "x-green-yellow"           , 0xadff2f
    ; "x-greenyellow"            , 0xadff2f
    ; "x-lime-green"             , 0x32cd32
    ; "x-limegreen"              , 0x32cd32
    ; "x-yellow-green"           , 0x9acd32
    ; "x-yellowgreen"            , 0x9acd32
    ; "x-forest-green"           , 0x228b22
    ; "x-forestgreen"            , 0x228b22
    ; "x-olive-drab"             , 0x6b8e23
    ; "x-olivedrab"              , 0x6b8e23
    ; "x-dark-khaki"             , 0xbdb76b
    ; "x-darkkhaki"              , 0xbdb76b
    ; "x-khaki"                  , 0xf0e68c
    ; "x-pale-goldenrod"         , 0xeee8aa
    ; "x-palegoldenrod"          , 0xeee8aa
    ; "x-light-goldenrod-yellow" , 0xfafad2
    ; "x-lightgoldenrodyellow"   , 0xfafad2
    ; "x-light-yellow"           , 0xffffe0
    ; "x-lightyellow"            , 0xffffe0
    ; "x-yellow"                 , 0xffff00
    ; "x-gold"                   , 0xffd700
    ; "x-light-goldenrod"        , 0xeedd82
    ; "x-lightgoldenrod"         , 0xeedd82
    ; "x-goldenrod"              , 0xdaa520
    ; "x-dark-goldenrod"         , 0xb8860b
    ; "x-darkgoldenrod"          , 0xb8860b
    ; "x-rosy-brown"             , 0xbc8f8f
    ; "x-rosybrown"              , 0xbc8f8f
    ; "x-indian-red"             , 0xcd5c5c
    ; "x-indianred"              , 0xcd5c5c
    ; "x-saddle-brown"           , 0x8b4513
    ; "x-saddlebrown"            , 0x8b4513
    ; "x-sienna"                 , 0xa0522d
    ; "x-peru"                   , 0xcd853f
    ; "x-burlywood"              , 0xdeb887
    ; "x-beige"                  , 0xf5f5dc
    ; "x-wheat"                  , 0xf5deb3
    ; "x-sandy-brown"            , 0xf4a460
    ; "x-sandybrown"             , 0xf4a460
    ; "x-tan"                    , 0xd2b48c
    ; "x-chocolate"              , 0xd2691e
    ; "x-firebrick"              , 0xb22222
    ; "x-brown"                  , 0xa52a2a
    ; "x-dark-salmon"            , 0xe9967a
    ; "x-darksalmon"             , 0xe9967a
    ; "x-salmon"                 , 0xfa8072
    ; "x-light-salmon"           , 0xffa07a
    ; "x-lightsalmon"            , 0xffa07a
    ; "x-orange"                 , 0xffa500
    ; "x-dark-orange"            , 0xff8c00
    ; "x-darkorange"             , 0xff8c00
    ; "x-coral"                  , 0xff7f50
    ; "x-light-coral"            , 0xf08080
    ; "x-lightcoral"             , 0xf08080
    ; "x-tomato"                 , 0xff6347
    ; "x-orange-red"             , 0xff4500
    ; "x-orangered"              , 0xff4500
    ; "x-red"                    , 0xff0000
    ; "x-hot-pink"               , 0xff69b4
    ; "x-hotpink"                , 0xff69b4
    ; "x-deep-pink"              , 0xff1493
    ; "x-deeppink"               , 0xff1493
    ; "x-pink"                   , 0xffc0cb
    ; "x-light-pink"             , 0xffb6c1
    ; "x-lightpink"              , 0xffb6c1
    ; "x-pale-violet-red"        , 0xdb7093
    ; "x-palevioletred"          , 0xdb7093
    ; "x-maroon"                 , 0xb03060
    ; "x-medium-violet-red"      , 0xc71585
    ; "x-mediumvioletred"        , 0xc71585
    ; "x-violet-red"             , 0xd02090
    ; "x-violetred"              , 0xd02090
    ; "x-magenta"                , 0xff00ff
    ; "x-violet"                 , 0xee82ee
    ; "x-plum"                   , 0xdda0dd
    ; "x-orchid"                 , 0xda70d6
    ; "x-medium-orchid"          , 0xba55d3
    ; "x-mediumorchid"           , 0xba55d3
    ; "x-dark-orchid"            , 0x9932cc
    ; "x-darkorchid"             , 0x9932cc
    ; "x-dark-violet"            , 0x9400d3
    ; "x-darkviolet"             , 0x9400d3
    ; "x-blue-violet"            , 0x8a2be2
    ; "x-blueviolet"             , 0x8a2be2
    ; "x-purple"                 , 0xa020f0
    ; "x-medium-purple"          , 0x9370db
    ; "x-mediumpurple"           , 0x9370db
    ; "x-thistle"                , 0xd8bfd8
    ; "x-snow1"                  , 0xfffafa
    ; "x-snow2"                  , 0xeee9e9
    ; "x-snow3"                  , 0xcdc9c9
    ; "x-snow4"                  , 0x8b8989
    ; "x-seashell1"              , 0xfff5ee
    ; "x-seashell2"              , 0xeee5de
    ; "x-seashell3"              , 0xcdc5bf
    ; "x-seashell4"              , 0x8b8682
    ; "x-antiquewhite1"          , 0xffefdb
    ; "x-antiquewhite2"          , 0xeedfcc
    ; "x-antiquewhite3"          , 0xcdc0b0
    ; "x-antiquewhite4"          , 0x8b8378
    ; "x-bisque1"                , 0xffe4c4
    ; "x-bisque2"                , 0xeed5b7
    ; "x-bisque3"                , 0xcdb79e
    ; "x-bisque4"                , 0x8b7d6b
    ; "x-peachpuff1"             , 0xffdab9
    ; "x-peachpuff2"             , 0xeecbad
    ; "x-peachpuff3"             , 0xcdaf95
    ; "x-peachpuff4"             , 0x8b7765
    ; "x-navajowhite1"           , 0xffdead
    ; "x-navajowhite2"           , 0xeecfa1
    ; "x-navajowhite3"           , 0xcdb38b
    ; "x-navajowhite4"           , 0x8b795e
    ; "x-lemonchiffon1"          , 0xfffacd
    ; "x-lemonchiffon2"          , 0xeee9bf
    ; "x-lemonchiffon3"          , 0xcdc9a5
    ; "x-lemonchiffon4"          , 0x8b8970
    ; "x-cornsilk1"              , 0xfff8dc
    ; "x-cornsilk2"              , 0xeee8cd
    ; "x-cornsilk3"              , 0xcdc8b1
    ; "x-cornsilk4"              , 0x8b8878
    ; "x-ivory1"                 , 0xfffff0
    ; "x-ivory2"                 , 0xeeeee0
    ; "x-ivory3"                 , 0xcdcdc1
    ; "x-ivory4"                 , 0x8b8b83
    ; "x-honeydew1"              , 0xf0fff0
    ; "x-honeydew2"              , 0xe0eee0
    ; "x-honeydew3"              , 0xc1cdc1
    ; "x-honeydew4"              , 0x838b83
    ; "x-lavenderblush1"         , 0xfff0f5
    ; "x-lavenderblush2"         , 0xeee0e5
    ; "x-lavenderblush3"         , 0xcdc1c5
    ; "x-lavenderblush4"         , 0x8b8386
    ; "x-mistyrose1"             , 0xffe4e1
    ; "x-mistyrose2"             , 0xeed5d2
    ; "x-mistyrose3"             , 0xcdb7b5
    ; "x-mistyrose4"             , 0x8b7d7b
    ; "x-azure1"                 , 0xf0ffff
    ; "x-azure2"                 , 0xe0eeee
    ; "x-azure3"                 , 0xc1cdcd
    ; "x-azure4"                 , 0x838b8b
    ; "x-slateblue1"             , 0x836fff
    ; "x-slateblue2"             , 0x7a67ee
    ; "x-slateblue3"             , 0x6959cd
    ; "x-slateblue4"             , 0x473c8b
    ; "x-royalblue1"             , 0x4876ff
    ; "x-royalblue2"             , 0x436eee
    ; "x-royalblue3"             , 0x3a5fcd
    ; "x-royalblue4"             , 0x27408b
    ; "x-blue1"                  , 0x0000ff
    ; "x-blue2"                  , 0x0000ee
    ; "x-blue3"                  , 0x0000cd
    ; "x-blue4"                  , 0x00008b
    ; "x-dodgerblue1"            , 0x1e90ff
    ; "x-dodgerblue2"            , 0x1c86ee
    ; "x-dodgerblue3"            , 0x1874cd
    ; "x-dodgerblue4"            , 0x104e8b
    ; "x-steelblue1"             , 0x63b8ff
    ; "x-steelblue2"             , 0x5cacee
    ; "x-steelblue3"             , 0x4f94cd
    ; "x-steelblue4"             , 0x36648b
    ; "x-deepskyblue1"           , 0x00bfff
    ; "x-deepskyblue2"           , 0x00b2ee
    ; "x-deepskyblue3"           , 0x009acd
    ; "x-deepskyblue4"           , 0x00688b
    ; "x-skyblue1"               , 0x87ceff
    ; "x-skyblue2"               , 0x7ec0ee
    ; "x-skyblue3"               , 0x6ca6cd
    ; "x-skyblue4"               , 0x4a708b
    ; "x-lightskyblue1"          , 0xb0e2ff
    ; "x-lightskyblue2"          , 0xa4d3ee
    ; "x-lightskyblue3"          , 0x8db6cd
    ; "x-lightskyblue4"          , 0x607b8b
    ; "x-slategray1"             , 0xc6e2ff
    ; "x-slategray2"             , 0xb9d3ee
    ; "x-slategray3"             , 0x9fb6cd
    ; "x-slategray4"             , 0x6c7b8b
    ; "x-lightsteelblue1"        , 0xcae1ff
    ; "x-lightsteelblue2"        , 0xbcd2ee
    ; "x-lightsteelblue3"        , 0xa2b5cd
    ; "x-lightsteelblue4"        , 0x6e7b8b
    ; "x-lightblue1"             , 0xbfefff
    ; "x-lightblue2"             , 0xb2dfee
    ; "x-lightblue3"             , 0x9ac0cd
    ; "x-lightblue4"             , 0x68838b
    ; "x-lightcyan1"             , 0xe0ffff
    ; "x-lightcyan2"             , 0xd1eeee
    ; "x-lightcyan3"             , 0xb4cdcd
    ; "x-lightcyan4"             , 0x7a8b8b
    ; "x-paleturquoise1"         , 0xbbffff
    ; "x-paleturquoise2"         , 0xaeeeee
    ; "x-paleturquoise3"         , 0x96cdcd
    ; "x-paleturquoise4"         , 0x668b8b
    ; "x-cadetblue1"             , 0x98f5ff
    ; "x-cadetblue2"             , 0x8ee5ee
    ; "x-cadetblue3"             , 0x7ac5cd
    ; "x-cadetblue4"             , 0x53868b
    ; "x-turquoise1"             , 0x00f5ff
    ; "x-turquoise2"             , 0x00e5ee
    ; "x-turquoise3"             , 0x00c5cd
    ; "x-turquoise4"             , 0x00868b
    ; "x-cyan1"                  , 0x00ffff
    ; "x-cyan2"                  , 0x00eeee
    ; "x-cyan3"                  , 0x00cdcd
    ; "x-cyan4"                  , 0x008b8b
    ; "x-darkslategray1"         , 0x97ffff
    ; "x-darkslategray2"         , 0x8deeee
    ; "x-darkslategray3"         , 0x79cdcd
    ; "x-darkslategray4"         , 0x528b8b
    ; "x-aquamarine1"            , 0x7fffd4
    ; "x-aquamarine2"            , 0x76eec6
    ; "x-aquamarine3"            , 0x66cdaa
    ; "x-aquamarine4"            , 0x458b74
    ; "x-darkseagreen1"          , 0xc1ffc1
    ; "x-darkseagreen2"          , 0xb4eeb4
    ; "x-darkseagreen3"          , 0x9bcd9b
    ; "x-darkseagreen4"          , 0x698b69
    ; "x-seagreen1"              , 0x54ff9f
    ; "x-seagreen2"              , 0x4eee94
    ; "x-seagreen3"              , 0x43cd80
    ; "x-seagreen4"              , 0x2e8b57
    ; "x-palegreen1"             , 0x9aff9a
    ; "x-palegreen2"             , 0x90ee90
    ; "x-palegreen3"             , 0x7ccd7c
    ; "x-palegreen4"             , 0x548b54
    ; "x-springgreen1"           , 0x00ff7f
    ; "x-springgreen2"           , 0x00ee76
    ; "x-springgreen3"           , 0x00cd66
    ; "x-springgreen4"           , 0x008b45
    ; "x-green1"                 , 0x00ff00
    ; "x-green2"                 , 0x00ee00
    ; "x-green3"                 , 0x00cd00
    ; "x-green4"                 , 0x008b00
    ; "x-chartreuse1"            , 0x7fff00
    ; "x-chartreuse2"            , 0x76ee00
    ; "x-chartreuse3"            , 0x66cd00
    ; "x-chartreuse4"            , 0x458b00
    ; "x-olivedrab1"             , 0xc0ff3e
    ; "x-olivedrab2"             , 0xb3ee3a
    ; "x-olivedrab3"             , 0x9acd32
    ; "x-olivedrab4"             , 0x698b22
    ; "x-darkolivegreen1"        , 0xcaff70
    ; "x-darkolivegreen2"        , 0xbcee68
    ; "x-darkolivegreen3"        , 0xa2cd5a
    ; "x-darkolivegreen4"        , 0x6e8b3d
    ; "x-khaki1"                 , 0xfff68f
    ; "x-khaki2"                 , 0xeee685
    ; "x-khaki3"                 , 0xcdc673
    ; "x-khaki4"                 , 0x8b864e
    ; "x-lightgoldenrod1"        , 0xffec8b
    ; "x-lightgoldenrod2"        , 0xeedc82
    ; "x-lightgoldenrod3"        , 0xcdbe70
    ; "x-lightgoldenrod4"        , 0x8b814c
    ; "x-lightyellow1"           , 0xffffe0
    ; "x-lightyellow2"           , 0xeeeed1
    ; "x-lightyellow3"           , 0xcdcdb4
    ; "x-lightyellow4"           , 0x8b8b7a
    ; "x-yellow1"                , 0xffff00
    ; "x-yellow2"                , 0xeeee00
    ; "x-yellow3"                , 0xcdcd00
    ; "x-yellow4"                , 0x8b8b00
    ; "x-gold1"                  , 0xffd700
    ; "x-gold2"                  , 0xeec900
    ; "x-gold3"                  , 0xcdad00
    ; "x-gold4"                  , 0x8b7500
    ; "x-goldenrod1"             , 0xffc125
    ; "x-goldenrod2"             , 0xeeb422
    ; "x-goldenrod3"             , 0xcd9b1d
    ; "x-goldenrod4"             , 0x8b6914
    ; "x-darkgoldenrod1"         , 0xffb90f
    ; "x-darkgoldenrod2"         , 0xeead0e
    ; "x-darkgoldenrod3"         , 0xcd950c
    ; "x-darkgoldenrod4"         , 0x8b6508
    ; "x-rosybrown1"             , 0xffc1c1
    ; "x-rosybrown2"             , 0xeeb4b4
    ; "x-rosybrown3"             , 0xcd9b9b
    ; "x-rosybrown4"             , 0x8b6969
    ; "x-indianred1"             , 0xff6a6a
    ; "x-indianred2"             , 0xee6363
    ; "x-indianred3"             , 0xcd5555
    ; "x-indianred4"             , 0x8b3a3a
    ; "x-sienna1"                , 0xff8247
    ; "x-sienna2"                , 0xee7942
    ; "x-sienna3"                , 0xcd6839
    ; "x-sienna4"                , 0x8b4726
    ; "x-burlywood1"             , 0xffd39b
    ; "x-burlywood2"             , 0xeec591
    ; "x-burlywood3"             , 0xcdaa7d
    ; "x-burlywood4"             , 0x8b7355
    ; "x-wheat1"                 , 0xffe7ba
    ; "x-wheat2"                 , 0xeed8ae
    ; "x-wheat3"                 , 0xcdba96
    ; "x-wheat4"                 , 0x8b7e66
    ; "x-tan1"                   , 0xffa54f
    ; "x-tan2"                   , 0xee9a49
    ; "x-tan3"                   , 0xcd853f
    ; "x-tan4"                   , 0x8b5a2b
    ; "x-chocolate1"             , 0xff7f24
    ; "x-chocolate2"             , 0xee7621
    ; "x-chocolate3"             , 0xcd661d
    ; "x-chocolate4"             , 0x8b4513
    ; "x-firebrick1"             , 0xff3030
    ; "x-firebrick2"             , 0xee2c2c
    ; "x-firebrick3"             , 0xcd2626
    ; "x-firebrick4"             , 0x8b1a1a
    ; "x-brown1"                 , 0xff4040
    ; "x-brown2"                 , 0xee3b3b
    ; "x-brown3"                 , 0xcd3333
    ; "x-brown4"                 , 0x8b2323
    ; "x-salmon1"                , 0xff8c69
    ; "x-salmon2"                , 0xee8262
    ; "x-salmon3"                , 0xcd7054
    ; "x-salmon4"                , 0x8b4c39
    ; "x-lightsalmon1"           , 0xffa07a
    ; "x-lightsalmon2"           , 0xee9572
    ; "x-lightsalmon3"           , 0xcd8162
    ; "x-lightsalmon4"           , 0x8b5742
    ; "x-orange1"                , 0xffa500
    ; "x-orange2"                , 0xee9a00
    ; "x-orange3"                , 0xcd8500
    ; "x-orange4"                , 0x8b5a00
    ; "x-darkorange1"            , 0xff7f00
    ; "x-darkorange2"            , 0xee7600
    ; "x-darkorange3"            , 0xcd6600
    ; "x-darkorange4"            , 0x8b4500
    ; "x-coral1"                 , 0xff7256
    ; "x-coral2"                 , 0xee6a50
    ; "x-coral3"                 , 0xcd5b45
    ; "x-coral4"                 , 0x8b3e2f
    ; "x-tomato1"                , 0xff6347
    ; "x-tomato2"                , 0xee5c42
    ; "x-tomato3"                , 0xcd4f39
    ; "x-tomato4"                , 0x8b3626
    ; "x-orangered1"             , 0xff4500
    ; "x-orangered2"             , 0xee4000
    ; "x-orangered3"             , 0xcd3700
    ; "x-orangered4"             , 0x8b2500
    ; "x-red1"                   , 0xff0000
    ; "x-red2"                   , 0xee0000
    ; "x-red3"                   , 0xcd0000
    ; "x-red4"                   , 0x8b0000
    ; "x-debianred"              , 0xd70751
    ; "x-deeppink1"              , 0xff1493
    ; "x-deeppink2"              , 0xee1289
    ; "x-deeppink3"              , 0xcd1076
    ; "x-deeppink4"              , 0x8b0a50
    ; "x-hotpink1"               , 0xff6eb4
    ; "x-hotpink2"               , 0xee6aa7
    ; "x-hotpink3"               , 0xcd6090
    ; "x-hotpink4"               , 0x8b3a62
    ; "x-pink1"                  , 0xffb5c5
    ; "x-pink2"                  , 0xeea9b8
    ; "x-pink3"                  , 0xcd919e
    ; "x-pink4"                  , 0x8b636c
    ; "x-lightpink1"             , 0xffaeb9
    ; "x-lightpink2"             , 0xeea2ad
    ; "x-lightpink3"             , 0xcd8c95
    ; "x-lightpink4"             , 0x8b5f65
    ; "x-palevioletred1"         , 0xff82ab
    ; "x-palevioletred2"         , 0xee799f
    ; "x-palevioletred3"         , 0xcd6889
    ; "x-palevioletred4"         , 0x8b475d
    ; "x-maroon1"                , 0xff34b3
    ; "x-maroon2"                , 0xee30a7
    ; "x-maroon3"                , 0xcd2990
    ; "x-maroon4"                , 0x8b1c62
    ; "x-violetred1"             , 0xff3e96
    ; "x-violetred2"             , 0xee3a8c
    ; "x-violetred3"             , 0xcd3278
    ; "x-violetred4"             , 0x8b2252
    ; "x-magenta1"               , 0xff00ff
    ; "x-magenta2"               , 0xee00ee
    ; "x-magenta3"               , 0xcd00cd
    ; "x-magenta4"               , 0x8b008b
    ; "x-orchid1"                , 0xff83fa
    ; "x-orchid2"                , 0xee7ae9
    ; "x-orchid3"                , 0xcd69c9
    ; "x-orchid4"                , 0x8b4789
    ; "x-plum1"                  , 0xffbbff
    ; "x-plum2"                  , 0xeeaeee
    ; "x-plum3"                  , 0xcd96cd
    ; "x-plum4"                  , 0x8b668b
    ; "x-mediumorchid1"          , 0xe066ff
    ; "x-mediumorchid2"          , 0xd15fee
    ; "x-mediumorchid3"          , 0xb452cd
    ; "x-mediumorchid4"          , 0x7a378b
    ; "x-darkorchid1"            , 0xbf3eff
    ; "x-darkorchid2"            , 0xb23aee
    ; "x-darkorchid3"            , 0x9a32cd
    ; "x-darkorchid4"            , 0x68228b
    ; "x-purple1"                , 0x9b30ff
    ; "x-purple2"                , 0x912cee
    ; "x-purple3"                , 0x7d26cd
    ; "x-purple4"                , 0x551a8b
    ; "x-mediumpurple1"          , 0xab82ff
    ; "x-mediumpurple2"          , 0x9f79ee
    ; "x-mediumpurple3"          , 0x8968cd
    ; "x-mediumpurple4"          , 0x5d478b
    ; "x-thistle1"               , 0xffe1ff
    ; "x-thistle2"               , 0xeed2ee
    ; "x-thistle3"               , 0xcdb5cd
    ; "x-thistle4"               , 0x8b7b8b
    ; "x-gray0"                  , 0x000000
    ; "x-grey0"                  , 0x000000
    ; "x-gray1"                  , 0x030303
    ; "x-grey1"                  , 0x030303
    ; "x-gray2"                  , 0x050505
    ; "x-grey2"                  , 0x050505
    ; "x-gray3"                  , 0x080808
    ; "x-grey3"                  , 0x080808
    ; "x-gray4"                  , 0x0a0a0a
    ; "x-grey4"                  , 0x0a0a0a
    ; "x-gray5"                  , 0x0d0d0d
    ; "x-grey5"                  , 0x0d0d0d
    ; "x-gray6"                  , 0x0f0f0f
    ; "x-grey6"                  , 0x0f0f0f
    ; "x-gray7"                  , 0x121212
    ; "x-grey7"                  , 0x121212
    ; "x-gray8"                  , 0x141414
    ; "x-grey8"                  , 0x141414
    ; "x-gray9"                  , 0x171717
    ; "x-grey9"                  , 0x171717
    ; "x-gray10"                 , 0x1a1a1a
    ; "x-grey10"                 , 0x1a1a1a
    ; "x-gray11"                 , 0x1c1c1c
    ; "x-grey11"                 , 0x1c1c1c
    ; "x-gray12"                 , 0x1f1f1f
    ; "x-grey12"                 , 0x1f1f1f
    ; "x-gray13"                 , 0x212121
    ; "x-grey13"                 , 0x212121
    ; "x-gray14"                 , 0x242424
    ; "x-grey14"                 , 0x242424
    ; "x-gray15"                 , 0x262626
    ; "x-grey15"                 , 0x262626
    ; "x-gray16"                 , 0x292929
    ; "x-grey16"                 , 0x292929
    ; "x-gray17"                 , 0x2b2b2b
    ; "x-grey17"                 , 0x2b2b2b
    ; "x-gray18"                 , 0x2e2e2e
    ; "x-grey18"                 , 0x2e2e2e
    ; "x-gray19"                 , 0x303030
    ; "x-grey19"                 , 0x303030
    ; "x-gray20"                 , 0x333333
    ; "x-grey20"                 , 0x333333
    ; "x-gray21"                 , 0x363636
    ; "x-grey21"                 , 0x363636
    ; "x-gray22"                 , 0x383838
    ; "x-grey22"                 , 0x383838
    ; "x-gray23"                 , 0x3b3b3b
    ; "x-grey23"                 , 0x3b3b3b
    ; "x-gray24"                 , 0x3d3d3d
    ; "x-grey24"                 , 0x3d3d3d
    ; "x-gray25"                 , 0x404040
    ; "x-grey25"                 , 0x404040
    ; "x-gray26"                 , 0x424242
    ; "x-grey26"                 , 0x424242
    ; "x-gray27"                 , 0x454545
    ; "x-grey27"                 , 0x454545
    ; "x-gray28"                 , 0x474747
    ; "x-grey28"                 , 0x474747
    ; "x-gray29"                 , 0x4a4a4a
    ; "x-grey29"                 , 0x4a4a4a
    ; "x-gray30"                 , 0x4d4d4d
    ; "x-grey30"                 , 0x4d4d4d
    ; "x-gray31"                 , 0x4f4f4f
    ; "x-grey31"                 , 0x4f4f4f
    ; "x-gray32"                 , 0x525252
    ; "x-grey32"                 , 0x525252
    ; "x-gray33"                 , 0x545454
    ; "x-grey33"                 , 0x545454
    ; "x-gray34"                 , 0x575757
    ; "x-grey34"                 , 0x575757
    ; "x-gray35"                 , 0x595959
    ; "x-grey35"                 , 0x595959
    ; "x-gray36"                 , 0x5c5c5c
    ; "x-grey36"                 , 0x5c5c5c
    ; "x-gray37"                 , 0x5e5e5e
    ; "x-grey37"                 , 0x5e5e5e
    ; "x-gray38"                 , 0x616161
    ; "x-grey38"                 , 0x616161
    ; "x-gray39"                 , 0x636363
    ; "x-grey39"                 , 0x636363
    ; "x-gray40"                 , 0x666666
    ; "x-grey40"                 , 0x666666
    ; "x-gray41"                 , 0x696969
    ; "x-grey41"                 , 0x696969
    ; "x-gray42"                 , 0x6b6b6b
    ; "x-grey42"                 , 0x6b6b6b
    ; "x-gray43"                 , 0x6e6e6e
    ; "x-grey43"                 , 0x6e6e6e
    ; "x-gray44"                 , 0x707070
    ; "x-grey44"                 , 0x707070
    ; "x-gray45"                 , 0x737373
    ; "x-grey45"                 , 0x737373
    ; "x-gray46"                 , 0x757575
    ; "x-grey46"                 , 0x757575
    ; "x-gray47"                 , 0x787878
    ; "x-grey47"                 , 0x787878
    ; "x-gray48"                 , 0x7a7a7a
    ; "x-grey48"                 , 0x7a7a7a
    ; "x-gray49"                 , 0x7d7d7d
    ; "x-grey49"                 , 0x7d7d7d
    ; "x-gray50"                 , 0x7f7f7f
    ; "x-grey50"                 , 0x7f7f7f
    ; "x-gray51"                 , 0x828282
    ; "x-grey51"                 , 0x828282
    ; "x-gray52"                 , 0x858585
    ; "x-grey52"                 , 0x858585
    ; "x-gray53"                 , 0x878787
    ; "x-grey53"                 , 0x878787
    ; "x-gray54"                 , 0x8a8a8a
    ; "x-grey54"                 , 0x8a8a8a
    ; "x-gray55"                 , 0x8c8c8c
    ; "x-grey55"                 , 0x8c8c8c
    ; "x-gray56"                 , 0x8f8f8f
    ; "x-grey56"                 , 0x8f8f8f
    ; "x-gray57"                 , 0x919191
    ; "x-grey57"                 , 0x919191
    ; "x-gray58"                 , 0x949494
    ; "x-grey58"                 , 0x949494
    ; "x-gray59"                 , 0x969696
    ; "x-grey59"                 , 0x969696
    ; "x-gray60"                 , 0x999999
    ; "x-grey60"                 , 0x999999
    ; "x-gray61"                 , 0x9c9c9c
    ; "x-grey61"                 , 0x9c9c9c
    ; "x-gray62"                 , 0x9e9e9e
    ; "x-grey62"                 , 0x9e9e9e
    ; "x-gray63"                 , 0xa1a1a1
    ; "x-grey63"                 , 0xa1a1a1
    ; "x-gray64"                 , 0xa3a3a3
    ; "x-grey64"                 , 0xa3a3a3
    ; "x-gray65"                 , 0xa6a6a6
    ; "x-grey65"                 , 0xa6a6a6
    ; "x-gray66"                 , 0xa8a8a8
    ; "x-grey66"                 , 0xa8a8a8
    ; "x-gray67"                 , 0xababab
    ; "x-grey67"                 , 0xababab
    ; "x-gray68"                 , 0xadadad
    ; "x-grey68"                 , 0xadadad
    ; "x-gray69"                 , 0xb0b0b0
    ; "x-grey69"                 , 0xb0b0b0
    ; "x-gray70"                 , 0xb3b3b3
    ; "x-grey70"                 , 0xb3b3b3
    ; "x-gray71"                 , 0xb5b5b5
    ; "x-grey71"                 , 0xb5b5b5
    ; "x-gray72"                 , 0xb8b8b8
    ; "x-grey72"                 , 0xb8b8b8
    ; "x-gray73"                 , 0xbababa
    ; "x-grey73"                 , 0xbababa
    ; "x-gray74"                 , 0xbdbdbd
    ; "x-grey74"                 , 0xbdbdbd
    ; "x-gray75"                 , 0xbfbfbf
    ; "x-grey75"                 , 0xbfbfbf
    ; "x-gray76"                 , 0xc2c2c2
    ; "x-grey76"                 , 0xc2c2c2
    ; "x-gray77"                 , 0xc4c4c4
    ; "x-grey77"                 , 0xc4c4c4
    ; "x-gray78"                 , 0xc7c7c7
    ; "x-grey78"                 , 0xc7c7c7
    ; "x-gray79"                 , 0xc9c9c9
    ; "x-grey79"                 , 0xc9c9c9
    ; "x-gray80"                 , 0xcccccc
    ; "x-grey80"                 , 0xcccccc
    ; "x-gray81"                 , 0xcfcfcf
    ; "x-grey81"                 , 0xcfcfcf
    ; "x-gray82"                 , 0xd1d1d1
    ; "x-grey82"                 , 0xd1d1d1
    ; "x-gray83"                 , 0xd4d4d4
    ; "x-grey83"                 , 0xd4d4d4
    ; "x-gray84"                 , 0xd6d6d6
    ; "x-grey84"                 , 0xd6d6d6
    ; "x-gray85"                 , 0xd9d9d9
    ; "x-grey85"                 , 0xd9d9d9
    ; "x-gray86"                 , 0xdbdbdb
    ; "x-grey86"                 , 0xdbdbdb
    ; "x-gray87"                 , 0xdedede
    ; "x-grey87"                 , 0xdedede
    ; "x-gray88"                 , 0xe0e0e0
    ; "x-grey88"                 , 0xe0e0e0
    ; "x-gray89"                 , 0xe3e3e3
    ; "x-grey89"                 , 0xe3e3e3
    ; "x-gray90"                 , 0xe5e5e5
    ; "x-grey90"                 , 0xe5e5e5
    ; "x-gray91"                 , 0xe8e8e8
    ; "x-grey91"                 , 0xe8e8e8
    ; "x-gray92"                 , 0xebebeb
    ; "x-grey92"                 , 0xebebeb
    ; "x-gray93"                 , 0xededed
    ; "x-grey93"                 , 0xededed
    ; "x-gray94"                 , 0xf0f0f0
    ; "x-grey94"                 , 0xf0f0f0
    ; "x-gray95"                 , 0xf2f2f2
    ; "x-grey95"                 , 0xf2f2f2
    ; "x-gray96"                 , 0xf5f5f5
    ; "x-grey96"                 , 0xf5f5f5
    ; "x-gray97"                 , 0xf7f7f7
    ; "x-grey97"                 , 0xf7f7f7
    ; "x-gray98"                 , 0xfafafa
    ; "x-grey98"                 , 0xfafafa
    ; "x-gray99"                 , 0xfcfcfc
    ; "x-grey99"                 , 0xfcfcfc
    ; "x-gray100"                , 0xffffff
    ; "x-grey100"                , 0xffffff
    ; "x-dark-grey"              , 0xa9a9a9
    ; "x-darkgrey"               , 0xa9a9a9
    ; "x-dark-gray"              , 0xa9a9a9
    ; "x-darkgray"               , 0xa9a9a9
    ; "x-dark-blue"              , 0x00008b
    ; "x-darkblue"               , 0x00008b
    ; "x-dark-cyan"              , 0x008b8b
    ; "x-darkcyan"               , 0x008b8b
    ; "x-dark-magenta"           , 0x8b008b
    ; "x-darkmagenta"            , 0x8b008b
    ; "x-dark-red"               , 0x8b0000
    ; "x-darkred"                , 0x8b0000
    ; "x-light-green"            , 0x90ee90
    ; "x-lightgreen"             , 0x90ee90
    ]
    |>  List.map ~f:(fun (name, t) -> (name, t + first_rgb))

  let color_by_name =
    String.Table.of_alist_exn
      (List.rev_append color_names x11_color_names)
  ;;

  let name_of_index = function
    | 0 -> "black"
    | 1 -> "red"
    | 2 -> "green"
    | 3 -> "yellow"
    | 4 -> "blue"
    | 5 -> "magenta"
    | 6 -> "cyan"
    | 7 -> "white"
    | 8 -> "light-black"
    | 9 -> "light-red"
    | 10 -> "light-green"
    | 11 -> "light-yellow"
    | 12 -> "light-blue"
    | 13 -> "light-magenta"
    | 14 -> "light-cyan"
    | 15 -> "light-white"
    | n -> string_of_int n
  ;;

  let to_string t =
    match kind t with
    | Transparent -> "transparent"
    | Default     -> "default"
    | Index       -> name_of_index (t - first_indexed)
    | RGB         -> sprintf "#%06x" t
  ;;

  let hex_of_char =
    let e = Exit in
    fun ch ->
      match ch with
      | '0' .. '9' -> Char.to_int ch - Char.to_int '0'
      | 'A' .. 'F' -> Char.to_int ch - Char.to_int 'A' + 10
      | 'a' .. 'f' -> Char.to_int ch - Char.to_int 'a' + 10
      | _ -> Exn.raise_without_backtrace e

  let invalid_color_string string = failwithf "invalid color string (%S)" string ()

  let of_string = function
    | "" -> invalid_color_string ""
    | s  ->
      if s.[0] = '#' && String.length s = 7 then
        try
          rgb
            ((hex_of_char s.[1] lsl 4) lor hex_of_char s.[2])
            ((hex_of_char s.[3] lsl 4) lor hex_of_char s.[4])
            ((hex_of_char s.[5] lsl 4) lor hex_of_char s.[6])
        with _ ->
          invalid_color_string s
      else
        try
          index (Int.of_string s)
        with _ ->
          match Hashtbl.find color_by_name (String.lowercase s) with
          | Some c -> c
          | None   -> invalid_color_string s
  ;;

  let get_index t (map:LTerm_color_mappings.map) =
    match kind t with
    | Transparent
    | Default -> 9
    | Index   -> t - first_indexed
    | RGB     ->
      let t = t - first_rgb in
      assert (t >= 0 && t <= 0xff_ff_ff);
      let r =  t lsr 16            in
      let g = (t lsr  8) land 0xff in
      let b =  t         land 0xff in
      let n = map.count_g * (Char.to_int (String.unsafe_get map.index_b b)    ) in
      let n = map.count_r * (Char.to_int (String.unsafe_get map.index_g g) + n) in
      let n =               (Char.to_int (String.unsafe_get map.index_r r) + n) in
      Char.to_int (String.unsafe_get map.map n)
  ;;
end

(* +-----------------------------------------------------------------+
   | Styles                                                          |
   +-----------------------------------------------------------------+ *)

module Switch = struct
  type t =
    | On
    | Off
    | Unset

  external to_int : t -> int = "%identity"

  let of_int = function
    | 0 -> On
    | 1 -> Off
    | 2 -> Unset
    | _ -> assert false
end

type t = Int63.t
(* bits:

   -  #0..#24: foreground
   - #25..#49: background
   - #50..#51: bold
   - #52..#53: underline
   - #54..#55: blink
   - #56..#57: reverse
*)

module Bits = struct
  let foreground = Int63.(shift_left one 25 - one)
  let background = Int63.(shift_left one 50 - one - foreground)
  let bold       = Int63.(shift_left (of_int 3) 50)
  let underline  = Int63.(shift_left (of_int 3) 52)
  let blink      = Int63.(shift_left (of_int 3) 54)
  let reverse    = Int63.(shift_left (of_int 3) 56)

  let unset_switches = Int63.(shift_left (of_int 0b10_10_10_10) 50)
end

let equal = Int63.equal

let ( & ) = Int63.bit_and
let ( + ) = Int63.bit_or
let ( - ) t m = t & Int63.bit_not m
let ( lsl ) = Int63.shift_left
let ( lsr ) = Int63.shift_right_logical

let style_of_bold       x = Int63.of_int (Switch.to_int x) lsl 50
let style_of_underline  x = Int63.of_int (Switch.to_int x) lsl 52
let style_of_blink      x = Int63.of_int (Switch.to_int x) lsl 54
let style_of_reverse    x = Int63.of_int (Switch.to_int x) lsl 56
let style_of_foreground x = Int63.of_int x
let style_of_background x = Int63.of_int x lsl 25

let make
      ?(bold      =Switch.Unset)
      ?(underline =Switch.Unset)
      ?(blink     =Switch.Unset)
      ?(reverse   =Switch.Unset)
      ?(foreground=Color.transparent)
      ?(background=Color.transparent)
      ()
  =
  style_of_foreground foreground +
  style_of_background background +
  style_of_bold       bold       +
  style_of_underline  underline  +
  style_of_blink      blink      +
  style_of_reverse    reverse
;;

let bold       t = Switch.of_int (Int63.to_int_exn ((t lsr 50) & Int63.of_int 3))
let underline  t = Switch.of_int (Int63.to_int_exn ((t lsr 52) & Int63.of_int 3))
let blink      t = Switch.of_int (Int63.to_int_exn ((t lsr 54) & Int63.of_int 3))
let reverse    t = Switch.of_int (Int63.to_int_exn ((t lsr 56) & Int63.of_int 3))
let foreground t = Int63.to_int_exn  (t & Bits.foreground)
let background t = Int63.to_int_exn ((t & Bits.background) lsr 25)

let set_bold       t x = t - Bits.bold       + style_of_bold       x
let set_underline  t x = t - Bits.underline  + style_of_underline  x
let set_blink      t x = t - Bits.blink      + style_of_blink      x
let set_reverse    t x = t - Bits.reverse    + style_of_reverse    x
let set_foreground t x = t - Bits.foreground + style_of_foreground x
let set_background t x = t - Bits.background + style_of_background x

let default =
  make
    ~bold:Off
    ~underline:Off
    ~blink:Off
    ~reverse:Off
    ~foreground:Color.default
    ~background:Color.default
    ()
;;

let none = make ()

let merge a b =
  let m = b & Bits.unset_switches in
  let b_unset_switches_mask = m + (m lsr 1) in
  let t = (a & b_unset_switches_mask) + (Int63.bit_xor b m) in
  let a_fg = a & Bits.foreground in
  let b_fg = b & Bits.foreground in
  let t = t + (if b_fg = Int63.zero then a_fg else b_fg) in
  let a_bg = a & Bits.background in
  let b_bg = b & Bits.background in
  let t = t + (if b_bg = Int63.zero then a_bg else b_bg) in
  t;
;;

let on_default t = merge default t

let set
      ?(bold      =Switch.Unset)
      ?(underline =Switch.Unset)
      ?(blink     =Switch.Unset)
      ?(reverse   =Switch.Unset)
      ?(foreground=Color.transparent)
      ?(background=Color.transparent)
      t
  =
  let t' =
    make
      ~bold
      ~underline
      ~blink
      ~reverse
      ~foreground
      ~background
      ()
  in
  merge t t'
;;
