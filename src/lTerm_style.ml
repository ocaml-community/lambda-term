(*
 * lTerm_style.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open StdLabels

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
    if n < 0 || n > 255 then invalid_arg "LTerm_style.Color.index";
    n + first_indexed
  ;;

  let rgb r g b =
    if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
      invalid_arg "LTerm_style.Color.rgb";
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
    let rgb name rgb = (name, first_rgb + rgb) in
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
    (* X11 color names *)
    ; rgb "x-snow"                   0xfffafa
    ; rgb "x-ghost-white"            0xf8f8ff
    ; rgb "x-ghostwhite"             0xf8f8ff
    ; rgb "x-white-smoke"            0xf5f5f5
    ; rgb "x-whitesmoke"             0xf5f5f5
    ; rgb "x-gainsboro"              0xdcdcdc
    ; rgb "x-floral-white"           0xfffaf0
    ; rgb "x-floralwhite"            0xfffaf0
    ; rgb "x-old-lace"               0xfdf5e6
    ; rgb "x-oldlace"                0xfdf5e6
    ; rgb "x-linen"                  0xfaf0e6
    ; rgb "x-antique-white"          0xfaebd7
    ; rgb "x-antiquewhite"           0xfaebd7
    ; rgb "x-papaya-whip"            0xffefd5
    ; rgb "x-papayawhip"             0xffefd5
    ; rgb "x-blanched-almond"        0xffebcd
    ; rgb "x-blanchedalmond"         0xffebcd
    ; rgb "x-bisque"                 0xffe4c4
    ; rgb "x-peach-puff"             0xffdab9
    ; rgb "x-peachpuff"              0xffdab9
    ; rgb "x-navajo-white"           0xffdead
    ; rgb "x-navajowhite"            0xffdead
    ; rgb "x-moccasin"               0xffe4b5
    ; rgb "x-cornsilk"               0xfff8dc
    ; rgb "x-ivory"                  0xfffff0
    ; rgb "x-lemon-chiffon"          0xfffacd
    ; rgb "x-lemonchiffon"           0xfffacd
    ; rgb "x-seashell"               0xfff5ee
    ; rgb "x-honeydew"               0xf0fff0
    ; rgb "x-mint-cream"             0xf5fffa
    ; rgb "x-mintcream"              0xf5fffa
    ; rgb "x-azure"                  0xf0ffff
    ; rgb "x-alice-blue"             0xf0f8ff
    ; rgb "x-aliceblue"              0xf0f8ff
    ; rgb "x-lavender"               0xe6e6fa
    ; rgb "x-lavender-blush"         0xfff0f5
    ; rgb "x-lavenderblush"          0xfff0f5
    ; rgb "x-misty-rose"             0xffe4e1
    ; rgb "x-mistyrose"              0xffe4e1
    ; rgb "x-white"                  0xffffff
    ; rgb "x-black"                  0x000000
    ; rgb "x-dark-slate-gray"        0x2f4f4f
    ; rgb "x-darkslategray"          0x2f4f4f
    ; rgb "x-dark-slate-grey"        0x2f4f4f
    ; rgb "x-darkslategrey"          0x2f4f4f
    ; rgb "x-dim-gray"               0x696969
    ; rgb "x-dimgray"                0x696969
    ; rgb "x-dim-grey"               0x696969
    ; rgb "x-dimgrey"                0x696969
    ; rgb "x-slate-gray"             0x708090
    ; rgb "x-slategray"              0x708090
    ; rgb "x-slate-grey"             0x708090
    ; rgb "x-slategrey"              0x708090
    ; rgb "x-light-slate-gray"       0x778899
    ; rgb "x-lightslategray"         0x778899
    ; rgb "x-light-slate-grey"       0x778899
    ; rgb "x-lightslategrey"         0x778899
    ; rgb "x-gray"                   0xbebebe
    ; rgb "x-grey"                   0xbebebe
    ; rgb "x-light-grey"             0xd3d3d3
    ; rgb "x-lightgrey"              0xd3d3d3
    ; rgb "x-light-gray"             0xd3d3d3
    ; rgb "x-lightgray"              0xd3d3d3
    ; rgb "x-midnight-blue"          0x191970
    ; rgb "x-midnightblue"           0x191970
    ; rgb "x-navy"                   0x000080
    ; rgb "x-navy-blue"              0x000080
    ; rgb "x-navyblue"               0x000080
    ; rgb "x-cornflower-blue"        0x6495ed
    ; rgb "x-cornflowerblue"         0x6495ed
    ; rgb "x-dark-slate-blue"        0x483d8b
    ; rgb "x-darkslateblue"          0x483d8b
    ; rgb "x-slate-blue"             0x6a5acd
    ; rgb "x-slateblue"              0x6a5acd
    ; rgb "x-medium-slate-blue"      0x7b68ee
    ; rgb "x-mediumslateblue"        0x7b68ee
    ; rgb "x-light-slate-blue"       0x8470ff
    ; rgb "x-lightslateblue"         0x8470ff
    ; rgb "x-medium-blue"            0x0000cd
    ; rgb "x-mediumblue"             0x0000cd
    ; rgb "x-royal-blue"             0x4169e1
    ; rgb "x-royalblue"              0x4169e1
    ; rgb "x-blue"                   0x0000ff
    ; rgb "x-dodger-blue"            0x1e90ff
    ; rgb "x-dodgerblue"             0x1e90ff
    ; rgb "x-deep-sky-blue"          0x00bfff
    ; rgb "x-deepskyblue"            0x00bfff
    ; rgb "x-sky-blue"               0x87ceeb
    ; rgb "x-skyblue"                0x87ceeb
    ; rgb "x-light-sky-blue"         0x87cefa
    ; rgb "x-lightskyblue"           0x87cefa
    ; rgb "x-steel-blue"             0x4682b4
    ; rgb "x-steelblue"              0x4682b4
    ; rgb "x-light-steel-blue"       0xb0c4de
    ; rgb "x-lightsteelblue"         0xb0c4de
    ; rgb "x-light-blue"             0xadd8e6
    ; rgb "x-lightblue"              0xadd8e6
    ; rgb "x-powder-blue"            0xb0e0e6
    ; rgb "x-powderblue"             0xb0e0e6
    ; rgb "x-pale-turquoise"         0xafeeee
    ; rgb "x-paleturquoise"          0xafeeee
    ; rgb "x-dark-turquoise"         0x00ced1
    ; rgb "x-darkturquoise"          0x00ced1
    ; rgb "x-medium-turquoise"       0x48d1cc
    ; rgb "x-mediumturquoise"        0x48d1cc
    ; rgb "x-turquoise"              0x40e0d0
    ; rgb "x-cyan"                   0x00ffff
    ; rgb "x-light-cyan"             0xe0ffff
    ; rgb "x-lightcyan"              0xe0ffff
    ; rgb "x-cadet-blue"             0x5f9ea0
    ; rgb "x-cadetblue"              0x5f9ea0
    ; rgb "x-medium-aquamarine"      0x66cdaa
    ; rgb "x-mediumaquamarine"       0x66cdaa
    ; rgb "x-aquamarine"             0x7fffd4
    ; rgb "x-dark-green"             0x006400
    ; rgb "x-darkgreen"              0x006400
    ; rgb "x-dark-olive-green"       0x556b2f
    ; rgb "x-darkolivegreen"         0x556b2f
    ; rgb "x-dark-sea-green"         0x8fbc8f
    ; rgb "x-darkseagreen"           0x8fbc8f
    ; rgb "x-sea-green"              0x2e8b57
    ; rgb "x-seagreen"               0x2e8b57
    ; rgb "x-medium-sea-green"       0x3cb371
    ; rgb "x-mediumseagreen"         0x3cb371
    ; rgb "x-light-sea-green"        0x20b2aa
    ; rgb "x-lightseagreen"          0x20b2aa
    ; rgb "x-pale-green"             0x98fb98
    ; rgb "x-palegreen"              0x98fb98
    ; rgb "x-spring-green"           0x00ff7f
    ; rgb "x-springgreen"            0x00ff7f
    ; rgb "x-lawn-green"             0x7cfc00
    ; rgb "x-lawngreen"              0x7cfc00
    ; rgb "x-green"                  0x00ff00
    ; rgb "x-chartreuse"             0x7fff00
    ; rgb "x-medium-spring-green"    0x00fa9a
    ; rgb "x-mediumspringgreen"      0x00fa9a
    ; rgb "x-green-yellow"           0xadff2f
    ; rgb "x-greenyellow"            0xadff2f
    ; rgb "x-lime-green"             0x32cd32
    ; rgb "x-limegreen"              0x32cd32
    ; rgb "x-yellow-green"           0x9acd32
    ; rgb "x-yellowgreen"            0x9acd32
    ; rgb "x-forest-green"           0x228b22
    ; rgb "x-forestgreen"            0x228b22
    ; rgb "x-olive-drab"             0x6b8e23
    ; rgb "x-olivedrab"              0x6b8e23
    ; rgb "x-dark-khaki"             0xbdb76b
    ; rgb "x-darkkhaki"              0xbdb76b
    ; rgb "x-khaki"                  0xf0e68c
    ; rgb "x-pale-goldenrod"         0xeee8aa
    ; rgb "x-palegoldenrod"          0xeee8aa
    ; rgb "x-light-goldenrod-yellow" 0xfafad2
    ; rgb "x-lightgoldenrodyellow"   0xfafad2
    ; rgb "x-light-yellow"           0xffffe0
    ; rgb "x-lightyellow"            0xffffe0
    ; rgb "x-yellow"                 0xffff00
    ; rgb "x-gold"                   0xffd700
    ; rgb "x-light-goldenrod"        0xeedd82
    ; rgb "x-lightgoldenrod"         0xeedd82
    ; rgb "x-goldenrod"              0xdaa520
    ; rgb "x-dark-goldenrod"         0xb8860b
    ; rgb "x-darkgoldenrod"          0xb8860b
    ; rgb "x-rosy-brown"             0xbc8f8f
    ; rgb "x-rosybrown"              0xbc8f8f
    ; rgb "x-indian-red"             0xcd5c5c
    ; rgb "x-indianred"              0xcd5c5c
    ; rgb "x-saddle-brown"           0x8b4513
    ; rgb "x-saddlebrown"            0x8b4513
    ; rgb "x-sienna"                 0xa0522d
    ; rgb "x-peru"                   0xcd853f
    ; rgb "x-burlywood"              0xdeb887
    ; rgb "x-beige"                  0xf5f5dc
    ; rgb "x-wheat"                  0xf5deb3
    ; rgb "x-sandy-brown"            0xf4a460
    ; rgb "x-sandybrown"             0xf4a460
    ; rgb "x-tan"                    0xd2b48c
    ; rgb "x-chocolate"              0xd2691e
    ; rgb "x-firebrick"              0xb22222
    ; rgb "x-brown"                  0xa52a2a
    ; rgb "x-dark-salmon"            0xe9967a
    ; rgb "x-darksalmon"             0xe9967a
    ; rgb "x-salmon"                 0xfa8072
    ; rgb "x-light-salmon"           0xffa07a
    ; rgb "x-lightsalmon"            0xffa07a
    ; rgb "x-orange"                 0xffa500
    ; rgb "x-dark-orange"            0xff8c00
    ; rgb "x-darkorange"             0xff8c00
    ; rgb "x-coral"                  0xff7f50
    ; rgb "x-light-coral"            0xf08080
    ; rgb "x-lightcoral"             0xf08080
    ; rgb "x-tomato"                 0xff6347
    ; rgb "x-orange-red"             0xff4500
    ; rgb "x-orangered"              0xff4500
    ; rgb "x-red"                    0xff0000
    ; rgb "x-hot-pink"               0xff69b4
    ; rgb "x-hotpink"                0xff69b4
    ; rgb "x-deep-pink"              0xff1493
    ; rgb "x-deeppink"               0xff1493
    ; rgb "x-pink"                   0xffc0cb
    ; rgb "x-light-pink"             0xffb6c1
    ; rgb "x-lightpink"              0xffb6c1
    ; rgb "x-pale-violet-red"        0xdb7093
    ; rgb "x-palevioletred"          0xdb7093
    ; rgb "x-maroon"                 0xb03060
    ; rgb "x-medium-violet-red"      0xc71585
    ; rgb "x-mediumvioletred"        0xc71585
    ; rgb "x-violet-red"             0xd02090
    ; rgb "x-violetred"              0xd02090
    ; rgb "x-magenta"                0xff00ff
    ; rgb "x-violet"                 0xee82ee
    ; rgb "x-plum"                   0xdda0dd
    ; rgb "x-orchid"                 0xda70d6
    ; rgb "x-medium-orchid"          0xba55d3
    ; rgb "x-mediumorchid"           0xba55d3
    ; rgb "x-dark-orchid"            0x9932cc
    ; rgb "x-darkorchid"             0x9932cc
    ; rgb "x-dark-violet"            0x9400d3
    ; rgb "x-darkviolet"             0x9400d3
    ; rgb "x-blue-violet"            0x8a2be2
    ; rgb "x-blueviolet"             0x8a2be2
    ; rgb "x-purple"                 0xa020f0
    ; rgb "x-medium-purple"          0x9370db
    ; rgb "x-mediumpurple"           0x9370db
    ; rgb "x-thistle"                0xd8bfd8
    ; rgb "x-snow1"                  0xfffafa
    ; rgb "x-snow2"                  0xeee9e9
    ; rgb "x-snow3"                  0xcdc9c9
    ; rgb "x-snow4"                  0x8b8989
    ; rgb "x-seashell1"              0xfff5ee
    ; rgb "x-seashell2"              0xeee5de
    ; rgb "x-seashell3"              0xcdc5bf
    ; rgb "x-seashell4"              0x8b8682
    ; rgb "x-antiquewhite1"          0xffefdb
    ; rgb "x-antiquewhite2"          0xeedfcc
    ; rgb "x-antiquewhite3"          0xcdc0b0
    ; rgb "x-antiquewhite4"          0x8b8378
    ; rgb "x-bisque1"                0xffe4c4
    ; rgb "x-bisque2"                0xeed5b7
    ; rgb "x-bisque3"                0xcdb79e
    ; rgb "x-bisque4"                0x8b7d6b
    ; rgb "x-peachpuff1"             0xffdab9
    ; rgb "x-peachpuff2"             0xeecbad
    ; rgb "x-peachpuff3"             0xcdaf95
    ; rgb "x-peachpuff4"             0x8b7765
    ; rgb "x-navajowhite1"           0xffdead
    ; rgb "x-navajowhite2"           0xeecfa1
    ; rgb "x-navajowhite3"           0xcdb38b
    ; rgb "x-navajowhite4"           0x8b795e
    ; rgb "x-lemonchiffon1"          0xfffacd
    ; rgb "x-lemonchiffon2"          0xeee9bf
    ; rgb "x-lemonchiffon3"          0xcdc9a5
    ; rgb "x-lemonchiffon4"          0x8b8970
    ; rgb "x-cornsilk1"              0xfff8dc
    ; rgb "x-cornsilk2"              0xeee8cd
    ; rgb "x-cornsilk3"              0xcdc8b1
    ; rgb "x-cornsilk4"              0x8b8878
    ; rgb "x-ivory1"                 0xfffff0
    ; rgb "x-ivory2"                 0xeeeee0
    ; rgb "x-ivory3"                 0xcdcdc1
    ; rgb "x-ivory4"                 0x8b8b83
    ; rgb "x-honeydew1"              0xf0fff0
    ; rgb "x-honeydew2"              0xe0eee0
    ; rgb "x-honeydew3"              0xc1cdc1
    ; rgb "x-honeydew4"              0x838b83
    ; rgb "x-lavenderblush1"         0xfff0f5
    ; rgb "x-lavenderblush2"         0xeee0e5
    ; rgb "x-lavenderblush3"         0xcdc1c5
    ; rgb "x-lavenderblush4"         0x8b8386
    ; rgb "x-mistyrose1"             0xffe4e1
    ; rgb "x-mistyrose2"             0xeed5d2
    ; rgb "x-mistyrose3"             0xcdb7b5
    ; rgb "x-mistyrose4"             0x8b7d7b
    ; rgb "x-azure1"                 0xf0ffff
    ; rgb "x-azure2"                 0xe0eeee
    ; rgb "x-azure3"                 0xc1cdcd
    ; rgb "x-azure4"                 0x838b8b
    ; rgb "x-slateblue1"             0x836fff
    ; rgb "x-slateblue2"             0x7a67ee
    ; rgb "x-slateblue3"             0x6959cd
    ; rgb "x-slateblue4"             0x473c8b
    ; rgb "x-royalblue1"             0x4876ff
    ; rgb "x-royalblue2"             0x436eee
    ; rgb "x-royalblue3"             0x3a5fcd
    ; rgb "x-royalblue4"             0x27408b
    ; rgb "x-blue1"                  0x0000ff
    ; rgb "x-blue2"                  0x0000ee
    ; rgb "x-blue3"                  0x0000cd
    ; rgb "x-blue4"                  0x00008b
    ; rgb "x-dodgerblue1"            0x1e90ff
    ; rgb "x-dodgerblue2"            0x1c86ee
    ; rgb "x-dodgerblue3"            0x1874cd
    ; rgb "x-dodgerblue4"            0x104e8b
    ; rgb "x-steelblue1"             0x63b8ff
    ; rgb "x-steelblue2"             0x5cacee
    ; rgb "x-steelblue3"             0x4f94cd
    ; rgb "x-steelblue4"             0x36648b
    ; rgb "x-deepskyblue1"           0x00bfff
    ; rgb "x-deepskyblue2"           0x00b2ee
    ; rgb "x-deepskyblue3"           0x009acd
    ; rgb "x-deepskyblue4"           0x00688b
    ; rgb "x-skyblue1"               0x87ceff
    ; rgb "x-skyblue2"               0x7ec0ee
    ; rgb "x-skyblue3"               0x6ca6cd
    ; rgb "x-skyblue4"               0x4a708b
    ; rgb "x-lightskyblue1"          0xb0e2ff
    ; rgb "x-lightskyblue2"          0xa4d3ee
    ; rgb "x-lightskyblue3"          0x8db6cd
    ; rgb "x-lightskyblue4"          0x607b8b
    ; rgb "x-slategray1"             0xc6e2ff
    ; rgb "x-slategray2"             0xb9d3ee
    ; rgb "x-slategray3"             0x9fb6cd
    ; rgb "x-slategray4"             0x6c7b8b
    ; rgb "x-lightsteelblue1"        0xcae1ff
    ; rgb "x-lightsteelblue2"        0xbcd2ee
    ; rgb "x-lightsteelblue3"        0xa2b5cd
    ; rgb "x-lightsteelblue4"        0x6e7b8b
    ; rgb "x-lightblue1"             0xbfefff
    ; rgb "x-lightblue2"             0xb2dfee
    ; rgb "x-lightblue3"             0x9ac0cd
    ; rgb "x-lightblue4"             0x68838b
    ; rgb "x-lightcyan1"             0xe0ffff
    ; rgb "x-lightcyan2"             0xd1eeee
    ; rgb "x-lightcyan3"             0xb4cdcd
    ; rgb "x-lightcyan4"             0x7a8b8b
    ; rgb "x-paleturquoise1"         0xbbffff
    ; rgb "x-paleturquoise2"         0xaeeeee
    ; rgb "x-paleturquoise3"         0x96cdcd
    ; rgb "x-paleturquoise4"         0x668b8b
    ; rgb "x-cadetblue1"             0x98f5ff
    ; rgb "x-cadetblue2"             0x8ee5ee
    ; rgb "x-cadetblue3"             0x7ac5cd
    ; rgb "x-cadetblue4"             0x53868b
    ; rgb "x-turquoise1"             0x00f5ff
    ; rgb "x-turquoise2"             0x00e5ee
    ; rgb "x-turquoise3"             0x00c5cd
    ; rgb "x-turquoise4"             0x00868b
    ; rgb "x-cyan1"                  0x00ffff
    ; rgb "x-cyan2"                  0x00eeee
    ; rgb "x-cyan3"                  0x00cdcd
    ; rgb "x-cyan4"                  0x008b8b
    ; rgb "x-darkslategray1"         0x97ffff
    ; rgb "x-darkslategray2"         0x8deeee
    ; rgb "x-darkslategray3"         0x79cdcd
    ; rgb "x-darkslategray4"         0x528b8b
    ; rgb "x-aquamarine1"            0x7fffd4
    ; rgb "x-aquamarine2"            0x76eec6
    ; rgb "x-aquamarine3"            0x66cdaa
    ; rgb "x-aquamarine4"            0x458b74
    ; rgb "x-darkseagreen1"          0xc1ffc1
    ; rgb "x-darkseagreen2"          0xb4eeb4
    ; rgb "x-darkseagreen3"          0x9bcd9b
    ; rgb "x-darkseagreen4"          0x698b69
    ; rgb "x-seagreen1"              0x54ff9f
    ; rgb "x-seagreen2"              0x4eee94
    ; rgb "x-seagreen3"              0x43cd80
    ; rgb "x-seagreen4"              0x2e8b57
    ; rgb "x-palegreen1"             0x9aff9a
    ; rgb "x-palegreen2"             0x90ee90
    ; rgb "x-palegreen3"             0x7ccd7c
    ; rgb "x-palegreen4"             0x548b54
    ; rgb "x-springgreen1"           0x00ff7f
    ; rgb "x-springgreen2"           0x00ee76
    ; rgb "x-springgreen3"           0x00cd66
    ; rgb "x-springgreen4"           0x008b45
    ; rgb "x-green1"                 0x00ff00
    ; rgb "x-green2"                 0x00ee00
    ; rgb "x-green3"                 0x00cd00
    ; rgb "x-green4"                 0x008b00
    ; rgb "x-chartreuse1"            0x7fff00
    ; rgb "x-chartreuse2"            0x76ee00
    ; rgb "x-chartreuse3"            0x66cd00
    ; rgb "x-chartreuse4"            0x458b00
    ; rgb "x-olivedrab1"             0xc0ff3e
    ; rgb "x-olivedrab2"             0xb3ee3a
    ; rgb "x-olivedrab3"             0x9acd32
    ; rgb "x-olivedrab4"             0x698b22
    ; rgb "x-darkolivegreen1"        0xcaff70
    ; rgb "x-darkolivegreen2"        0xbcee68
    ; rgb "x-darkolivegreen3"        0xa2cd5a
    ; rgb "x-darkolivegreen4"        0x6e8b3d
    ; rgb "x-khaki1"                 0xfff68f
    ; rgb "x-khaki2"                 0xeee685
    ; rgb "x-khaki3"                 0xcdc673
    ; rgb "x-khaki4"                 0x8b864e
    ; rgb "x-lightgoldenrod1"        0xffec8b
    ; rgb "x-lightgoldenrod2"        0xeedc82
    ; rgb "x-lightgoldenrod3"        0xcdbe70
    ; rgb "x-lightgoldenrod4"        0x8b814c
    ; rgb "x-lightyellow1"           0xffffe0
    ; rgb "x-lightyellow2"           0xeeeed1
    ; rgb "x-lightyellow3"           0xcdcdb4
    ; rgb "x-lightyellow4"           0x8b8b7a
    ; rgb "x-yellow1"                0xffff00
    ; rgb "x-yellow2"                0xeeee00
    ; rgb "x-yellow3"                0xcdcd00
    ; rgb "x-yellow4"                0x8b8b00
    ; rgb "x-gold1"                  0xffd700
    ; rgb "x-gold2"                  0xeec900
    ; rgb "x-gold3"                  0xcdad00
    ; rgb "x-gold4"                  0x8b7500
    ; rgb "x-goldenrod1"             0xffc125
    ; rgb "x-goldenrod2"             0xeeb422
    ; rgb "x-goldenrod3"             0xcd9b1d
    ; rgb "x-goldenrod4"             0x8b6914
    ; rgb "x-darkgoldenrod1"         0xffb90f
    ; rgb "x-darkgoldenrod2"         0xeead0e
    ; rgb "x-darkgoldenrod3"         0xcd950c
    ; rgb "x-darkgoldenrod4"         0x8b6508
    ; rgb "x-rosybrown1"             0xffc1c1
    ; rgb "x-rosybrown2"             0xeeb4b4
    ; rgb "x-rosybrown3"             0xcd9b9b
    ; rgb "x-rosybrown4"             0x8b6969
    ; rgb "x-indianred1"             0xff6a6a
    ; rgb "x-indianred2"             0xee6363
    ; rgb "x-indianred3"             0xcd5555
    ; rgb "x-indianred4"             0x8b3a3a
    ; rgb "x-sienna1"                0xff8247
    ; rgb "x-sienna2"                0xee7942
    ; rgb "x-sienna3"                0xcd6839
    ; rgb "x-sienna4"                0x8b4726
    ; rgb "x-burlywood1"             0xffd39b
    ; rgb "x-burlywood2"             0xeec591
    ; rgb "x-burlywood3"             0xcdaa7d
    ; rgb "x-burlywood4"             0x8b7355
    ; rgb "x-wheat1"                 0xffe7ba
    ; rgb "x-wheat2"                 0xeed8ae
    ; rgb "x-wheat3"                 0xcdba96
    ; rgb "x-wheat4"                 0x8b7e66
    ; rgb "x-tan1"                   0xffa54f
    ; rgb "x-tan2"                   0xee9a49
    ; rgb "x-tan3"                   0xcd853f
    ; rgb "x-tan4"                   0x8b5a2b
    ; rgb "x-chocolate1"             0xff7f24
    ; rgb "x-chocolate2"             0xee7621
    ; rgb "x-chocolate3"             0xcd661d
    ; rgb "x-chocolate4"             0x8b4513
    ; rgb "x-firebrick1"             0xff3030
    ; rgb "x-firebrick2"             0xee2c2c
    ; rgb "x-firebrick3"             0xcd2626
    ; rgb "x-firebrick4"             0x8b1a1a
    ; rgb "x-brown1"                 0xff4040
    ; rgb "x-brown2"                 0xee3b3b
    ; rgb "x-brown3"                 0xcd3333
    ; rgb "x-brown4"                 0x8b2323
    ; rgb "x-salmon1"                0xff8c69
    ; rgb "x-salmon2"                0xee8262
    ; rgb "x-salmon3"                0xcd7054
    ; rgb "x-salmon4"                0x8b4c39
    ; rgb "x-lightsalmon1"           0xffa07a
    ; rgb "x-lightsalmon2"           0xee9572
    ; rgb "x-lightsalmon3"           0xcd8162
    ; rgb "x-lightsalmon4"           0x8b5742
    ; rgb "x-orange1"                0xffa500
    ; rgb "x-orange2"                0xee9a00
    ; rgb "x-orange3"                0xcd8500
    ; rgb "x-orange4"                0x8b5a00
    ; rgb "x-darkorange1"            0xff7f00
    ; rgb "x-darkorange2"            0xee7600
    ; rgb "x-darkorange3"            0xcd6600
    ; rgb "x-darkorange4"            0x8b4500
    ; rgb "x-coral1"                 0xff7256
    ; rgb "x-coral2"                 0xee6a50
    ; rgb "x-coral3"                 0xcd5b45
    ; rgb "x-coral4"                 0x8b3e2f
    ; rgb "x-tomato1"                0xff6347
    ; rgb "x-tomato2"                0xee5c42
    ; rgb "x-tomato3"                0xcd4f39
    ; rgb "x-tomato4"                0x8b3626
    ; rgb "x-orangered1"             0xff4500
    ; rgb "x-orangered2"             0xee4000
    ; rgb "x-orangered3"             0xcd3700
    ; rgb "x-orangered4"             0x8b2500
    ; rgb "x-red1"                   0xff0000
    ; rgb "x-red2"                   0xee0000
    ; rgb "x-red3"                   0xcd0000
    ; rgb "x-red4"                   0x8b0000
    ; rgb "x-debianred"              0xd70751
    ; rgb "x-deeppink1"              0xff1493
    ; rgb "x-deeppink2"              0xee1289
    ; rgb "x-deeppink3"              0xcd1076
    ; rgb "x-deeppink4"              0x8b0a50
    ; rgb "x-hotpink1"               0xff6eb4
    ; rgb "x-hotpink2"               0xee6aa7
    ; rgb "x-hotpink3"               0xcd6090
    ; rgb "x-hotpink4"               0x8b3a62
    ; rgb "x-pink1"                  0xffb5c5
    ; rgb "x-pink2"                  0xeea9b8
    ; rgb "x-pink3"                  0xcd919e
    ; rgb "x-pink4"                  0x8b636c
    ; rgb "x-lightpink1"             0xffaeb9
    ; rgb "x-lightpink2"             0xeea2ad
    ; rgb "x-lightpink3"             0xcd8c95
    ; rgb "x-lightpink4"             0x8b5f65
    ; rgb "x-palevioletred1"         0xff82ab
    ; rgb "x-palevioletred2"         0xee799f
    ; rgb "x-palevioletred3"         0xcd6889
    ; rgb "x-palevioletred4"         0x8b475d
    ; rgb "x-maroon1"                0xff34b3
    ; rgb "x-maroon2"                0xee30a7
    ; rgb "x-maroon3"                0xcd2990
    ; rgb "x-maroon4"                0x8b1c62
    ; rgb "x-violetred1"             0xff3e96
    ; rgb "x-violetred2"             0xee3a8c
    ; rgb "x-violetred3"             0xcd3278
    ; rgb "x-violetred4"             0x8b2252
    ; rgb "x-magenta1"               0xff00ff
    ; rgb "x-magenta2"               0xee00ee
    ; rgb "x-magenta3"               0xcd00cd
    ; rgb "x-magenta4"               0x8b008b
    ; rgb "x-orchid1"                0xff83fa
    ; rgb "x-orchid2"                0xee7ae9
    ; rgb "x-orchid3"                0xcd69c9
    ; rgb "x-orchid4"                0x8b4789
    ; rgb "x-plum1"                  0xffbbff
    ; rgb "x-plum2"                  0xeeaeee
    ; rgb "x-plum3"                  0xcd96cd
    ; rgb "x-plum4"                  0x8b668b
    ; rgb "x-mediumorchid1"          0xe066ff
    ; rgb "x-mediumorchid2"          0xd15fee
    ; rgb "x-mediumorchid3"          0xb452cd
    ; rgb "x-mediumorchid4"          0x7a378b
    ; rgb "x-darkorchid1"            0xbf3eff
    ; rgb "x-darkorchid2"            0xb23aee
    ; rgb "x-darkorchid3"            0x9a32cd
    ; rgb "x-darkorchid4"            0x68228b
    ; rgb "x-purple1"                0x9b30ff
    ; rgb "x-purple2"                0x912cee
    ; rgb "x-purple3"                0x7d26cd
    ; rgb "x-purple4"                0x551a8b
    ; rgb "x-mediumpurple1"          0xab82ff
    ; rgb "x-mediumpurple2"          0x9f79ee
    ; rgb "x-mediumpurple3"          0x8968cd
    ; rgb "x-mediumpurple4"          0x5d478b
    ; rgb "x-thistle1"               0xffe1ff
    ; rgb "x-thistle2"               0xeed2ee
    ; rgb "x-thistle3"               0xcdb5cd
    ; rgb "x-thistle4"               0x8b7b8b
    ; rgb "x-gray0"                  0x000000
    ; rgb "x-grey0"                  0x000000
    ; rgb "x-gray1"                  0x030303
    ; rgb "x-grey1"                  0x030303
    ; rgb "x-gray2"                  0x050505
    ; rgb "x-grey2"                  0x050505
    ; rgb "x-gray3"                  0x080808
    ; rgb "x-grey3"                  0x080808
    ; rgb "x-gray4"                  0x0a0a0a
    ; rgb "x-grey4"                  0x0a0a0a
    ; rgb "x-gray5"                  0x0d0d0d
    ; rgb "x-grey5"                  0x0d0d0d
    ; rgb "x-gray6"                  0x0f0f0f
    ; rgb "x-grey6"                  0x0f0f0f
    ; rgb "x-gray7"                  0x121212
    ; rgb "x-grey7"                  0x121212
    ; rgb "x-gray8"                  0x141414
    ; rgb "x-grey8"                  0x141414
    ; rgb "x-gray9"                  0x171717
    ; rgb "x-grey9"                  0x171717
    ; rgb "x-gray10"                 0x1a1a1a
    ; rgb "x-grey10"                 0x1a1a1a
    ; rgb "x-gray11"                 0x1c1c1c
    ; rgb "x-grey11"                 0x1c1c1c
    ; rgb "x-gray12"                 0x1f1f1f
    ; rgb "x-grey12"                 0x1f1f1f
    ; rgb "x-gray13"                 0x212121
    ; rgb "x-grey13"                 0x212121
    ; rgb "x-gray14"                 0x242424
    ; rgb "x-grey14"                 0x242424
    ; rgb "x-gray15"                 0x262626
    ; rgb "x-grey15"                 0x262626
    ; rgb "x-gray16"                 0x292929
    ; rgb "x-grey16"                 0x292929
    ; rgb "x-gray17"                 0x2b2b2b
    ; rgb "x-grey17"                 0x2b2b2b
    ; rgb "x-gray18"                 0x2e2e2e
    ; rgb "x-grey18"                 0x2e2e2e
    ; rgb "x-gray19"                 0x303030
    ; rgb "x-grey19"                 0x303030
    ; rgb "x-gray20"                 0x333333
    ; rgb "x-grey20"                 0x333333
    ; rgb "x-gray21"                 0x363636
    ; rgb "x-grey21"                 0x363636
    ; rgb "x-gray22"                 0x383838
    ; rgb "x-grey22"                 0x383838
    ; rgb "x-gray23"                 0x3b3b3b
    ; rgb "x-grey23"                 0x3b3b3b
    ; rgb "x-gray24"                 0x3d3d3d
    ; rgb "x-grey24"                 0x3d3d3d
    ; rgb "x-gray25"                 0x404040
    ; rgb "x-grey25"                 0x404040
    ; rgb "x-gray26"                 0x424242
    ; rgb "x-grey26"                 0x424242
    ; rgb "x-gray27"                 0x454545
    ; rgb "x-grey27"                 0x454545
    ; rgb "x-gray28"                 0x474747
    ; rgb "x-grey28"                 0x474747
    ; rgb "x-gray29"                 0x4a4a4a
    ; rgb "x-grey29"                 0x4a4a4a
    ; rgb "x-gray30"                 0x4d4d4d
    ; rgb "x-grey30"                 0x4d4d4d
    ; rgb "x-gray31"                 0x4f4f4f
    ; rgb "x-grey31"                 0x4f4f4f
    ; rgb "x-gray32"                 0x525252
    ; rgb "x-grey32"                 0x525252
    ; rgb "x-gray33"                 0x545454
    ; rgb "x-grey33"                 0x545454
    ; rgb "x-gray34"                 0x575757
    ; rgb "x-grey34"                 0x575757
    ; rgb "x-gray35"                 0x595959
    ; rgb "x-grey35"                 0x595959
    ; rgb "x-gray36"                 0x5c5c5c
    ; rgb "x-grey36"                 0x5c5c5c
    ; rgb "x-gray37"                 0x5e5e5e
    ; rgb "x-grey37"                 0x5e5e5e
    ; rgb "x-gray38"                 0x616161
    ; rgb "x-grey38"                 0x616161
    ; rgb "x-gray39"                 0x636363
    ; rgb "x-grey39"                 0x636363
    ; rgb "x-gray40"                 0x666666
    ; rgb "x-grey40"                 0x666666
    ; rgb "x-gray41"                 0x696969
    ; rgb "x-grey41"                 0x696969
    ; rgb "x-gray42"                 0x6b6b6b
    ; rgb "x-grey42"                 0x6b6b6b
    ; rgb "x-gray43"                 0x6e6e6e
    ; rgb "x-grey43"                 0x6e6e6e
    ; rgb "x-gray44"                 0x707070
    ; rgb "x-grey44"                 0x707070
    ; rgb "x-gray45"                 0x737373
    ; rgb "x-grey45"                 0x737373
    ; rgb "x-gray46"                 0x757575
    ; rgb "x-grey46"                 0x757575
    ; rgb "x-gray47"                 0x787878
    ; rgb "x-grey47"                 0x787878
    ; rgb "x-gray48"                 0x7a7a7a
    ; rgb "x-grey48"                 0x7a7a7a
    ; rgb "x-gray49"                 0x7d7d7d
    ; rgb "x-grey49"                 0x7d7d7d
    ; rgb "x-gray50"                 0x7f7f7f
    ; rgb "x-grey50"                 0x7f7f7f
    ; rgb "x-gray51"                 0x828282
    ; rgb "x-grey51"                 0x828282
    ; rgb "x-gray52"                 0x858585
    ; rgb "x-grey52"                 0x858585
    ; rgb "x-gray53"                 0x878787
    ; rgb "x-grey53"                 0x878787
    ; rgb "x-gray54"                 0x8a8a8a
    ; rgb "x-grey54"                 0x8a8a8a
    ; rgb "x-gray55"                 0x8c8c8c
    ; rgb "x-grey55"                 0x8c8c8c
    ; rgb "x-gray56"                 0x8f8f8f
    ; rgb "x-grey56"                 0x8f8f8f
    ; rgb "x-gray57"                 0x919191
    ; rgb "x-grey57"                 0x919191
    ; rgb "x-gray58"                 0x949494
    ; rgb "x-grey58"                 0x949494
    ; rgb "x-gray59"                 0x969696
    ; rgb "x-grey59"                 0x969696
    ; rgb "x-gray60"                 0x999999
    ; rgb "x-grey60"                 0x999999
    ; rgb "x-gray61"                 0x9c9c9c
    ; rgb "x-grey61"                 0x9c9c9c
    ; rgb "x-gray62"                 0x9e9e9e
    ; rgb "x-grey62"                 0x9e9e9e
    ; rgb "x-gray63"                 0xa1a1a1
    ; rgb "x-grey63"                 0xa1a1a1
    ; rgb "x-gray64"                 0xa3a3a3
    ; rgb "x-grey64"                 0xa3a3a3
    ; rgb "x-gray65"                 0xa6a6a6
    ; rgb "x-grey65"                 0xa6a6a6
    ; rgb "x-gray66"                 0xa8a8a8
    ; rgb "x-grey66"                 0xa8a8a8
    ; rgb "x-gray67"                 0xababab
    ; rgb "x-grey67"                 0xababab
    ; rgb "x-gray68"                 0xadadad
    ; rgb "x-grey68"                 0xadadad
    ; rgb "x-gray69"                 0xb0b0b0
    ; rgb "x-grey69"                 0xb0b0b0
    ; rgb "x-gray70"                 0xb3b3b3
    ; rgb "x-grey70"                 0xb3b3b3
    ; rgb "x-gray71"                 0xb5b5b5
    ; rgb "x-grey71"                 0xb5b5b5
    ; rgb "x-gray72"                 0xb8b8b8
    ; rgb "x-grey72"                 0xb8b8b8
    ; rgb "x-gray73"                 0xbababa
    ; rgb "x-grey73"                 0xbababa
    ; rgb "x-gray74"                 0xbdbdbd
    ; rgb "x-grey74"                 0xbdbdbd
    ; rgb "x-gray75"                 0xbfbfbf
    ; rgb "x-grey75"                 0xbfbfbf
    ; rgb "x-gray76"                 0xc2c2c2
    ; rgb "x-grey76"                 0xc2c2c2
    ; rgb "x-gray77"                 0xc4c4c4
    ; rgb "x-grey77"                 0xc4c4c4
    ; rgb "x-gray78"                 0xc7c7c7
    ; rgb "x-grey78"                 0xc7c7c7
    ; rgb "x-gray79"                 0xc9c9c9
    ; rgb "x-grey79"                 0xc9c9c9
    ; rgb "x-gray80"                 0xcccccc
    ; rgb "x-grey80"                 0xcccccc
    ; rgb "x-gray81"                 0xcfcfcf
    ; rgb "x-grey81"                 0xcfcfcf
    ; rgb "x-gray82"                 0xd1d1d1
    ; rgb "x-grey82"                 0xd1d1d1
    ; rgb "x-gray83"                 0xd4d4d4
    ; rgb "x-grey83"                 0xd4d4d4
    ; rgb "x-gray84"                 0xd6d6d6
    ; rgb "x-grey84"                 0xd6d6d6
    ; rgb "x-gray85"                 0xd9d9d9
    ; rgb "x-grey85"                 0xd9d9d9
    ; rgb "x-gray86"                 0xdbdbdb
    ; rgb "x-grey86"                 0xdbdbdb
    ; rgb "x-gray87"                 0xdedede
    ; rgb "x-grey87"                 0xdedede
    ; rgb "x-gray88"                 0xe0e0e0
    ; rgb "x-grey88"                 0xe0e0e0
    ; rgb "x-gray89"                 0xe3e3e3
    ; rgb "x-grey89"                 0xe3e3e3
    ; rgb "x-gray90"                 0xe5e5e5
    ; rgb "x-grey90"                 0xe5e5e5
    ; rgb "x-gray91"                 0xe8e8e8
    ; rgb "x-grey91"                 0xe8e8e8
    ; rgb "x-gray92"                 0xebebeb
    ; rgb "x-grey92"                 0xebebeb
    ; rgb "x-gray93"                 0xededed
    ; rgb "x-grey93"                 0xededed
    ; rgb "x-gray94"                 0xf0f0f0
    ; rgb "x-grey94"                 0xf0f0f0
    ; rgb "x-gray95"                 0xf2f2f2
    ; rgb "x-grey95"                 0xf2f2f2
    ; rgb "x-gray96"                 0xf5f5f5
    ; rgb "x-grey96"                 0xf5f5f5
    ; rgb "x-gray97"                 0xf7f7f7
    ; rgb "x-grey97"                 0xf7f7f7
    ; rgb "x-gray98"                 0xfafafa
    ; rgb "x-grey98"                 0xfafafa
    ; rgb "x-gray99"                 0xfcfcfc
    ; rgb "x-grey99"                 0xfcfcfc
    ; rgb "x-gray100"                0xffffff
    ; rgb "x-grey100"                0xffffff
    ; rgb "x-dark-grey"              0xa9a9a9
    ; rgb "x-darkgrey"               0xa9a9a9
    ; rgb "x-dark-gray"              0xa9a9a9
    ; rgb "x-darkgray"               0xa9a9a9
    ; rgb "x-dark-blue"              0x00008b
    ; rgb "x-darkblue"               0x00008b
    ; rgb "x-dark-cyan"              0x008b8b
    ; rgb "x-darkcyan"               0x008b8b
    ; rgb "x-dark-magenta"           0x8b008b
    ; rgb "x-darkmagenta"            0x8b008b
    ; rgb "x-dark-red"               0x8b0000
    ; rgb "x-darkred"                0x8b0000
    ; rgb "x-light-green"            0x90ee90
    ; rgb "x-lightgreen"             0x90ee90
    ]

  let color_by_name =
    let table = Hashtbl.create (List.length color_names) in
    List.iter color_names ~f:(fun (name, t) ->
      Hashtbl.add table name t);
    table
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
    | RGB         -> Printf.sprintf "#%06x" t
  ;;

  let hex_of_char ch =
    match ch with
    | '0' .. '9' -> Char.code ch - Char.code '0'
    | 'A' .. 'F' -> Char.code ch - Char.code 'A' + 10
    | 'a' .. 'f' -> Char.code ch - Char.code 'a' + 10
    | _ -> raise_notrace Exit

  let of_string_error () =
    failwith "LTerm_style.Color.of_string"
  ;;

  let of_string = function
    | "" -> of_string_error ()
    | s  ->
      if s.[0] = '#' && String.length s = 7 then
        try
          rgb
            ((hex_of_char s.[1] lsl 4) lor hex_of_char s.[2])
            ((hex_of_char s.[3] lsl 4) lor hex_of_char s.[4])
            ((hex_of_char s.[5] lsl 4) lor hex_of_char s.[6])
        with _ ->
          of_string_error ()
      else
        try
          index (int_of_string s)
        with _ ->
          match Hashtbl.find color_by_name (String.lowercase s) with
          | c -> c
          | exception Not_found -> of_string_error ()
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
      let n = map.count_g * (Char.code (String.unsafe_get map.index_b b)    ) in
      let n = map.count_r * (Char.code (String.unsafe_get map.index_g g) + n) in
      let n =               (Char.code (String.unsafe_get map.index_r r) + n) in
      Char.code (String.unsafe_get map.map n)
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

module type Int = sig
  type t
  val zero : t
  val one : t
  val to_int : t -> int
  val of_int : int -> t
  val shift_left : t -> int -> t
  val shift_right_logical : t -> int -> t
  val ( - ) : t -> t -> t
  val equal : t -> t -> bool
  val logand : t -> t -> t
  val logor  : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
end

module Int : Int = struct
  type t = int
  let zero = 0
  let one = 1
  let to_int n = n
  let of_int n = n
  let shift_left = ( lsl )
  let shift_right_logical = ( lsr )
  let ( - ) a b = a - b
  let equal (a : t) b = a = b
  let logand = ( land )
  let logor = ( lor )
  let logxor = ( lxor )
  let lognot = lnot
end

module Int64 : Int = struct
  include Int64
  let (-) = sub
  let equal (a : t) b = a = b
end

module Int63 = (val if Sys.word_size = 64 then (module Int) else (module Int64) : Int)
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

let ( & ) = Int63.logand
let ( + ) = Int63.logor
let ( - ) t m = t & Int63.lognot m
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

let bold       t = Switch.of_int (Int63.to_int ((t lsr 50) & Int63.of_int 3))
let underline  t = Switch.of_int (Int63.to_int ((t lsr 52) & Int63.of_int 3))
let blink      t = Switch.of_int (Int63.to_int ((t lsr 54) & Int63.of_int 3))
let reverse    t = Switch.of_int (Int63.to_int ((t lsr 56) & Int63.of_int 3))
let foreground t = Int63.to_int  (t & Bits.foreground)
let background t = Int63.to_int ((t & Bits.background) lsr 25)

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
  let t = (a & b_unset_switches_mask) + (Int63.logxor b m) in
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
