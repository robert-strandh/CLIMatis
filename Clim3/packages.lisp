;;; Create the clim3-lisp package
(eval-when (:compile-toplevel :load-toplevel)
  (defpackage #:clim3-lisp)
  
  (do-external-symbols (symbol '#:common-lisp)
    ;; We use (list symbol) in order to get NIL as well.
    ;; Otherwise NIL would be interpreted as an empty list of symbols.
    (import (list symbol) '#:clim3-lisp))

  (let ((gray-symbols
         '(#:fundamental-stream
           #:fundamental-input-stream
           #:fundamental-output-stream
           #:fundamental-character-stream
           #:fundamental-binary-stream
           #:fundamental-character-input-stream
           #:fundamental-character-output-stream
           #:fundamental-binary-input-stream
           #:fundamental-binary-output-stream
           #:stream-read-char
           #:stream-unread-char
           #:stream-read-char-no-hang
           #:stream-peek-char
           #:stream-listen
           #:stream-read-line
           #:stream-clear-input
           #:stream-write-char
           #:stream-line-column
           #:stream-start-line-p
           #:stream-write-string
           #:stream-terpri
           #:stream-fresh-line
           #:stream-finish-output
           #:stream-force-output
           #:stream-advance-to-column
           #:stream-clear-output
           #:stream-read-byte
           #:stream-write-byte ))
        (gray-packages
         `(#+clisp                 ,@'(:gray)
           #+cmu                   ,@'(:ext)
           #+scl                   ,@'(:ext)
	   #+(or mcl openmcl)      ,@'(:ccl)
           #+allegro               ,@'(:common-lisp :excl :stream)
           #+harlequin-common-lisp ,@'(:stream)
           #+sbcl                  ,@'(:sb-gray))))
    (loop for symbol in gray-symbols
	  do (loop for package in gray-packages
		   do (let ((symbol (find-symbol (symbol-name symbol) package)))
			(unless (null symbol)
			  (import symbol '#:clim3-lisp)
			  (loop-finish))))))
  (do-symbols (symbol '#:clim3-lisp)
    ;; We use (list symbol) in order to get NIL as well.
    ;; Otherwise NIL would be interpreted as an empty list of symbols.
    (export (list symbol) '#:clim3-lisp)))

(defpackage #:clim3
    (:use #:clim3-lisp)
  (:export ;; Symbols related to ports
   #:port #:paint #:port-paint #:make-port
   #:port-ascent #:port-descent
   #:port-font-ascent #:port-font-descent
   #:port-text-width
   )
  (:export ;; Symbols related to zones
   #:zone #:atomic-zone #:compound-zone #:children
   #:baseline
   #:solid-zone #:image-zone #:monochrome-zone #:masked-zone #:mask
   #:add-zone #:delete-zone
   #:pos-x #:pos-y #:width #:height #:image-data #:parent
   #:read-image-file #:read-image-file-with-type
   #:update #:find-port #:update
   #:ascent #:descent #:font-ascent #:font-descent
   #:text-buffer-zone
   #:bboard
   #:stream-zone #:*font* #:*text-color*
   #:compute-vertical-elasticity #:compute-horizontal-elasticity
   #:impose-width #:impose-height
   #:text-zone #:add-text #:text
   #:vbox #:hbox
   #:very-elastic-zone #:very-rigid-zone
   ;; rename this one
   #:search-zone
   #:input-method-mixin #:input-method
   )
  (:export ;; Symbols related to elasticities
   #:elasticity #:make-general-elasticity #:make-elementary-elasticity
   #:force-at-size #:size-at-force #:serial-elasticities #:parallel-elasticities
   )
  (:export ;; Symbols related to events
   #:event #:device-event #:keyboard-event
   #:key-press-event #:key-release-event
   #:pointer-event #:pointer-button-event
   #:pointer-button-press-event #:pointer-button-release-event
   #:pointer-button-hold-event #:pointer-motion-event
   #:pointer-boundary-event #:pointer-enter-event #:pointer-exit-event
   #:modifier-state
   #:interpretations
   #:handle-event #:event-loop
   )
  (:export ;; Symbols related to color
   #:color #:colorp #:make-rgb-color #:make-lhs-color
   #:make-gray-color #:rgb #:lhs #:red #:blue #:green
   #:opacity #:pixel
   #:+snow+ #:+ghost-white+ #:+ghostwhite+ #:+white-smoke+
   #:+whitesmoke+ #:+gainsboro+ #:+floral-white+ #:+floralwhite+
   #:+old-lace+ #:+oldlace+ #:+linen+ #:+antique-white+
   #:+antiquewhite+ #:+papaya-whip+ #:+papayawhip+ #:+blanched-almond+
   #:+blanchedalmond+ #:+bisque+ #:+peach-puff+ #:+peachpuff+
   #:+navajo-white+ #:+navajowhite+ #:+moccasin+ #:+cornsilk+
   #:+ivory+ #:+lemon-chiffon+ #:+lemonchiffon+ #:+seashell+
   #:+honeydew+ #:+mint-cream+ #:+mintcream+ #:+azure+
   #:+alice-blue+ #:+aliceblue+ #:+lavender+ #:+lavender-blush+
   #:+lavenderblush+ #:+misty-rose+ #:+mistyrose+ #:+white+
   #:+black+ #:+dark-slate-gray+ #:+darkslategray+ #:+dark-slate-grey+
   #:+darkslategrey+ #:+dim-gray+ #:+dimgray+ #:+dim-grey+
   #:+dimgrey+ #:+slate-gray+ #:+slategray+ #:+slate-grey+
   #:+slategrey+ #:+light-slate-gray+ #:+lightslategray+ #:+light-slate-grey+
   #:+lightslategrey+ #:+gray+ #:+grey+ #:+light-grey+
   #:+lightgrey+ #:+light-gray+ #:+lightgray+ #:+midnight-blue+
   #:+midnightblue+ #:+navy+ #:+navy-blue+ #:+navyblue+
   #:+cornflower-blue+ #:+cornflowerblue+ #:+dark-slate-blue+ #:+darkslateblue+
   #:+slate-blue+ #:+slateblue+ #:+medium-slate-blue+ #:+mediumslateblue+
   #:+light-slate-blue+ #:+lightslateblue+ #:+medium-blue+ #:+mediumblue+
   #:+royal-blue+ #:+royalblue+ #:+blue+ #:+dodger-blue+
   #:+dodgerblue+ #:+deep-sky-blue+ #:+deepskyblue+ #:+sky-blue+
   #:+skyblue+ #:+light-sky-blue+ #:+lightskyblue+ #:+steel-blue+
   #:+steelblue+ #:+light-steel-blue+ #:+lightsteelblue+ #:+light-blue+
   #:+lightblue+ #:+powder-blue+ #:+powderblue+ #:+pale-turquoise+
   #:+paleturquoise+ #:+dark-turquoise+ #:+darkturquoise+ #:+medium-turquoise+
   #:+mediumturquoise+ #:+turquoise+ #:+cyan+ #:+light-cyan+
   #:+lightcyan+ #:+cadet-blue+ #:+cadetblue+ #:+medium-aquamarine+
   #:+mediumaquamarine+ #:+aquamarine+ #:+dark-green+ #:+darkgreen+
   #:+dark-olive-green+ #:+darkolivegreen+ #:+dark-sea-green+ #:+darkseagreen+
   #:+sea-green+ #:+seagreen+ #:+medium-sea-green+ #:+mediumseagreen+
   #:+light-sea-green+ #:+lightseagreen+ #:+pale-green+ #:+palegreen+
   #:+spring-green+ #:+springgreen+ #:+lawn-green+ #:+lawngreen+
   #:+green+ #:+chartreuse+ #:+medium-spring-green+ #:+mediumspringgreen+
   #:+green-yellow+ #:+greenyellow+ #:+lime-green+ #:+limegreen+
   #:+yellow-green+ #:+yellowgreen+ #:+forest-green+ #:+forestgreen+
   #:+olive-drab+ #:+olivedrab+ #:+dark-khaki+ #:+darkkhaki+
   #:+khaki+ #:+pale-goldenrod+ #:+palegoldenrod+ #:+light-goldenrod-yellow+
   #:+lightgoldenrodyellow+ #:+light-yellow+ #:+lightyellow+ #:+yellow+
   #:+gold+ #:+light-goldenrod+ #:+lightgoldenrod+ #:+goldenrod+
   #:+dark-goldenrod+ #:+darkgoldenrod+ #:+rosy-brown+ #:+rosybrown+
   #:+indian-red+ #:+indianred+ #:+saddle-brown+ #:+saddlebrown+
   #:+sienna+ #:+peru+ #:+burlywood+ #:+beige+
   #:+wheat+ #:+sandy-brown+ #:+sandybrown+ #:+tan+
   #:+chocolate+ #:+firebrick+ #:+brown+ #:+dark-salmon+
   #:+darksalmon+ #:+salmon+ #:+light-salmon+ #:+lightsalmon+
   #:+orange+ #:+dark-orange+ #:+darkorange+ #:+coral+
   #:+light-coral+ #:+lightcoral+ #:+tomato+ #:+orange-red+
   #:+orangered+ #:+red+ #:+hot-pink+ #:+hotpink+
   #:+deep-pink+ #:+deeppink+ #:+pink+ #:+light-pink+
   #:+lightpink+ #:+pale-violet-red+ #:+palevioletred+ #:+maroon+
   #:+medium-violet-red+ #:+mediumvioletred+ #:+violet-red+ #:+violetred+
   #:+magenta+ #:+violet+ #:+plum+ #:+orchid+
   #:+medium-orchid+ #:+mediumorchid+ #:+dark-orchid+ #:+darkorchid+
   #:+dark-violet+ #:+darkviolet+ #:+blue-violet+ #:+blueviolet+
   #:+purple+ #:+medium-purple+ #:+mediumpurple+ #:+thistle+
   #:+snow1+ #:+snow2+ #:+snow3+ #:+snow4+
   #:+seashell1+ #:+seashell2+ #:+seashell3+ #:+seashell4+
   #:+antiquewhite1+ #:+antiquewhite2+ #:+antiquewhite3+ #:+antiquewhite4+
   #:+bisque1+ #:+bisque2+ #:+bisque3+ #:+bisque4+
   #:+peachpuff1+ #:+peachpuff2+ #:+peachpuff3+ #:+peachpuff4+
   #:+navajowhite1+ #:+navajowhite2+ #:+navajowhite3+ #:+navajowhite4+
   #:+lemonchiffon1+ #:+lemonchiffon2+ #:+lemonchiffon3+ #:+lemonchiffon4+
   #:+cornsilk1+ #:+cornsilk2+ #:+cornsilk3+ #:+cornsilk4+
   #:+ivory1+ #:+ivory2+ #:+ivory3+ #:+ivory4+
   #:+honeydew1+ #:+honeydew2+ #:+honeydew3+ #:+honeydew4+
   #:+lavenderblush1+ #:+lavenderblush2+ #:+lavenderblush3+ #:+lavenderblush4+
   #:+mistyrose1+ #:+mistyrose2+ #:+mistyrose3+ #:+mistyrose4+
   #:+azure1+ #:+azure2+ #:+azure3+ #:+azure4+
   #:+slateblue1+ #:+slateblue2+ #:+slateblue3+ #:+slateblue4+
   #:+royalblue1+ #:+royalblue2+ #:+royalblue3+ #:+royalblue4+
   #:+blue1+ #:+blue2+ #:+blue3+ #:+blue4+
   #:+dodgerblue1+ #:+dodgerblue2+ #:+dodgerblue3+ #:+dodgerblue4+
   #:+steelblue1+ #:+steelblue2+ #:+steelblue3+ #:+steelblue4+
   #:+deepskyblue1+ #:+deepskyblue2+ #:+deepskyblue3+ #:+deepskyblue4+
   #:+skyblue1+ #:+skyblue2+ #:+skyblue3+ #:+skyblue4+
   #:+lightskyblue1+ #:+lightskyblue2+ #:+lightskyblue3+ #:+lightskyblue4+
   #:+slategray1+ #:+slategray2+ #:+slategray3+ #:+slategray4+
   #:+lightsteelblue1+ #:+lightsteelblue2+ #:+lightsteelblue3+ #:+lightsteelblue4+
   #:+lightblue1+ #:+lightblue2+ #:+lightblue3+ #:+lightblue4+
   #:+lightcyan1+ #:+lightcyan2+ #:+lightcyan3+ #:+lightcyan4+
   #:+paleturquoise1+ #:+paleturquoise2+ #:+paleturquoise3+ #:+paleturquoise4+
   #:+cadetblue1+ #:+cadetblue2+ #:+cadetblue3+ #:+cadetblue4+
   #:+turquoise1+ #:+turquoise2+ #:+turquoise3+ #:+turquoise4+
   #:+cyan1+ #:+cyan2+ #:+cyan3+ #:+cyan4+
   #:+darkslategray1+ #:+darkslategray2+ #:+darkslategray3+ #:+darkslategray4+
   #:+aquamarine1+ #:+aquamarine2+ #:+aquamarine3+ #:+aquamarine4+
   #:+darkseagreen1+ #:+darkseagreen2+ #:+darkseagreen3+ #:+darkseagreen4+
   #:+seagreen1+ #:+seagreen2+ #:+seagreen3+ #:+seagreen4+
   #:+palegreen1+ #:+palegreen2+ #:+palegreen3+ #:+palegreen4+
   #:+springgreen1+ #:+springgreen2+ #:+springgreen3+ #:+springgreen4+
   #:+green1+ #:+green2+ #:+green3+ #:+green4+
   #:+chartreuse1+ #:+chartreuse2+ #:+chartreuse3+ #:+chartreuse4+
   #:+olivedrab1+ #:+olivedrab2+ #:+olivedrab3+ #:+olivedrab4+
   #:+darkolivegreen1+ #:+darkolivegreen2+ #:+darkolivegreen3+ #:+darkolivegreen4+
   #:+khaki1+ #:+khaki2+ #:+khaki3+ #:+khaki4+
   #:+lightgoldenrod1+ #:+lightgoldenrod2+ #:+lightgoldenrod3+ #:+lightgoldenrod4+
   #:+lightyellow1+ #:+lightyellow2+ #:+lightyellow3+ #:+lightyellow4+
   #:+yellow1+ #:+yellow2+ #:+yellow3+ #:+yellow4+
   #:+gold1+ #:+gold2+ #:+gold3+ #:+gold4+
   #:+goldenrod1+ #:+goldenrod2+ #:+goldenrod3+ #:+goldenrod4+
   #:+darkgoldenrod1+ #:+darkgoldenrod2+ #:+darkgoldenrod3+ #:+darkgoldenrod4+
   #:+rosybrown1+ #:+rosybrown2+ #:+rosybrown3+ #:+rosybrown4+
   #:+indianred1+ #:+indianred2+ #:+indianred3+ #:+indianred4+
   #:+sienna1+ #:+sienna2+ #:+sienna3+ #:+sienna4+
   #:+burlywood1+ #:+burlywood2+ #:+burlywood3+ #:+burlywood4+
   #:+wheat1+ #:+wheat2+ #:+wheat3+ #:+wheat4+
   #:+tan1+ #:+tan2+ #:+tan3+ #:+tan4+
   #:+chocolate1+ #:+chocolate2+ #:+chocolate3+ #:+chocolate4+
   #:+firebrick1+ #:+firebrick2+ #:+firebrick3+ #:+firebrick4+
   #:+brown1+ #:+brown2+ #:+brown3+ #:+brown4+
   #:+salmon1+ #:+salmon2+ #:+salmon3+ #:+salmon4+
   #:+lightsalmon1+ #:+lightsalmon2+ #:+lightsalmon3+ #:+lightsalmon4+
   #:+orange1+ #:+orange2+ #:+orange3+ #:+orange4+
   #:+darkorange1+ #:+darkorange2+ #:+darkorange3+ #:+darkorange4+
   #:+coral1+ #:+coral2+ #:+coral3+ #:+coral4+
   #:+tomato1+ #:+tomato2+ #:+tomato3+ #:+tomato4+
   #:+orangered1+ #:+orangered2+ #:+orangered3+ #:+orangered4+
   #:+red1+ #:+red2+ #:+red3+ #:+red4+
   #:+deeppink1+ #:+deeppink2+ #:+deeppink3+ #:+deeppink4+
   #:+hotpink1+ #:+hotpink2+ #:+hotpink3+ #:+hotpink4+
   #:+pink1+ #:+pink2+ #:+pink3+ #:+pink4+
   #:+lightpink1+ #:+lightpink2+ #:+lightpink3+ #:+lightpink4+
   #:+palevioletred1+ #:+palevioletred2+ #:+palevioletred3+ #:+palevioletred4+
   #:+maroon1+ #:+maroon2+ #:+maroon3+ #:+maroon4+
   #:+violetred1+ #:+violetred2+ #:+violetred3+ #:+violetred4+
   #:+magenta1+ #:+magenta2+ #:+magenta3+ #:+magenta4+
   #:+orchid1+ #:+orchid2+ #:+orchid3+ #:+orchid4+
   #:+plum1+ #:+plum2+ #:+plum3+ #:+plum4+
   #:+mediumorchid1+ #:+mediumorchid2+ #:+mediumorchid3+ #:+mediumorchid4+
   #:+darkorchid1+ #:+darkorchid2+ #:+darkorchid3+ #:+darkorchid4+
   #:+purple1+ #:+purple2+ #:+purple3+ #:+purple4+
   #:+mediumpurple1+ #:+mediumpurple2+ #:+mediumpurple3+ #:+mediumpurple4+
   #:+thistle1+ #:+thistle2+ #:+thistle3+ #:+thistle4+
   #:+gray0+ #:+grey0+ #:+gray1+ #:+grey1+
   #:+gray2+ #:+grey2+ #:+gray3+ #:+grey3+
   #:+gray4+ #:+grey4+ #:+gray5+ #:+grey5+
   #:+gray6+ #:+grey6+ #:+gray7+ #:+grey7+
   #:+gray8+ #:+grey8+ #:+gray9+ #:+grey9+
   #:+gray10+ #:+grey10+ #:+gray11+ #:+grey11+
   #:+gray12+ #:+grey12+ #:+gray13+ #:+grey13+
   #:+gray14+ #:+grey14+ #:+gray15+ #:+grey15+
   #:+gray16+ #:+grey16+ #:+gray17+ #:+grey17+
   #:+gray18+ #:+grey18+ #:+gray19+ #:+grey19+
   #:+gray20+ #:+grey20+ #:+gray21+ #:+grey21+
   #:+gray22+ #:+grey22+ #:+gray23+ #:+grey23+
   #:+gray24+ #:+grey24+ #:+gray25+ #:+grey25+
   #:+gray26+ #:+grey26+ #:+gray27+ #:+grey27+
   #:+gray28+ #:+grey28+ #:+gray29+ #:+grey29+
   #:+gray30+ #:+grey30+ #:+gray31+ #:+grey31+
   #:+gray32+ #:+grey32+ #:+gray33+ #:+grey33+
   #:+gray34+ #:+grey34+ #:+gray35+ #:+grey35+
   #:+gray36+ #:+grey36+ #:+gray37+ #:+grey37+
   #:+gray38+ #:+grey38+ #:+gray39+ #:+grey39+
   #:+gray40+ #:+grey40+ #:+gray41+ #:+grey41+
   #:+gray42+ #:+grey42+ #:+gray43+ #:+grey43+
   #:+gray44+ #:+grey44+ #:+gray45+ #:+grey45+
   #:+gray46+ #:+grey46+ #:+gray47+ #:+grey47+
   #:+gray48+ #:+grey48+ #:+gray49+ #:+grey49+
   #:+gray50+ #:+grey50+ #:+gray51+ #:+grey51+
   #:+gray52+ #:+grey52+ #:+gray53+ #:+grey53+
   #:+gray54+ #:+grey54+ #:+gray55+ #:+grey55+
   #:+gray56+ #:+grey56+ #:+gray57+ #:+grey57+
   #:+gray58+ #:+grey58+ #:+gray59+ #:+grey59+
   #:+gray60+ #:+grey60+ #:+gray61+ #:+grey61+
   #:+gray62+ #:+grey62+ #:+gray63+ #:+grey63+
   #:+gray64+ #:+grey64+ #:+gray65+ #:+grey65+
   #:+gray66+ #:+grey66+ #:+gray67+ #:+grey67+
   #:+gray68+ #:+grey68+ #:+gray69+ #:+grey69+
   #:+gray70+ #:+grey70+ #:+gray71+ #:+grey71+
   #:+gray72+ #:+grey72+ #:+gray73+ #:+grey73+
   #:+gray74+ #:+grey74+ #:+gray75+ #:+grey75+
   #:+gray76+ #:+grey76+ #:+gray77+ #:+grey77+
   #:+gray78+ #:+grey78+ #:+gray79+ #:+grey79+
   #:+gray80+ #:+grey80+ #:+gray81+ #:+grey81+
   #:+gray82+ #:+grey82+ #:+gray83+ #:+grey83+
   #:+gray84+ #:+grey84+ #:+gray85+ #:+grey85+
   #:+gray86+ #:+grey86+ #:+gray87+ #:+grey87+
   #:+gray88+ #:+grey88+ #:+gray89+ #:+grey89+
   #:+gray90+ #:+grey90+ #:+gray91+ #:+grey91+
   #:+gray92+ #:+grey92+ #:+gray93+ #:+grey93+
   #:+gray94+ #:+grey94+ #:+gray95+ #:+grey95+
   #:+gray96+ #:+grey96+ #:+gray97+ #:+grey97+
   #:+gray98+ #:+grey98+ #:+gray99+ #:+grey99+
   #:+gray100+ #:+grey100+ #:+dark-grey+ #:+darkgrey+
   #:+dark-gray+ #:+darkgray+ #:+dark-blue+ #:+darkblue+
   #:+dark-cyan+ #:+darkcyan+ #:+dark-magenta+ #:+darkmagenta+
   #:+dark-red+ #:+darkred+ #:+light-green+ #:+lightgreen+
   )
  (:export ; Symbols related to text styles
   #:text-style #:make-text-style #:family #:face #:size)
  (:export ; Symbols related to input methods
   #:input-method #:character-input-method
   #:+shift-modifier-mask+ #:+control-modifier-mask+ #:+meta-modifier-mask+
   #:immediate-accumulate-mixin #:multi-keystroke-input-method-mixin
   #:latin-1-prefix-input-method
   #:manage-event #:gesture-available-p #:next-gesture
   #:accumulate-gesture #:queue-gesture
   )
  (:export ;Symbols related to commands
   #:define-command #:*unsupplied-argument-marker*
   )
  )

(defpackage #:clim3-port
    (:use #:clim3-lisp #:clim3)
  (:export))

(defpackage #:clim3-elasticity
    (:use #:clim3-lisp #:clim3)
  (:export))

(defpackage #:clim3-event
    (:use #:clim3-lisp #:clim3)
  (:export))

(defpackage #:clim3-zone
    (:use #:clim3-lisp #:clim3)
  (:export))

(defpackage #:clim3-text-style
    (:use #:clim3-lisp #:clim3)
  (:export))

(defpackage #:clim3-menu
    (:use #:clim3-lisp #:clim3)
  (:export))

(defpackage #:clim3-input-method
    (:use #:clim3-lisp #:clim3)
  (:export))

(defpackage #:clim3-command
    (:use #:clim3-lisp #:clim3)
  (:export))
