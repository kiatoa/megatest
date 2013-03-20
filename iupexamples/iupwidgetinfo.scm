#! /usr/bin/env csi

(require-library srfi-4 iup)
(import srfi-4 iup iup-pplot iup-glcanvas)

(define (popup dlg . args)
  (apply show dlg #:modal? 'yes args)
  (destroy! dlg))

(define (properties ih)
  (popup (element-properties-dialog ih))
  'default)

(define dlg
  (dialog
    (vbox
      (hbox ; headline
        (fill)
        (frame (label " Inspect control and dialog classes "
                      fontsize: 15))
        (fill)
        margin: '0x0)

      (label "") 
      (label "Dialogs" fontsize: 12)
      (hbox
        (button "dialog"
                action: (lambda (self) (properties (dialog (vbox)))))
        (button "color-dialog"
                action: (lambda (self) (properties (color-dialog))))
        (button "file-dialog"
                action: (lambda (self) (properties (file-dialog))))
        (button "font-dialog"
                action: (lambda (self) (properties (font-dialog))))
        (button "message-dialog"
                action: (lambda (self) (properties (message-dialog))))
        (fill)
        margin: '0x0)
      (hbox
        (button "layout-dialog"
                action: (lambda (self) (properties (layout-dialog))))
        (button "element-properties-dialog"
                action: (lambda (self)
                          (properties
                            (element-properties-dialog (create 'user)))))
        (fill)
        margin: '0x0)

      (label "") 
      (label "Composition widgets" fontsize: 12)
      (hbox
        (button "fill"
                action: (lambda (self) (properties (fill))))
        (button "hbox"
                action: (lambda (self) (properties (hbox))))
        (button "vbox"
                action: (lambda (self) (properties (vbox))))
        (button "zbox"
                action: (lambda (self) (properties (zbox))))
        (button "radio"
                action: (lambda (self) (properties (radio (vbox)))))
        (button "normalizer"
                action: (lambda (self) (properties (normalizer))))
        (button "cbox"
                action: (lambda (self) (properties (cbox))))
        (button "sbox"
                action: (lambda (self) (properties (sbox (vbox)))))
        (button "split"
                action: (lambda (self) (properties (split (vbox) (vbox)))))
        (fill)
        margin: '0x0)

      (label "") 
      (label "Standard widgets" fontsize: 12)
      (hbox
        (button "button"
                action: (lambda (self) (properties (button))))
        (button "canvas"
                action: (lambda (self) (properties (canvas))))
        (button "frame"
                action: (lambda (self) (properties (frame))))
        (button "label"
                action: (lambda (self) (properties (label))))
        (button "listbox"
                action: (lambda (self) (properties (listbox))))
        (button "progress-bar"
                action: (lambda (self) (properties (progress-bar))))
        (button "spin"
                action: (lambda (self) (properties (spin))))
        (fill)
        margin: '0x0)
      (hbox
        (button "tabs"
                action: (lambda (self) (properties (tabs))))
        (button "textbox"
                action: (lambda (self) (properties (textbox))))
        (button "toggle"
                action: (lambda (self) (properties (toggle))))
        (button "treebox"
                action: (lambda (self) (properties (treebox))))
        (button "valuator"
                action: (lambda (self) (properties (valuator ""))))
        (fill)
        margin: '0x0)

      (label "") 
      (label "Additional widgets" fontsize: 12)
      (hbox
        (button "cells"
                action: (lambda (self) (properties (cells))))
        (button "color-bar"
                action: (lambda (self) (properties (color-bar))))
        (button "color-browser"
                action: (lambda (self) (properties (color-browser))))
        (button "dial"
                action: (lambda (self) (properties (dial ""))))
        (button "matrix"
                action: (lambda (self) (properties (matrix))))
        (fill)
        margin: '0x0)
      (hbox
        (button "pplot"
                action: (lambda (self) (properties (pplot))))
        (button "glcanvas"
                action: (lambda (self) (properties (glcanvas))))
        (button "web-browser"
                action: (lambda (self) (properties (web-browser))))
        (fill)
        margin: '0x0)

      (label "") 
      (label "Menu widgets" fontsize: 12)
      (hbox
        (button "menu"
                action: (lambda (self) (properties (menu))))
        (button "menu-item"
                action: (lambda (self) (properties (menu-item))))
        (button "menu-separator"
                action: (lambda (self) (properties (menu-separator))))
        (fill)
        margin: '0x0)

      (label "") 
      (label "Images" fontsize: 12)
      (hbox
        (button "image/palette"
                action: (lambda (self)
                          (properties
                            (image/palette 1 1 (u8vector->blob (u8vector 0))))))
        (button "image/rgb"
                action: (lambda (self)
                          (properties
                            (image/rgb 1 1 (u8vector->blob (u8vector 0))))))
        (button "image/rgba"
                action: (lambda (self)
                          (properties
                            (image/rgba 1 1 (u8vector->blob (u8vector 0))))))
        (button "image/file"
                action: (lambda (self)
                          (properties
                            ;; same attributes as image/palette
                            (image/palette 1 1 (u8vector->blob (u8vector 0))))))
                            ;; needs a file in current directory
                            ;(image/file "chicken.ico")))) ; ok
                            ;(image/file "chicken.png")))) ; doesn't work
        (fill)
        margin: '0x0)

      (label "") 
      (label "Other widgets" fontsize: 12)
      (hbox
        (button "clipboard"
                action: (lambda (self) (properties (clipboard))))
        (button "timer"
                action: (lambda (self) (properties (timer))))
        (button "spinbox"
                action: (lambda (self) (properties (spinbox (vbox)))))
        (fill)
        margin: '0x0)

      (fill)
      (button "E&xit"
              expand: 'horizontal
              action: (lambda (self) 'close))
      )
    margin: '15x15
    title: "Iup inspector"))

(show dlg)
(main-loop)
(exit 0)
