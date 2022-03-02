;;; elsarticle.el --- AUCTeX style for `elsarticle.cls'

(TeX-add-style-hook
 "elsarticle"
 (lambda ()
   (TeX-run-style-hooks "natbib" "graphicx" "geometry")))
