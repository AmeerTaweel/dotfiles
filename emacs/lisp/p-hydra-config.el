;-------------------------------------------------------------------------------
; # HYDRA - Plugin Configuration
;-------------------------------------------------------------------------------

(defhydra hydra-scale-text ()
  "Scale text."
  ("k" text-scale-increase "increase text size")
  ("j" text-scale-decrease "decrease text size"))

(defhydra hydra-scale-window ()
  "Scale window."
  ("l" enlarge-window-horizontally "+ column")
  ("k" enlarge-window "+ row")
  ("j" shrink-window "- row")
  ("h" shrink-window-horizontally "- column"))
