(define-minor-mode cgl-mode
  "CGL mode defines a few key bindings for use in the CGL buffer"
  nil
  "GAME OF LIFE"
  '(("s" . cgl-step)
    ("r" . cgl-start)
    ("q" . kill-current-buffer)
    ("g" . cgl-go-pause)
    ("e" . (lambda () (interactive) (setf cgl-earth-mode (not cgl-earth-mode))))))
