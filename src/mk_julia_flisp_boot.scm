(set! lastpwd (path.cwd))
(path.cwd (cadr *argv*))
(load (caddr *argv*))
(path.cwd lastpwd)
(make-system-image (cadddr *argv*))
