
(in-package :zgnuplot)

;;<<>>=
(defun determine-minimum-arity (fn &optional (ranges (tb:roll-list '((.1 1))))
                                             (maximum-arity-tried 10))
  (iter (for arity from 0 to maximum-arity-tried)
    (until (ignore-errors (apply fn (mapcar #'first (head ranges arity)))))
    (finally (if (>= arity maximum-arity-tried)
                 (error "Arity could not be determined")
                 (return arity)))))

;;<<>>=
(defun determine-arity (fn &optional (ranges (tb:roll-list '((.1 1))))
                                     (maximum-arity-tried 10))
  (let ((min nil))
    (iter (for arity from 0 to maximum-arity-tried)
      (let ((res (ignore-errors (apply fn (mapcar #'first (head ranges arity))))))
        (when (and (not min) res) (setf min arity))
        (until (and min (not res)))
        (finally (return (values min (if (>= arity maximum-arity-tried)
                                         nil
                                         (- arity 1)))))))))