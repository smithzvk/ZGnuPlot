
(in-package :zgnuplot)

;;<<>>=
#+sbcl
(defun determine-minimum-arity (fn &optional maximum-arity-tried ranges)
  (declare (ignore maximum-arity-tried ranges))
  (let ((args (sb-introspect:function-lambda-list fn)))
    (- (length args)
       (length (find-if (lambda (x) (member x '(&rest &optional &keyword)))
                        args)))))
#+(not (or sbcl))
(defun determine-minimum-arity (fn &optional (maximum-arity-tried 10)
                                             (ranges (tb:roll-list '((.1 1)))))
  (iter (for arity from 0 to maximum-arity-tried)
    (until (ignore-errors (apply fn (mapcar #'first (head ranges arity)))))
    (finally (if (>= arity maximum-arity-tried)
                 (error "Arity could not be determined")
                 (return arity)))))

;;<<>>=
#+sbcl
(defun determine-arity (fn &optional maximum-arity-tried ranges)
  (declare (ignore maximum-arity-tried ranges))
  (let* ((arglist (sb-introspect:function-lambda-list fn))
         (min-arity (determine-minimum-arity fn maximum-arity-tried ranges)))
    (cond ((or (member '&allow-other-keys arglist)
               (member '&rest arglist))
           (values min-arity nil))
          (t (values
              min-arity
              (length (remove-if (lambda (x) (member x '(&optional &key)))
                                 arglist)))))))
#+(not (or sbcl))
(defun determine-arity (fn &optional (maximum-arity-tried 10)
                                     (ranges (tb:roll-list '((.1 1)))))
  (let ((min nil))
    (iter (for arity from 0 to maximum-arity-tried)
      (let ((res (ignore-errors (apply fn (mapcar #'first (head ranges arity))))))
        (when (and (not min) res) (setf min arity))
        (until (and min (not res)))
        (finally (return (values min (if (>= arity maximum-arity-tried)
                                         nil
                                         (- arity 1)))))))))
