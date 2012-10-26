
(in-package :zgnuplot)

(defun roll-list (list)
  (let ((circular-list (copy-list list)))
    (setf (cdr (last circular-list)) circular-list)
    circular-list))

(defun format-ext (str control-string &rest args)
  "Just like format, except convert certain elements in the arg list into forms
more readable by other programs.  For instance, print all number types in the
1e0 format \(i.e. no fractions or 1d0s), and print pathnames as namestrings.

Format has all sorts of nooks and crannies, so I bet that this facility can be
broken without too much effort."
  (let ((*read-default-float-format* 'long-float))
    (apply #'format str control-string
           (labels
               ((convert-to-external (tree)
                  (cond ((null tree) nil)
                        ((atom tree)
                         (typecase tree
                            (integer tree)
                            (number (float tree 0L0))
                            (pathname (namestring tree))
                            (t tree)))
                        ((consp tree)
                         (mapcar #'convert-to-external tree)))))
             (convert-to-external args)))))

;;; Symbol/string utilities

(defun mkstr (&rest args)
  "MaKe STRing"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun mkdstr (&rest args)
  "MaKe space Delimited STRing"
  (apply #'mkdstr* " " args))

(defun mkdstr* (delimiter &rest args)
  "MaKe arbitrarily Delimited STRing"
  (let ((new-args
          (and args (shuffle args (make-list (1- (length args))
                                             :initial-element delimiter)))))
    (with-output-to-string (s)
      (dolist (a new-args)
        (princ a s)))))

;;; PPCRE extensions
(defun reg-scan-to-string (regex target-string &key (start 0) (end (length target-string)))
  (multiple-value-bind (matches registers) (ppcre:scan-to-strings regex target-string :start start :end end)
    (declare (ignore matches))
    (values-list (iter (for el in-sequence registers)
                       (collect el)))))

(defun reg-scan-to-strings (regex target-string
                                  &key (start 0) (end (length target-string)))
  (multiple-value-bind (matches registers)
      (ppcre:scan-to-strings regex target-string :start start :end end)
    (declare (ignore matches))
    (coerce registers 'list)))

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
                                             (ranges (roll-list '((.1 1)))))
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
                                     (ranges (roll-list '((.1 1)))))
  (let ((min nil))
    (iter (for arity from 0 to maximum-arity-tried)
      (let ((res (ignore-errors (apply fn (mapcar #'first (head ranges arity))))))
        (when (and (not min) res) (setf min arity))
        (until (and min (not res)))
        (finally (return (values min (if (>= arity maximum-arity-tried)
                                         nil
                                         (- arity 1)))))))))

(defun determine-range-dimensionality (fn &optional (ranges (roll-list '((.1 1)))))
  (ima:ima-dimension
   (ima:ensure-ima
    (apply fn (mapcar #'first
                      (head ranges
                            (determine-minimum-arity fn 10 ranges)))))
   0))

