
(ql:quickload :trivia)

(defpackage :rx
  (:use :cl :trivia :sb-mop))

(defpackage :rx-kw
  (:export :function :+ :- :return :prop :-> :struct
           :i32))

(defpackage :rx-user
  (:use :rx-kw))

(in-package :rx)

(defparameter *contexts* nil)

(defun bind (fn &rest args)
  (lambda (&rest rest)
    (apply fn (concatenate 'list args rest))))

(defmacro with-context (name &body body)
  `(progn
     (push ,name *contexts*)
     (unwind-protect
          (progn ,@body)
       (pop *contexts*))))

(defun syntax-error (msg)
  (format *error-output* "~a~%" msg)
  (loop for ctx in *contexts* do
    (format *error-output* "    In ~a~%" ctx))
  (format *error-output* "~%")
  (error "hi")
  ;(sb-ext:exit :code 1)
  )

(defun typecheck (v ty)
  (unless (typep v ty)
    (syntax-error
     (format nil
             "Syntax error: The form ~a is expected to be of type ~a but is of type ~a"
             v ty (type-of v))))
  v)

(defun arg-to-string (arg)
  (ematch arg
          ((list name type)
           (format nil "~a ~a" type name))
          ((list type)
           (format nil "~a" type))))

;;; Types

;; TY class

(defclass ty ()
  ((name :type string :accessor ty/name :initarg :name)))

(defclass ty.struct-t (ty)
  ((field-names :type list :accessor struct-t/field-names :initarg :field-names)
   (field-types :type list :accessor struct-t/field-types :initarg :field-types)))

(defclass ty.int-t (ty)
  ((bits :type integer :accessor int-t/bits :initarg :bits)
   (signed :type boolean :accessor int-t/signed :initarg :signed)))

;; TYPEDESC class

(defclass typedesc ()
  ())

(defclass typedesc.func-desc (typedesc)
  ((ret-type :type typedesc :accessor func-desc/ret-type :initarg :ret-type)
   (arg-types :type list :accessor func-desc/arg-types :initarg :arg-types)))

(defclass typedesc.ref-desc (typedesc)
  ((name :type symbol :accessor ref-desc/name :initarg :name)
   (ref :type ty :accessor ref-desc/ref :initarg :ref)))

(defclass typedesc.array-desc (typedesc)
  ((base-type :type typedesc :accessor array-desc/base-type :initarg :base-type)
   (dimms :type list :accessor array-desc/dimms :initarg :dimms)))

(defclass typedesc.pointer-desc (typedesc)
  ((base-type :type typedesc :accessor pointer-desc/base-type :initarg :base-type)))

(defgeneric format-type (desc &optional child))


;; (defmethod format-type ((desc typedesc.name-desc) &optional child)
  ;; (format nil "~a~a~a" desc (if child " " "") (or child "")))

(defmethod format-type ((type typedesc.func-desc) &optional child)
  (with-slots (ret-type arg-types) type
    (if child
        (format nil "~a(~a)(~{~a~^ ,~})"
                (format-type ret-type)
                child
                (mapcar #'format-type arg-types))
        (format nil "~a(~{~a~^ ,~})"
                (format-type ret-type)
                (mapcar #'format-type arg-types)))))

(defmethod format-type ((type typedesc.ref-desc) &optional child)
  (format nil "~a~a~a"
          (ty/name type)
          (if child " " "")
          (if child child "")))

(defmethod format-type ((type typedesc.array-desc) &optional child)
  (with-slots (base-type dimms) type
    (format nil "~a~{[~a]~}"
            (format-type base-type child)
            (mapcar #'format-type dimms))))

;;; AST

(defclass ast ()
  ())

(defclass ast.statement (ast)
  ((kind :type string :accessor statement/kind :initarg :kind)))

(defclass ast.statement.function (ast.statement)
  ((name :type symbol :accessor function/name :initarg :name)
   (args :type list :accessor function/args :initarg :args)
   (ret-type :type ty :accessor function/ret-type :initarg :ret-type)
   (body :type list :accessor function/body :initarg :body)
   (type :type ty :accessor function/type :initarg :type)))

(defclass ast.statement.struct (ast.statement)
  ((name :type symbol :accessor struct/name :initarg :name)
   (field-names :type list :accessor struct/field-names :initarg :field-names)
   (field-types :type list :accessor struct/field-types :initarg :field-types)))

(defclass ast.statement.return (ast.statement)
  ((body :type ast.expression :accessor return/body :initarg :body)))

(defclass ast.expression (ast)
  ((type :type typedesc :accessor expression/type :initarg :type :initform nil)))

(defclass ast.expression.identifier (ast.expression)
  ((symbol :type symbol :accessor identifier/symbol :initarg :symbol)))

(defclass ast.expression.number (ast.expression)
  ((value :type number :accessor number/value :initarg :value)))

(defclass ast.expression.string (ast.expression)
  ((value :type symbol :accessor string/value :initarg :value)))

(defclass ast.expression.unop (ast.expression)
  ((op :type string :accessor unop/op :initarg :op)
   (expr :type ast.expression :accessor unop/expr :initarg :expr)))

(defclass ast.expression.binop (ast.expression)
  ((op :type string :accessor binop/op :initarg :op)
   (lhs :type ast.expression :accessor binop/lhs :initarg :lhs)
   (rhs :type ast.expression :accessor binop/rhs :initarg :rhs)))

(defclass ast.expression.funcall (ast.expression)
  ((target :type ast.expression :accessor funcall/target :initarg :target)
   (args :type list :accessor funcall/args :initarg :args)))

(defclass ast.expression.prop-access (ast.expression)
  ((base :type ast.expression :accessor prop-access/base :initarg :base)
   (member :type symbol :accessor prop-access/member :initarg :member)))

(defclass ast.expression.array-sub (ast.expression)
  ((base :type ast.expression :accessor array-sub/base :initarg :base)
   (sub :type ast.expression :accessor array-sub/sub :initarg :sub)))

(defstruct function-arg
  name type)

(defclass ast.expression.var (ast.expression)
  ((name :type symbol :accessor var/name :initarg :name)))

(defun binop-lassoc (name rest)
  (if (= (length rest) 2)
      (make-instance 'ast.expression.binop
                     :op name
                     :lhs (car rest)
                     :rhs (cadr rest))
      (binop-lassoc name
                    (cons (make-instance 'ast.expression.binop
                                         :op name
                                         :lhs (car rest)
                                         :rhs (cadr rest))
                          (cddr rest)))))

(defgeneric format-ast (node &optional toplevel))

(defmethod format-ast ((node ast.statement.return) &optional toplevel)
  (assert (not toplevel))

  (format nil "return (~a)" (format-ast (return/body node))))

(defmethod format-ast ((node ast.expression.var) &optional toplevel)
  (assert (not toplevel))

  (symbol-name (var/name node)))

(defmethod format-ast ((node ast.statement.function) &optional toplevel)
  (assert toplevel)

  (with-slots (name args ret-type body) node
    (with-output-to-string (str)
      (format str "~a ~a(~{~a~^, ~}) {~%"
              (format-type ret-type)
              name
              (mapcar (lambda (arg)
                        (with-slots (name type) arg
                          (format-type type name)))
                      args))
      (loop for n in body do
        (format str "  ~a;~%" (format-ast n)))
      (format str "}~%"))))

(defmethod format-ast ((node ast.statement.struct) &optional toplevel)
  (assert toplevel)

  (with-slots (name field-names field-types) node
    (with-output-to-string (str)
      (format str "struct ~a {~%~{~a;~%~} };~%"
              name
              (loop for field-name in field-names for field-type in field-types
                    collect (format-type field-type field-name))))))

(defmethod format-ast ((node ast.expression.binop) &optional toplevel)
  (assert (not toplevel))

  (with-slots (op lhs rhs) node
    (format nil "(~a) ~a (~a)" (format-ast lhs) op (format-ast rhs))))

(defmethod format-ast ((node integer) &optional toplevel)
  (assert (not toplevel))

  (format nil "~a" node))

(defmethod format-ast ((node ast.expression.funcall) &optional toplevel)
  (declare (ignore toplevel))
  (with-slots (target args) node
    (format nil "(~a)(~{(~a)~^, ~})" (format-ast target) (mapcar #'format-ast args))))

;(defmacro rx-kw:+ (&rest args)
;  `(binop-lassoc "+" (list ,@args)))
;
;(defmacro rx-kw:- (&rest args)
;  `(binop-lassoc "-" (list ,@args)))

;(defun pretty-print-clos-object (object)
;  (let* ((class (class-of object))
;         (class-slots (sb-mop:class-slots class))
;         (slot-names (mapcar #'sb-mop:slot-definition-name class-slots))
;         (slot-values (mapcar (lambda (slot)
;                                (slot-value object (sb-mop:slot-definition-name slot)))
;                              class-slots)))
;    (format t "~&~A~%" class)
;    (loop for name in slot-names
;          for value in slot-values
;          do (format t "  ~A: ~A~%" name value))))

(defun pretty-print-clos-object (object)
  (let* ((class (class-of object))
         (class-slots (class-slots class))
         (slot-names (mapcar #'slot-definition-name class-slots))
         (slot-values (mapcar (lambda (slot)
                                (slot-value object (slot-definition-name slot)))
                              class-slots)))
    (with-output-to-string (str)
      (format str "#<~a" (class-name class))
      (loop for name in slot-names
            for value in slot-values
            do
               (if (listp value)
                   (progn
                     (format str " .~a=(" name)
                     (loop for item in value for i from (length value) downto 0
                           do (write item :stream str :pretty nil)
                              (when (/= i 1)
                                (write " " :stream str :pretty nil)))
                     (format str ")"))
                   (progn
                     (format str " .~a=" name)
                     (write value :pretty nil :stream str))))
      (format str ">"))))

(defparameter *indent* 0)

(defun whitespace (&optional (indent *indent*))
  (if (> indent 0)
      (concatenate 'string
                   "|   "
                   (whitespace (- indent 4)))
      ""))

(defun big-print-clos-object (object)
  (let* ((class (class-of object))
         (class-slots (class-slots class))
         (slot-names (mapcar #'slot-definition-name class-slots))
         (slot-values (mapcar (lambda (slot)
                                (slot-value object (slot-definition-name slot)))
                              class-slots))
         (*indent* (+ *indent* 4)))
    (with-output-to-string (str)
      (if slot-values
          (progn
            (format str "~a:~%" (class-name class))
            (loop for name in slot-names
                  for value in slot-values
                  for i2 from (length slot-names) downto 0
                  do
                     (if (listp value)
                         (if value
                             (progn
                               (format str "~a.~a=(~%" (whitespace) name)
                               (let ((*indent* (+ *indent* 4)))
                                 (loop for item in value for i from (length value) downto 0
                                     do (format str "~a~a" (whitespace) (big-print-clos-object item))
                                        (format str "~%")))
                               (format str "~a)" (whitespace))
                               (when (/= i2 1)
                                 (format str "~%")))
                             (if (/= i2 1)
                                 (format str "~a.~a=()~%" (whitespace) name)
                                 (format str "~a.~a=()" (whitespace) name)))
                         (progn
                           (format str "~a.~a=" (whitespace) name)
                           (format str "~a" (big-print-clos-object value))
                           (when (/= i2 1)
                             (format str "~%"))))))
          (format str "~a" object)))))

(defmethod print-object ((obj ty) stream)
  (write-string (pretty-print-clos-object obj) stream))

(defmethod print-object ((obj ast) stream)
  (write-string (pretty-print-clos-object obj) stream))

(defmethod print-object ((obj typedesc) stream)
  (write-string (pretty-print-clos-object obj) stream))

(defstruct lexical-env
  names
  parent)

(defun new-lexical-env (&optional parent)
  (make-lexical-env :names (make-hash-table) :parent parent))

(defun lexical-env.lookup-or-error (lexenv name)
  (typecheck name 'symbol)

  (if lexenv
      (let ((it (gethash name (lexical-env-names lexenv))))
        (if it
            it
            (lexical-env.lookup-or-error (lexical-env-parent lexenv) name)))
      (error "The name ~a is not found" name)))

(defun lexical-env.push (lexenv name value)
  (when (gethash name (lexical-env-names lexenv))
    (error "The name ~a is already set" name))
  (setf (gethash name (lexical-env-names lexenv)) value))

(defmacro with-lexical-env (compiler &body body)
  (let ((compiler% (gensym))
        (old-env% (gensym)))
    `(let* ((,compiler% ,compiler)
            (,old-env% (compiler-lexenv ,compiler%)))
       (setf (compiler-lexenv ,compiler%) (new-lexical-env ,old-env%))
       (unwind-protect
            (progn ,@body)
         (setf (compiler-lexenv ,compiler%) ,old-env%)))))

(defstruct compiler
  lexenv)

(defun new-compiler ()
  (let ((compiler (make-compiler :lexenv (new-lexical-env))))
    (with-slots (lexenv) compiler
      (lexical-env.push lexenv
                        'rx-kw:i32
                        (make-instance 'ty.int-t :name "int32_t" :bits 32 :signed t)))
    compiler))

(defun compiler.parse (compiler form)
  (with-slots (lexenv) compiler
    (ematch form
      ((list* 'rx-kw:function name args ret-type body)
       (compiler.parse-function compiler name args ret-type body))
      ((list* 'rx-kw:struct name types)
       (compiler.parse-struct compiler name types))
      ((list 'rx-kw:return expr)
       (make-instance 'ast.statement.return
                      :kind "return"
                      :body (compiler.parse compiler expr)))
      ((list 'rx-kw:prop base member)
       (compiler.parse-prop compiler base member))
      ((type integer)
       (make-instance 'ast.expression.number :value form))
      ((type symbol)
       (make-instance 'ast.expression.identifier :symbol form))
      ((list 'quote x)
       )
      ((type string)
       (make-instance 'ast.expression.string :value form)))))

(defun compiler.parse-prop (compiler base member)
  (ematch member
    ((list 'quote _)
     (make-instance 'ast.expression.prop-access
                    :base (compiler.parse compiler base)
                    :member member))
    ((integer)
     (make-instance 'ast.expression.array-sub
                    :base (compiler.parse compiler base)
                    :sub member))))

(defun compiler.parse-type (compiler typ)
  "form -> AST"
  (ematch typ
    ((type symbol)
     (make-instance 'ast.expression.identifier :symbol typ))))

(defun compiler.parse-struct (compiler name members)
  (typecheck name 'symbol)
  (typecheck members 'list)

  (loop for member in members do
    (ematch member
      ((list name typ) ;; only accept this form
       nil)))

  (with-slots (lexenv) compiler
    (let ((field-names (loop for member in members
                             collect (typecheck (car member) 'symbol)))
          (field-types (mapcar #'cadr members)))
      (make-instance 'ast.statement.struct
                     :kind "struct"
                     :name name
                     :field-names field-names
                     :field-types (mapcar (bind #'compiler.parse-type compiler)
                                          field-types)))))

(defun compiler.parse-function (compiler name args ret-type body)
  (typecheck name 'symbol)
  (typecheck args 'list)

  (with-slots (lexenv) compiler
    (let ((arg-structs
            (loop for arg in args collect
                                  (ematch arg
                                    ((list arg-name arg-type)
                                     (typecheck arg-name 'symbol)

                                     (make-function-arg
                                      :name arg-name
                                      :type (compiler.parse-type compiler arg-type))))))
          (parsed-ret-type (compiler.parse-type compiler ret-type))
          (parsed-body (mapcar (lambda (stmt) (compiler.parse compiler stmt)) body)))
      (make-instance 'ast.statement.function
                     :kind "function"
                     :name name
                     :args arg-structs
                     :ret-type parsed-ret-type
                     :body parsed-body))))

(defun compiler.ast-to-typedesc (compiler ast)
  "AST -> typedesc"
  (with-slots (lexenv) compiler
    (typecheck
     (ematch ast
       ((ast.expression.identifier :symbol name)
        (ematch (lexical-env.lookup-or-error lexenv name)
          ((and typ (ty :name name))
           (make-instance 'typedesc.ref-desc :name name :ref typ))))
       ((ast.expression.number)
        (make-instance 'typedesc.ref-desc :name "int32_t" :ref (lexical-env.lookup-or-error lexenv "int32_t")))
       ((ast.expression.funcall :target 'rx-kw:-> :args args)
        (ematch args
          ((list ret-type)
           (make-instance 'typedesc.func-desc
                          :ret-type (compiler.ast-to-typedesc compiler ret-type)
                          :arg-types nil))
          ((type list)
           (make-instance 'typedesc.func-desc
                          :ret-type (compiler.ast-to-typedesc compiler (car (last args)))
                          :arg-types (mapcar (bind #'compiler.ast-to-typedesc compiler) (butlast args))))))
       ((ast.expression.prop-access :base array-type :member dimm)
        (typecheck dimm 'integer)
        (make-instance 'typedesc.array-desc
                       :base-type (compiler.ast-to-typedesc compiler array-type)
                       :dimms dimm)))
     'typedesc)))

(defun compiler.check-types-function (compiler ast)
  (with-lexical-env compiler
    (with-slots (name args ret-type body) ast
      (let ((arg-types
              (loop for arg in args
                    collect
                    (ematch arg
                      ((function-arg :name name :type typ)
                       (typecheck name 'symbol)

                       (let ((arg-type (compiler.ast-to-typedesc compiler typ)))
                         (lexical-env.push (compiler-lexenv compiler) name arg-type)
                         arg-type)))))
            (ret-type-checked (compiler.ast-to-typedesc compiler ret-type)))
        (setf (function/type ast) (make-instance 'typedesc.func-desc
                                                 :ret-type ret-type-checked
                                                 :arg-types arg-types))
        (lexical-env.push (compiler-lexenv compiler) name (function/type ast))
        (loop for stmt in body do
              (compiler.check-types compiler stmt))))))

(defun compiler.check-types-struct (compiler ast)
  (with-slots (name field-names field-types) ast
    (let ((actual-field-types (loop for field-name in field-names
                                    for field-type-desc in field-types
                                    collect (ematch (list field-name field-type-desc)
                                              ((list (type symbol) _)
                                               (compiler.ast-to-typedesc compiler field-type-desc))))))
      (lexical-env.push (compiler-lexenv compiler)
                        name
                        (make-instance 'ty.struct-t
                                       :name name
                                       :field-names field-names
                                       :field-types actual-field-types)))))

(defun compiler.check-types-struct-member-access (compiler ast base member-sym)
  (setf (expression/type ast)
        (let ((base-type (expression/type base))
              (member-sym-name (symbol-name member-sym)))
          (ematch base-type
            ((typedesc.ref-desc :ref (and struct (ty.struct-t)))
             (loop for field-name in (struct-t/field-names struct)
                   for field-type in (struct-t/field-types struct) do
                     (when (string= field-name member-sym-name)
                       (return field-type))
                   finally
                      (error "Struct ~a doesn't have a field named ~a" (ty/name struct) member-sym)))))))

(defun compiler.check-types-prop (compiler ast base member)
  (compiler.check-types compiler base)

  (let ((base-type (expression/type base)))
    (ematch (list base-type member)
      ((list (typedesc.ref-desc :ref (ty.struct-t)) (list 'quote member-name))
       (typecheck member-name 'symbol)
       (compiler.check-types-struct-member-access compiler ast base member-name))
      ((list (typedesc.array-desc) _)
       (compiler.check-types-array-access compiler base))
      ((list (typedesc.pointer-desc) _)
       (compiler.check-types-pointer-deref compiler base member)))))

(defun compiler.lookup-symbol-type (compiler sym)
  (typecheck
   (lexical-env.lookup-or-error (compiler-lexenv compiler) sym)
   'typedesc))

(defun compiler.check-types-identifier (compiler ast)
  (setf (expression/type ast) (compiler.lookup-symbol-type compiler (identifier/symbol ast))))

(defun compiler.check-types (compiler ast)
  (ematch ast
    ((ast.statement.function)
     (compiler.check-types-function compiler ast))
    ((ast.statement.struct)
     (compiler.check-types-struct compiler ast))
    ((ast.expression.prop-access :base base :member member)
     (compiler.check-types-prop compiler ast base member))
    ((ast.expression.identifier :symbol sym)
     (compiler.check-types-identifier compiler ast))))

(defun read-property-access (stream char)
  `(rx-kw:prop ,@(read-delimited-list #\] stream)))

(set-macro-character #\[ #'read-property-access nil)

(defun right-bracket-read-error (stream char)
  (error "Unexpected ]"))

(set-macro-character #\] #'right-bracket-read-error nil)

(defun main ()
  (with-open-file (s "test.rx")
    (let ((*package* (find-package :rx-user)))
      (let ((compiler (new-compiler)))
        (loop for x = (read s nil 'eof)
              while (not (eq x 'eof))
              do
                 (let ((ast (compiler.parse compiler x)))
                   (compiler.check-types compiler ast)
                   (format t "~a~%" (big-print-clos-object ast))))
                                        ;    (loop for x across *output* do
                                        ;      (format t "~a~%" (pretty-print-clos-object x))
                                        ;      (format t "~a~%" (format-ast x t)))

        ))))
