
(ql:quickload :trivia)

(declaim (optimize (debug 3)))

(defpackage :rx
  (:use :cl :trivia :sb-mop))

(defpackage :rx-kw
  (:export

   :|function|

   :+ :- :|return| :|prop| :-> :|struct|
   :+ :- :/ :* :%
   :&
   := :== :!= :< :> :<= :>=
   :>> :<<
   :|bor| :|band| :|bxor|
   :|and| :|or|

   :|let| :|while| :|if|

   :|const|

   :|void| :|i32| :|i8| :|i16| :|i64| :|char|
   :|u8| :|u16| :|u32| :|u64|

   :|template|

   :|block|))

(defpackage :rx-user
  (:use :rx-kw))

(in-package :rx)

(defparameter *contexts* nil)

(defun bind (fn &rest args)
  (lambda (&rest rest)
    (apply fn (concatenate 'list args rest))))

(defmacro $ (class &rest args)
  `(make-instance ',class ,@args))

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
  (error "~a" msg)
  ;(sb-ext:exit :code 1)
  )

(defun typecheck (v ty)
  (unless (typep v ty)
    (syntax-error
     (format nil
             "Syntax error: The form ~a is expected to be of type ~a but is of type ~a"
             v ty (type-of v))))
  v)

(defun internal-check (v &rest msg)
  (unless v
    (apply #'error msg))
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

(defclass ty.char-t (ty)
  ())

(defclass ty.void-t (ty)
  ())

;; TYPEDESC class

(defclass typedesc ()
  ((const :type t :accessor typedesc/const :initarg :const :initform nil)))

(defclass typedesc.func-desc (typedesc)
  ((ret-type :type typedesc :accessor func-desc/ret-type :initarg :ret-type)
   (param-types :type list :accessor func-desc/param-types :initarg :param-types)))

(defclass typedesc.ref-desc (typedesc)
  ((name :type symbol :accessor ref-desc/name :initarg :name)
   (ref :type ty :accessor ref-desc/ref :initarg :ref)))

(defclass typedesc.array-desc (typedesc)
  ((base-type :type typedesc :accessor array-desc/base-type :initarg :base-type)
   (dimms :type list :accessor array-desc/dimms :initarg :dimms)))

(defclass typedesc.pointer-desc (typedesc)
  ((base-type :type typedesc :accessor pointer-desc/base-type :initarg :base-type)))

(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'slot-definition-name (class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

(defun typedesc-make-const (td)
  (let ((copy (shallow-copy-object td)))
    (setf (typedesc/const copy) t)
    copy))

;; FORMAT-TYPE

(defgeneric format-type (desc &optional child))

;; (defmethod format-type ((desc typedesc.name-desc) &optional child)
  ;; (format nil "~a~a~a" desc (if child " " "") (or child "")))

(defmethod format-type ((type typedesc.func-desc) &optional child)
  (with-slots (ret-type param-types) type
    (if child
        (format nil "~a(~a)(~{~a~^ ,~})"
                (format-type ret-type)
                child
                (mapcar #'format-type param-types))
        (format nil "~a(~{~a~^ ,~})"
                (format-type ret-type)
                (mapcar #'format-type param-types)))))

(defmethod format-type ((type typedesc.ref-desc) &optional child)
  (let ((const (typedesc/const type)))
    (format nil "~a~a~a~a"
            (ty/name (ref-desc/ref type))
            (if const " const" "")
            (if child " " "")
            (if child child ""))))

(defmethod format-type ((type typedesc.pointer-desc) &optional child)
  (format-type (pointer-desc/base-type type) (format nil "*~a"  (if child child ""))))

(defmethod format-type ((type typedesc.array-desc) &optional child)
  (with-slots (base-type dimms) type
    (if (and child (< 0 (length child)))
        (format-type base-type (format nil "(~a)~{[~a]~}" child dimms))
        (format-type base-type (format nil "~{[~a]~}" dimms)))))

;;; AST

(defclass ast ()
  ((original-source :type t :accessor ast/original-source :initarg :original-source :initform nil)))

(defclass ast.statement (ast)
  ((kind :type string :accessor statement/kind :initarg :kind)))

(defstruct function-arg
  name type-expr)

(defclass ast.statement.function (ast.statement)
  ((name :type symbol :accessor function/name :initarg :name)
   (args :type list :accessor function/args :initarg :args)
   (ret-type-expr :type ast :accessor function/ret-type-expr :initarg :ret-type-expr)
   (body :type list :accessor function/body :initarg :body)
   (type :type typedesc.func-desc :accessor function/type)))

(defclass ast.statement.struct (ast.statement)
  ((name :type symbol :accessor struct/name :initarg :name)
   (field-names :type list :accessor struct/field-names :initarg :field-names)
   (field-type-exprs :type list :accessor struct/field-type-exprs :initarg :field-type-exprs)
   (type :type ty.struct-t :accessor struct/type)))

(defclass ast.statement.return (ast.statement)
  ((body :type ast.expression :accessor return/body :initarg :body)))

(defclass ast.statement.while (ast.statement)
  ((condition :type ast.expression :accessor while/condition :initarg :condition)
   (body :type list :accessor while/body :initarg :body)))

(defclass ast.statement.compound (ast.statement)
  ((statements :type list :accessor compound/statements :initarg :statements)))

(defclass ast.statement.if (ast.statement)
  ((condition :type ast.expression :accessor if/condition :initarg :condition)
   (body :type ast.statement.compound :accessor if/body :initarg :body)
   (else :type (or ast.statement.compound null) :accessor if/else :initarg :else)))

(defclass ast.statement.var-def (ast.statement)
  ((name :type symbol :accessor var-def/name :initarg :name)
   (type-expr :type ast :accessor var-def/type :initarg :type-expr)
   (init :type ast.expression :accessor var-def/init :initarg :init)
   (type :type typedesc :accessor var-def/type)))

(defclass ast.expression (ast)
  ((type :type typedesc :accessor expression/type :initarg :type :initform nil)))

(defclass ast.expression.identifier (ast.expression)
  ((symbol :type symbol :accessor identifier/symbol :initarg :symbol)))

(defclass ast.expression.character (ast.expression)
  ((value :type character :accessor character/value :initarg :value)))

(defclass ast.expression.number (ast.expression)
  ((value :type number :accessor number/value :initarg :value)
   (bits :type (or number null) :accessor number/bits :initarg :bits)
   (signed :type t :accessor number/signed :initarg :signed)))

(defclass ast.expression.string-literal (ast.expression)
  ((value :type string :accessor string-literal/value :initarg :value)))

(defclass ast.expression.binop (ast.expression)
  ((op :type string :accessor binop/op :initarg :op)
   (lhs :type ast.expression :accessor binop/lhs :initarg :lhs)
   (rhs :type ast.expression :accessor binop/rhs :initarg :rhs)))

(defclass ast.expression.unop (ast.expression)
  ((operator :type string :accessor unop/operator :initarg :operator)
   (operand :type ast.expression :accessor unop/operand :initarg :operand)))

(defclass ast.expression.funcall (ast.expression)
  ((target :type ast.expression :accessor funcall/target :initarg :target)
   (args :type list :accessor funcall/args :initarg :args)))

(defclass ast.expression.prop-access (ast.expression)
  ((base :type ast.expression :accessor prop-access/base :initarg :base)
   (member :type symbol :accessor prop-access/member :initarg :member)))

(defclass ast.expression.array-sub (ast.expression)
  ((base :type ast.expression :accessor array-sub/base :initarg :base)
   (sub :type ast.expression :accessor array-sub/sub :initarg :sub)))

(defun ast/original-source-string (ast)
  (write-to-string (ast/original-source ast) :readably nil))

(defun ast/is-lvalue (ast)
  (match ast
    ((or (ast.expression.identifier)
         (ast.expression.unop :operator "*")
         (ast.expression.array-sub))
     t)
    ((ast.expression.prop-access :base base)
     (ast/is-lvalue base))))

(defun type/assignable-to (a b &optional check-const)
  (assert (and a b))

  (match (list a b)
    ((list (ty.int-t (bits bits-1) (signed signed-1))
           (ty.int-t (bits bits-2) (signed signed-2)))
     (and (= bits-1 bits-2)
          (eq signed-1 signed-2)))
    ((list (ty.char-t) (ty.char-t))
     t)
    ((list (typedesc.ref-desc :ref ref-1 :const const-1)
           (typedesc.ref-desc :ref ref-2 :const const-2))
     (and (or (not check-const)
              (eq const-1 const-2))
          (type/assignable-to ref-1 ref-2 t)))
    ((list (typedesc.pointer-desc :base-type base-1 :const const-1)
           (typedesc.pointer-desc :base-type base-2 :const const-2))
     (and (or (not check-const)
              (eq const-1 const-2))
          (type/assignable-to base-1 base-2 t)))
    ((list (typedesc.array-desc :dimms dimms-1 :base-type base-1 :const const-1)
           (typedesc.array-desc :dimms dimms-2 :base-type base-2 :const const-2))
     (and (or (not check-const)
              (eq const-1 const-2))
          (= (length dimms-1) (length dimms-2))
          (equal dimms-1 dimms-2)
          (type/assignable-to base-1 base-2 t)))
    ((list (typedesc.func-desc :ret-type ret-type-1 :param-types ptypes-1)
           (typedesc.func-desc :ret-type ret-type-2 :param-types ptypes-2))
     (and (= (length ptypes-1) (length ptypes-2))
          (type/assignable-to ret-type-1 ret-type-2)
          (loop for p1 in ptypes-1 for p2 in ptypes-2
                always (type/assignable-to p1 p2))))))

(defun type/is-integral (obj)
  (assert obj)
  (match obj
    ((ty.int-t)
     obj)
    ((typedesc.ref-desc :ref r)
     (type/is-integral r))
    ((ast.expression :type typ)
     (type/is-integral typ))))

(defun type/is-array (obj)
  (assert obj)

  (typep obj 'typedesc.array-desc))

(defun type/is-func (obj)
  (assert obj)

  (typep obj 'typedesc.func-desc))

(defun type/is-pointer (obj)
  (assert obj)

  (match obj
    ((typedesc.pointer-desc :base-type base)
     base)
    ((typedesc.ref-desc :ref r)
     (type/is-pointer r))
    ((ast.expression :type typ)
     (type/is-pointer typ))))

(defgeneric format-ast (node &optional toplevel))

(defmethod format-ast :before (node &optional toplevel)
  (assert (or (not (typep node 'ast.expression))
              (expression/type node))))

(defmethod format-ast ((node ast.statement.return) &optional toplevel)
  (assert (not toplevel))

  (format nil "return (~a)" (format-ast (return/body node))))

(defmethod format-ast ((node ast.statement.compound) &optional toplevel)
  (assert (not toplevel))

  (format nil "{~%~{~a;~%~}}~%"
          (mapcar #'format-ast (compound/statements node))))

(defmethod format-ast ((node ast.statement.while) &optional toplevel)
  (assert (not toplevel))

  (format nil "while (~a) {~%~{~a;~%~}}"
          (format-ast (while/condition node))
          (mapcar #'format-ast (while/body node))))

(defmethod format-ast ((node ast.statement.var-def) &optional toplevel)
  (with-slots (name type init) node
    (format nil "~a = ~a;"
            (format-type type (symbol-name name))
            (format-ast init))))

(defmethod format-ast ((node ast.statement.if) &optional toplevel)
  (assert (not toplevel))

  (with-slots (condition body else) node
    (if else
        (format nil "if (~a) { ~a; } else { ~a; }"
                (format-ast condition)
                (format-ast body)
                (format-ast else))
        (format nil "if (~a) { ~a };"
                (format-ast condition)
                (format-ast body)))))

(defmethod format-ast ((node ast.expression.identifier) &optional toplevel)
  (assert (not toplevel))

  (symbol-name (identifier/symbol node)))

(defmethod format-ast ((node ast.expression.number) &optional toplevel)
  (assert (not toplevel))

  (format nil "((~a)~a)"
          (ty/name (ref-desc/ref (expression/type node)))
          (number/value node)))

(defmethod format-ast ((node ast.expression.character) &optional toplevel)
  (assert (not toplevel))

  (format nil "'~a'" (character/value)))

(defmethod format-ast ((node ast.expression.string-literal) &optional toplevel)
  (format nil "~s" (string-literal/value node)))

(defmethod format-ast ((node ast.statement.function) &optional toplevel)
  (assert toplevel)

  (with-slots (name args body type) node
    (with-output-to-string (str)
      (format str "~a ~a(~{~a~^, ~}) {~%"
              (format-type (func-desc/ret-type type))
              name
              (loop for arg in args for arg-type in (func-desc/param-types type)
                    collect (format-type arg-type (symbol-name (function-arg-name arg)))))
      (loop for n in body do
        (format str "  ~a;~%" (format-ast n)))
      (format str "}~%"))))

(defmethod format-ast ((node ast.expression.prop-access) &optional toplevel)
  (assert (not toplevel))

  (with-slots (base member) node
    (format nil "(~a.~a)" (format-ast base) member)))

(defmethod format-ast ((node ast.expression.array-sub) &optional toplevel)
  (assert (not toplevel))

  (with-slots (base sub) node
    (format nil "(~a[~a])" (format-ast base) (format-ast sub))))

(defmethod format-ast ((node ast.statement.struct) &optional toplevel)
  (assert toplevel)

  (with-slots (name field-names field-types) (struct/type node)
    (with-output-to-string (str)
      (format str "typedef struct ~a {~%~{~a;~%~}} ~a;~%"
              name
              (loop for field-name in field-names for field-type in field-types
                    collect (format-type field-type (symbol-name field-name)))
              name))))

(defmethod format-ast ((node ast.expression.unop) &optional toplevel)
  (assert (not toplevel))

  (with-slots (operand operator) node
    (format nil "(~a(~a))"
            operator
            (format-ast operand))))

(defmethod format-ast ((node ast.expression.binop) &optional toplevel)
  (assert (not toplevel))

  (with-slots (op lhs rhs) node
    (format nil "((~a)(~a ~a ~a))"
            (format-type (expression/type node))
            (format-ast lhs)
            op
            (format-ast rhs))))

(defmethod format-ast ((node ast.expression.funcall) &optional toplevel)
  (declare (ignore toplevel))
  (with-slots (target args) node
    (format nil "(~a(~{~a~^, ~}))" (format-ast target) (mapcar #'format-ast args))))

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

(defun lexical-env.lookup-or-error (lexenv name enforce-type)
  (typecheck name 'symbol)

  (if lexenv
      (let ((it (gethash name (lexical-env-names lexenv))))
        (if it
            (if (typep it enforce-type)
                it
                (error "The name ~a is not of type ~a" enforce-type))
            (lexical-env.lookup-or-error (lexical-env-parent lexenv) name enforce-type)))
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

(defparameter *i8*
  ($ ty.int-t :name "int8_t" :bits 8 :signed t))
(defparameter *i16*
  ($ ty.int-t :name "int16_t" :bits 16 :signed t))
(defparameter *i32*
  ($ ty.int-t :name "int32_t" :bits 32 :signed t))
(defparameter *i64*
  ($ ty.int-t :name "int64_t" :bits 64 :signed t))

(defparameter *bitsize-to-signed-type*
  (list (cons 8 *i8*)
        (cons 16 *i16*)
        (cons 32 *i32*)
        (cons 64 *i64*)))

(defparameter *u8*
  ($ ty.int-t :name "uint8_t" :bits 8 :signed nil))
(defparameter *u16*
  ($ ty.int-t :name "uint16_t" :bits 16 :signed nil))
(defparameter *u32*
  ($ ty.int-t :name "uint32_t" :bits 32 :signed nil))
(defparameter *u64*
  ($ ty.int-t :name "uint64_t" :bits 64 :signed nil))

(defparameter *bitsize-to-unsigned-type*
  (list (cons 8 *u8*)
        (cons 16 *u16*)
        (cons 32 *u32*)
        (cons 64 *u64*)))

(defparameter *char*
  ($ ty.char-t :name "char"))

(defparameter *void*
  ($ ty.void-t :name "void"))

(defparameter *symbol-to-int-type*
  (list (list 'rx-kw:|i8| *i8*)
        (list 'rx-kw:|i16| *i16*)
        (list 'rx-kw:|i32| *i32*)
        (list 'rx-kw:|i64| *i64*)
        (list 'rx-kw:|u8| *u8*)
        (list 'rx-kw:|u16| *u16*)
        (list 'rx-kw:|u32| *u32*)
        (list 'rx-kw:|u64| *u64*)))

(defun new-compiler ()
  (let ((compiler (make-compiler :lexenv (new-lexical-env))))
    (with-slots (lexenv) compiler
      (loop for (sym typ) in *symbol-to-int-type*
            do (lexical-env.push lexenv sym ($ typedesc.ref-desc
                                               :name (ty/name typ)
                                               :ref typ)))

      (lexical-env.push lexenv
                        'rx-kw:|char|
                        *char*)

      (lexical-env.push lexenv
                        'rx-kw:|void|
                        *void*))
    compiler))

(defparameter *op-to-repr*
  '((rx-kw:|bor| . "|")
    (rx-kw:|band| . "&")
    (rx-kw:|bxor| . "^")
    (rx-kw:|and| . "&&")
    (rx-kw:|or| . "||")))

(defparameter *form-parsers* nil)
(setf *form-parsers* nil)

(defmacro def-parser (name args matcher &body body)
  (let ((compiler% (gensym))
        (form% (gensym)))
    `(progn
       (defun ,name ,args
         (ematch ,(second args)
           (,matcher
            ,@body)))
       (setf
        *form-parsers*
        (append *form-parsers*
                (list
                 (lambda (,compiler% ,form%)
                   (match ,form%
                     (,matcher
                      (,name ,compiler% ,form%))
                     (_
                      nil)))))))))

(defun compiler.parse* (compiler form)
  (or (some (lambda (parser) (funcall parser compiler form))
            *form-parsers*)
      (error "No parser for form ~a" form)))

(def-parser compiler.parse-let (compiler form)
    (list 'rx-kw:|let| name-sym type-expr init)
  (typecheck name-sym 'symbol)
  ($ ast.statement.var-def
     :kind "let"
     :name name-sym
     :type-expr (compiler.parse-type-expr compiler type-expr)
     :init (compiler.parse compiler init)))

(def-parser compiler.parse-let-invalid! (compiler form)
    (list* 'rx-kw:|let| _)
  (error "Invalid syntax in let"))

(def-parser compiler.parse-if-else (compiler form)
    (list 'rx-kw:|if| condition body else)
  ($ ast.statement.if
     :kind "if"
     :condition (compiler.parse compiler condition)
     :body (compiler.parse compiler body)
     :else (when else (compiler.parse compiler else))))

(def-parser compiler.parse-if (compiler form)
    (list 'rx-kw:|if| cond body)
  (compiler.parse-if-else compiler (append form nil)))

(def-parser compiler.parse-compound-statement (compiler form)
    (list* 'rx-kw:|block| stmts)
  ($ ast.statement.compound
     :kind "compound"
     :statements (mapcar (bind #'compiler.parse compiler)
                         stmts)))

(def-parser compiler.parse-while (compiler form)
    (list* 'rx-kw:|while| condition body)
  (let ((condition-ast (compiler.parse compiler condition)))
    (let ((body-asts (mapcar (bind #'compiler.parse compiler)
                             body)))
      ($ ast.statement.while
         :kind "while"
         :condition condition-ast
         :body body-asts))))

(def-parser compiler.parse-function (compiler form)
    (list* 'rx-kw:|function| name args ret-type body)
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
                                      :type-expr (compiler.parse-type-expr compiler arg-type))))))
          (ret-type-expr (compiler.parse-type-expr compiler ret-type))
          (parsed-body (mapcar (lambda (stmt) (compiler.parse compiler stmt)) body)))
      ($ ast.statement.function
         :kind "function"
         :name name
         :args arg-structs
         :ret-type-expr ret-type-expr
         :body parsed-body))))

(def-parser compiler.parse-struct (compiler form)
    (list* 'rx-kw:|struct| name members)
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
      ($ ast.statement.struct
         :kind "struct"
         :name name
         :field-names field-names
         :field-type-exprs (mapcar (bind #'compiler.parse-type-expr compiler)
                                   field-types)))))

(def-parser compiler.parse-return (compiler form)
    (list 'rx-kw:|return| expr)
  ($ ast.statement.return
     :kind "return"
     :body (compiler.parse compiler expr)))

(def-parser compiler.parse-prop (compiler form)
    (list* 'rx-kw:|prop| base member1 members)

  (labels ((parse-prop (compiler base members)
             (ematch members
               (nil
                base)
               ((list* (list 'quote mbr-sym) rest)
                (parse-prop
                 compiler
                 ($ ast.expression.prop-access
                    :base base
                    :member mbr-sym)
                 rest))
               ((list* (and member (integer)) rest)
                (parse-prop
                 compiler
                 ($ ast.expression.array-sub
                    :base base
                    :sub (compiler.parse compiler member))
                 rest)))))
    (parse-prop compiler (compiler.parse compiler base) (cons member1 members))))

(def-parser compiler.parse-invalid-prop (compiler form)
    (list* 'rx-kw:|prop| _)
  (error "Invalid prop syntax"))

(def-parser compiler.parse-char (compiler form)
    (type character)
  ($ ast.expression.character :value form))

(def-parser compiler.parse-integer (compiler form)
    (type integer)
  (if (> form #xFFFFFFFFFFFFFFFF)
      (error "Integer ~a too large to fit in 64 bits" form)
      (if (> form #x7FFFFFFFFFFFFFFF)
          ($ ast.expression.number :value form :bits 64 :signed nil)
          (if (> form #xFFFFFFFF)
              ($ ast.expression.number :value form :bits 64 :signed t)
              (if (> form #x7FFFFFFF)
                  ($ ast.expression.number :value form :bits 32 :signed nil)
                  ($ ast.expression.number :value form :bits 32 :signed t))))))

(def-parser compiler.parse-string (compiler form)
    (type string)
  ($ ast.expression.string-literal :value form))

(def-parser compiler.parse-identifier (compiler form)
    (type symbol)
  ($ ast.expression.identifier :symbol form))

(defun compiler.parse-nary-op-to-binary-lassoc (compiler name ast1 ast2 rest)
  (let ((expr ($ ast.expression.binop
                 :op name
                 :lhs ast1
                 :rhs ast2)))
    (if rest
        (compiler.parse-nary-op-to-binary-lassoc
         compiler
         name
         expr
         (compiler.parse compiler (car rest))
         (cdr rest))

        expr)))

(def-parser compiler.parse-nary-lassoc-expr (compiler form)
    (list* (and op-name (or 'rx-kw:+ 'rx-kw:- 'rx-kw:/ 'rx-kw:* 'rx-kw:%
                            'rx-kw:== 'rx-kw:!= 'rx-kw:< 'rx-kw:> 'rx-kw:<= 'rx-kw:>=
                            'rx-kw:>> 'rx-kw:<<
                            'rx-kw:|bor| 'rx-kw:|band| 'rx-kw:|bxor|
                            'rx-kw:|and| 'rx-kw:|or|))
           arg1 arg2 rest)
  (compiler.parse-nary-op-to-binary-lassoc
   compiler
   (or (cdr (assoc op-name *op-to-repr*)) (symbol-name op-name))
   (compiler.parse compiler arg1)
   (compiler.parse compiler arg2)
   rest))

(defun compiler.parse-nary-op-to-binary-rassoc (compiler name ast1 ast2 rest)
  (let ((expr ($ ast.expression.binop
                 :op name
                 :lhs ast2
                 :rhs ast1)))
    (if rest
        (compiler.parse-nary-op-to-binary-rassoc
         compiler
         name
         expr
         (compiler.parse compiler (car rest))
         (cdr rest))

        expr)))

(def-parser compiler.parse-nary-rassoc-expr (compiler form)
    (list* (and op-name (or 'rx-kw:=))
           arg1 arg2 rest)
  (let ((reversed-args (reverse (list* arg1 arg2 rest))))
    (compiler.parse-nary-op-to-binary-rassoc
     compiler
     (symbol-name op-name)
     (compiler.parse compiler (car reversed-args))
     (compiler.parse compiler (cadr reversed-args))
     (cddr reversed-args))))

(def-parser compiler.parse-unop (compiler form)
    (list (and op-name (or 'rx-kw:* 'rx-kw:&))
          operand)
  ($ ast.expression.unop
     :operator (symbol-name op-name)
     :operand (compiler.parse compiler operand)))

(def-parser compiler.parse-funcall (compiler form)
    (list* fn args)
  ($ ast.expression.funcall
     :target (compiler.parse compiler fn)
     :args (mapcar (bind #'compiler.parse compiler)
                   args)))

(defun compiler.parse (compiler form)
  (let ((result (compiler.parse* compiler form)))
    (setf (ast/original-source result) form)
    result))

(defun compiler.parse-type-expr (compiler typ)
  "form -> AST"
  (ematch typ
    ((type symbol)
     ($ ast.expression.identifier :symbol typ))
    ((list 'rx-kw:|prop| arr-type (and (type integer) bound))
     ($ ast.expression.array-sub
        :base (compiler.parse-type-expr compiler arr-type)
        :sub bound))
    ((list 'rx-kw:* x)
     ($ ast.expression.funcall
        :target ($ ast.expression.identifier
                   :symbol 'rx-kw:*)
        :args (list (compiler.parse-type-expr compiler x))))
    ((list* 'rx-kw:-> type1 rest)
     ($ ast.expression.funcall
        :target ($ ast.expression.identifier
                   :symbol 'rx-kw:->)
        :args (mapcar (bind #'compiler.parse-type-expr compiler)
                      (cons type1 rest))))
    ((list 'rx-kw:|const| x)
     ($ ast.expression.funcall
        :target ($ ast.expression.identifier
                   :symbol 'rx-kw:|const|)
        :args (list (compiler.parse-type-expr compiler x))))
    (_
     (error "Syntax error in type specification ~a" typ))))

(defun compiler.ast-to-typedesc (compiler ast)
  "AST -> typedesc"
  (with-slots (lexenv) compiler
    (typecheck
     (ematch ast
       ((ast.expression.identifier :symbol name)
        (ematch (lexical-env.lookup-or-error lexenv name '(or ty typedesc))
          ((and typ (ty :name name))
           ($ typedesc.ref-desc :name name :ref typ))
          ((and typ (typedesc))
           typ)))
       ((ast.expression.array-sub :base (and (ast) base-type) :sub (and (type integer) bound))
        ($ typedesc.array-desc
           :base-type (compiler.ast-to-typedesc compiler base-type)
           :dimms (list bound)))
       ((ast.expression.funcall :target (ast.expression.identifier :symbol 'rx-kw:->)
                                :args args)
        (ematch args
          ((list ret-type)
           ($ typedesc.func-desc
              :ret-type (compiler.ast-to-typedesc compiler ret-type)
              :param-types nil))
          ((type list)
           ($ typedesc.func-desc
              :ret-type (compiler.ast-to-typedesc compiler (car (last args)))
              :param-types (mapcar (bind #'compiler.ast-to-typedesc compiler) (butlast args))))))
       ((ast.expression.prop-access :base array-type :member dimm)
        (typecheck dimm 'integer)
        ($ typedesc.array-desc
           :base-type (compiler.ast-to-typedesc compiler array-type)
           :dimms dimm))
       ((ast.expression.funcall :target (ast.expression.identifier :symbol 'rx-kw:|const|)
                                :args (list x))
        (typedesc-make-const (compiler.ast-to-typedesc compiler x)))
       ((ast.expression.funcall :target (ast.expression.identifier :symbol 'rx-kw:*)
                                :args (list x))
        ($ typedesc.pointer-desc :base-type (compiler.ast-to-typedesc compiler x))))
     'typedesc)))

(defgeneric compiler.check-types (compiler ast))

(defmethod compiler.check-types (compiler (ast ast.statement.var-def))
  (with-slots (name type-expr init) ast
    (compiler.check-types compiler init)
    (setf (var-def/type ast) (compiler.ast-to-typedesc compiler type-expr))

    (when (type/is-func (var-def/type ast))
      (error "In definition ~a cannot assign to a variable of function type"
             (ast/original-source ast)))

    (unless (type/assignable-to (var-def/type ast) (expression/type init))
      (error "In definition ~a cannot convert ~a from type ~a to type ~a"
             (ast/original-source-string ast)
             (ast/original-source-string init)
             (format-type (expression/type init) "x")
             (format-type (var-def/type ast) "x")))
    (lexical-env.push (compiler-lexenv compiler) name (var-def/type ast))))

(defmethod compiler.check-types (compiler (ast ast.expression.funcall))
  (with-slots (target args) ast
    (compiler.check-types compiler target)
    (mapc (bind #'compiler.check-types compiler) args)

    (let ((target-type (expression/type target)))
      (typecheck target-type 'typedesc.func-desc)

      (with-slots (ret-type param-types) target-type
        (unless (= (length args) (length param-types))
          (error "~a expects ~a arguments, called with ~a"
                 (format-ast target)
                 (length param-types)
                 (length args)))

        (loop for param-type in param-types
              for arg in args
              do (unless (type/assignable-to param-type (expression/type arg))
                   (error "In function call ~a can't convert from argument ~a type ~a to parameter type ~a"
                          (ast/original-source-string ast)
                          (format-ast arg)
                          (format-type (expression/type arg))
                          (format-type param-type ))))

        (setf (expression/type ast) ret-type)))))

(defmethod compiler.check-types (compiler (ast ast.statement.while))
  (with-lexical-env compiler
    (with-slots (condition body) ast
      (compiler.check-types compiler condition)

      (assert (type/is-integral condition))

      (mapc (bind #'compiler.check-types compiler) body))))

(defmethod compiler.check-types (compiler (ast ast.statement.function))
  (with-slots (name args ret-type-expr body) ast
    (let ((param-types (mapcar (lambda (arg)
                                 (compiler.ast-to-typedesc
                                  compiler
                                  (function-arg-type-expr arg)))
                               args)))
      (loop for p-type in param-types
            for arg in args
            do (when (type/is-array p-type)
                 (error "In function ~a: array type not allowed in function parameter ~a"
                        name
                        (format-type p-type (symbol-name (function-arg-name arg))))))
      (setf (function/type ast)
            ($ typedesc.func-desc
               :ret-type (compiler.ast-to-typedesc compiler ret-type-expr)
               :param-types param-types)))

    (lexical-env.push (compiler-lexenv compiler) name (function/type ast))
    (with-lexical-env compiler
      (loop for arg-type in (func-desc/param-types (function/type ast))
            for arg in args do
              (lexical-env.push (compiler-lexenv compiler)
                                (function-arg-name arg)
                                arg-type))
      (mapc (bind #'compiler.check-types compiler) body))))

(defmethod compiler.check-types (compiler (ast ast.statement.struct))
  (with-slots (name field-names field-type-exprs) ast
    (let ((struct-t ($ ty.struct-t
                       :name name
                       :field-names field-names
                       :field-types (mapcar
                                     (bind #'compiler.ast-to-typedesc compiler)
                                     field-type-exprs))))
      (setf (struct/type ast) struct-t)
      (lexical-env.push (compiler-lexenv compiler) name struct-t))))

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

(defun compiler.check-types (compiler (ast ast.expression.prop-access))
  (with-slots (base member) ast
    (compiler.check-types compiler base)

    (let ((base-type (expression/type base)))
      (ematch (list base-type member)
        ((list (typedesc.ref-desc :ref (ty.struct-t)) member-name)
         (typecheck member-name 'symbol)
         (compiler.check-types-struct-member-access compiler ast base member-name))
        ((list (typedesc.array-desc) _)
         (compiler.check-types-array-access compiler base))
        ((list (typedesc.pointer-desc) _)
         (compiler.check-types-pointer-deref compiler base member))))))

(defun compiler.lookup-symbol-type (compiler sym)
  (lexical-env.lookup-or-error (compiler-lexenv compiler) sym 'typedesc))

(defmethod compiler.check-types (compiler (ast ast.expression.identifier))
  (setf (expression/type ast)
        (compiler.lookup-symbol-type compiler (identifier/symbol ast))))

(defmethod compiler.check-types (compiler (ast ast.expression.character))
  (setf (expression/type ast) *char*))

(defmethod )

(defun compiler.check-types-number (compiler ast)
  (with-slots (signed bits) ast
    (setf (expression/type ast)
          (let ((typ (if signed
                         (cdr (assoc bits *bitsize-to-signed-type*))
                         (cdr (assoc bits *bitsize-to-unsigned-type*)))))
            (unless typ
              (error "Internal error: expected a size of 8, 16, 32, 64 bits, got ~a" bits))
            ($ typedesc.ref-desc
               :name (ty/name typ)
               :ref typ)))))

(defun compiler.check-types-string-literal (compiler ast)
  (setf (expression/type ast)
        ($ typedesc.array-desc
           :dimms (list (1+ (length (string-literal/value ast))))
           :base-type ($ typedesc.ref-desc
                         :name "char"
                         :const t
                         :ref *char*))))

(defun compiler.check-types-binop (compiler ast)
  (with-slots (op lhs rhs) ast
    (compiler.check-types compiler lhs)
    (compiler.check-types compiler rhs)

    (ematch op
      ("="
       (unless (ast/is-lvalue lhs)
         (error "Assignment ~a = ~a expects lvalue"
                (ast/original-source-string lhs)
                (ast/original-source-string rhs)))
       (when (typedesc/const (expression/type lhs))
         (error "Assignment of const in ~a = ~a ~a"
                (ast/original-source-string lhs)
                (ast/original-source-string rhs)
                (format-type (expression/type lhs))))
       (when (type/is-array (expression/type lhs))
         (error "In assignment ~a cannot assign to expression of array type"
                (ast/original-source-string ast)))
       (when (type/is-func (expression/type lhs))
         (error "In assignment ~a cannot assign to expression of function type"
                (ast/original-source ast)))
       (unless (type/assignable-to (expression/type lhs) (expression/type rhs))
         (error "In assignment ~a = ~a: type ~a cannot be converted to type ~a"
                (ast/original-source-string lhs)
                (ast/original-source-string rhs)
                (format-type (expression/type rhs))
                (format-type (expression/type lhs))))
       (setf (expression/type ast)
             (expression/type lhs)))
      (_
       (let ((lhs-int (type/is-integral lhs))
             (rhs-int (type/is-integral rhs)))
         (unless (and lhs-int
                      rhs-int

                      ;; Is this the right predicate to use here?
                      (type/assignable-to lhs-int rhs-int))
           (error "Type mismatch in operator ~a: Expression ~a and ~a are not integral or are different (~a vs ~a)"
                  (ast/original-source-string ast)
                  (ast/original-source-string lhs)
                  (ast/original-source-string rhs)
                  (format-type (expression/type lhs))
                  (format-type (expression/type rhs))))
         (setf (expression/type ast)
               (expression/type lhs)))))))

(defun compiler.check-types-compound (compiler ast)
  (mapc (bind #'compiler.check-types compiler)
        (compound/statements ast)))

(defun compiler.check-types-if (compiler ast)
  (with-slots (condition body else) ast
    (compiler.check-types compiler condition)
    (assert (type/is-integral condition))

    (compiler.check-types compiler body)

    (when else
      (compiler.check-types compiler else))))

(defun compiler.check-types-ptr-deref (compiler ast operand)
  (compiler.check-types compiler operand)

  (let ((deref-type (type/is-pointer (expression/type operand))))
    (unless deref-type
      (error "Unary * expects pointer type, got ~a in ~a"
             (format-type (expression/type operand))
             (ast/original-source-string ast)))
    (setf (expression/type ast) deref-type)))

(defun compiler.check-types-address-of (compiler ast operand)
  (compiler.check-types compiler operand)

  (let ((opnd-type (expression/type operand)))
    (unless (ast/is-lvalue operand)
      (error "Address-of operator expects lvalue in ~a"
             (ast/original-source-string operand)))

    (setf (expression/type ast)
          ($ typedesc.pointer-desc :base-type opnd-type))))

(defun compiler.check-types-unop (compiler ast)
  (with-slots (operand operator) ast
    (ematch operator
      ("*"
       (compiler.check-types-ptr-deref compiler ast operand))
      ("&"
       (compiler.check-types-address-of compiler ast operand)))))

(defun compiler.check-types-array-sub (compiler ast)
  (with-slots (base sub) ast
    (compiler.check-types compiler base)
    (compiler.check-types compiler sub)

    (ematch (expression/type base)
      ((or (typedesc.pointer-desc :base-type base) (typedesc.array-desc :base-type base))
       (setf (expression/type ast) base))
      (_
       (error "In array subscript ~a invalid type"
              (ast/original-source-string ast)
              (format-type (expression/type base)))))))

(defun compiler.check-types (compiler ast)
  (ematch ast
    ((ast.expression.array-sub)
     (compiler.check-types-array-sub compiler ast))
    ((ast.expression.string-literal)
     (compiler.check-types-string-literal compiler ast))
    ((ast.statement.var-def)
     (compiler.check-types-var-def compiler ast))
    ((ast.statement.if)
     (compiler.check-types-if compiler ast))
    ((ast.statement.compound)
     (compiler.check-types-compound compiler ast))
    ((ast.statement.function)
     (compiler.check-types-function compiler ast))
    ((ast.statement.while)
     (compiler.check-types-while compiler ast))
    ((ast.statement.struct)
     (compiler.check-types-struct compiler ast))
    ((ast.expression.prop-access :base base :member member)
     (compiler.check-types-prop compiler ast base member))
    ((ast.expression.identifier)
     (compiler.check-types-identifier compiler ast))
    ((ast.expression.number)
     (compiler.check-types-number compiler ast))
    ((ast.expression.character)
     (compiler.check-types-character compiler ast))
    ((ast.expression.binop)
     (compiler.check-types-binop compiler ast))
    ((ast.expression.funcall)
     (compiler.check-types-funcall compiler ast))
    ((ast.expression.unop)
     (compiler.check-types-unop compiler ast))))

(defun read-property-access (stream char)
  `(rx-kw:|prop| ,@(read-delimited-list #\] stream)))

(defun right-bracket-read-error (stream char)
  (error "Unexpected ]"))

(defparameter *rx-readtable* (copy-readtable))

(setf (readtable-case *rx-readtable*) :preserve)

(set-macro-character #\[ #'read-property-access nil *rx-readtable*)

(set-macro-character #\] #'right-bracket-read-error nil *rx-readtable*)

(defun main ()
  (with-open-file (s "test.rx")
    (let ((*package* (find-package :rx-user))
          (*readtable* *rx-readtable*))
      (let ((compiler (new-compiler)))
        (loop for x = (read s nil 'eof )
              while (not (eq x 'eof))
              do
                 (let ((ast (compiler.parse compiler x)))
                   (compiler.check-types compiler ast)
                   (format t "~a~%" (big-print-clos-object ast))
                   (format t "~a~%"
                           (with-output-to-string (str)
                             (sb-ext:run-program
                              "/usr/bin/clang-format"
                              '()
                              :input (make-string-input-stream (format-ast ast t))
                              :output str)))))
                                        ;    (loop for x across *output* do
                                        ;      (format t "~a~%" (pretty-print-clos-object x))
                                        ;      (format t "~a~%" (format-ast x t)))

        ))))
