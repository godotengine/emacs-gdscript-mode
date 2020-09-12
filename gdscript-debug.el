;;; gdscript-debug.el --- Description -*- lexical-binding: t; -*-
;;
;; Inspired by gdb-mi

(require 'bindat)
(require 'generator)
(require 'gdscript-customization)

;; Overlay arrow markers
(defvar gdscript-debug--thread-position nil)

(defvar gdscript-debug--null-spec)

(defvar gdscript-debug--boolean-spec
  '((:boolean-data u32r)))

(defvar gdscript-debug--integer-spec
  '((:integer-data u32r)))

(defvar gdscript-debug--integer-64-spec
  '((:data-a u32r)
    (:data-b u32r)
    (:integer-data eval (let ((a (bindat-get-field struct :data-a))
                              (b (bindat-get-field struct :data-b)))
                          (logior (lsh b 32) a)))))

;; Credit goes to https://github.com/skeeto/bitpack/blob/master/bitpack.el
(defsubst gdscript-debug--load-f32 (b0 b1 b2 b3)
  (let* ((negp (= #x80 (logand b0 #x80)))
         (exp (logand (logior (ash b0 1) (ash b1 -7)) #xff))
         (mantissa (logior #x800000
                           (ash (logand #x7f b1) 16)
                           (ash b2 8)
                           b3))
         (result (cond ((= #xff exp)
                        (if (= #x800000 mantissa)
                            1.0e+INF
                          0.0e+NaN))
                       ((= #x0 exp b1 b2 b3) 0.0)
                       (t (ldexp (ldexp mantissa -24) (- exp 126))))))
    (if negp
        (- result)
      result)))

(defsubst gdscript-debug--load-f64 (b0 b1 b2 b3 b4 b5 b6 b7)
  (let* ((negp (= #x80 (logand b0 #x80)))
         (exp (logand (logior (ash b0 4) (ash b1 -4)) #x7ff))
         (mantissa (logior #x10000000000000
                           (ash (logand #xf b1) 48)
                           (ash b2 40)
                           (ash b3 32)
                           (ash b4 24)
                           (ash b5 16)
                           (ash b6  8)
                           b7))
         (result (if (= #x7ff exp)
                     (if (= #x10000000000000 mantissa)
                         1.0e+INF
                       0.0e+NaN)
                   (ldexp (ldexp mantissa -53) (- exp 1022)))))
    (if negp
        (- result)
      result)))

(defsubst gdscript-debug--to-symbol (symbol-name &optional suffix)
  (intern (concat (symbol-name symbol-name) suffix)))

(defmacro gdscript-debug--capture-float-spec (symbol-name)
  (let ((symbol (gdscript-debug--to-symbol symbol-name)))
    `(quote ((:vect vec 4 byte)
             (,symbol eval (let ((alist (reverse (append (bindat-get-field struct :vect) nil))))
                             (apply 'gdscript-debug--load-f32 alist)))))))

(defvar gdscript-debug--float-spec
  '((:vect vec 4 byte)
    (:float-value eval (let ((alist (reverse (append (bindat-get-field struct :vect) nil))))
                         (apply 'gdscript-debug--load-f32 alist)))))

(defvar gdscript-debug--float-64-spec
  '((:vect vec 8 byte)
    (:float-value eval (let ((alist (reverse (append (bindat-get-field struct :vect) nil))))
                         (apply 'gdscript-debug--load-f64 alist)))))

(defvar gdscript-debug--string-spec
  '((:data-length u32r)
    (:string-data str (:data-length))
    (align 4)))

(defvar gdscript-debug--string-z-spec
  '((:data-length u32r)
    (:string-data strz (:data-length))
    (align 4)))

(defvar gdscript-debug--vector2-spec
  `(,@(gdscript-debug--capture-float-spec :x)
    ,@(gdscript-debug--capture-float-spec :y)))

(defvar gdscript-debug--vector3-spec
  `(,@(gdscript-debug--capture-float-spec :x)
    ,@(gdscript-debug--capture-float-spec :y)
    ,@(gdscript-debug--capture-float-spec :z)))

(defvar gdscript-debug--transform2d-spec
  `(,@(gdscript-debug--capture-float-spec :xx)
    ,@(gdscript-debug--capture-float-spec :yx)
    ,@(gdscript-debug--capture-float-spec :xy)
    ,@(gdscript-debug--capture-float-spec :yy)
    ,@(gdscript-debug--capture-float-spec :x-origin)
    ,@(gdscript-debug--capture-float-spec :y-origin)))

(defvar gdscript-debug--dictionary-spec
  '((:data u32r)
    (:shared   eval (logand (bindat-get-field struct :data) #x80000000))
    (:elements eval (logand (bindat-get-field struct :data) #x7fffffff))
    (:dictionary-length eval (* 2 last))
    (:items repeat (:dictionary-length) (struct godot-data-bindat-spec))))

(defvar gdscript-debug--array-spec
  '((:data u32r)
    (:shared       eval (logand (bindat-get-field struct :data) #x80000000))
    (:array-length eval (logand (bindat-get-field struct :data) #x7fffffff))
    (:items repeat (:array-length) (struct godot-data-bindat-spec))))

(defvar gdscript-debug--pool-int-array-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--integer-spec))))

(defvar gdscript-debug--pool-string-array-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--string-z-spec))))

(defvar gdscript-debug--pool-vector-2-array-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--vector2-spec))))

(defvar gdscript-debug--pool-color-array-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--color-spec))))

;;(print (macroexpand '(gdscript-debug--capture-float-spec :hi)))

(defvar gdscript-debug--plane-spec
  `(,@(gdscript-debug--capture-float-spec :normal-x)
    ,@(gdscript-debug--capture-float-spec :normal-y)
    ,@(gdscript-debug--capture-float-spec :normal-z)
    ,@(gdscript-debug--capture-float-spec :distance)))

(defvar gdscript-debug--aabb-spec
  `(,@(gdscript-debug--capture-float-spec :x-coordinate)
    ,@(gdscript-debug--capture-float-spec :y-coordinate)
    ,@(gdscript-debug--capture-float-spec :z-coordinate)
    ,@(gdscript-debug--capture-float-spec :x-size)
    ,@(gdscript-debug--capture-float-spec :y-size)
    ,@(gdscript-debug--capture-float-spec :z-size)))

(defvar gdscript-debug--basis-spec
  `(,@(gdscript-debug--capture-float-spec :xx)
    ,@(gdscript-debug--capture-float-spec :yx)
    ,@(gdscript-debug--capture-float-spec :zx)
    ,@(gdscript-debug--capture-float-spec :xy)
    ,@(gdscript-debug--capture-float-spec :yy)
    ,@(gdscript-debug--capture-float-spec :zy)
    ,@(gdscript-debug--capture-float-spec :xz)
    ,@(gdscript-debug--capture-float-spec :yz)
    ,@(gdscript-debug--capture-float-spec :zz)))

(defvar gdscript-debug--color-spec
  `(,@(gdscript-debug--capture-float-spec :red)
    ,@(gdscript-debug--capture-float-spec :green)
    ,@(gdscript-debug--capture-float-spec :blue)
    ,@(gdscript-debug--capture-float-spec :alpha)))

(defvar gdscript-debug--node-path-spec
  '((:data-length u32r)
    (:new-format eval (logand (bindat-get-field struct :data-length) #x80000000))
    (:name-count eval (logand (bindat-get-field struct :data-length) #x7FFFFFFF))
    (:sub-name-count u32)
    (:total eval (+ (bindat-get-field struct :name-count) (bindat-get-field struct :sub-name-count) ))
    (:flags u32)
    (:absolute eval (not (eq 0 (logand (bindat-get-field struct :flags) #x1))))
    (:items repeat (:total) (struct gdscript-debug--string-spec))))

(defvar gdscript-debug--rid-spec nil) ;; unsupported

(defvar gdscript-debug--object-as-id
  '((:object-as-id-a u32r)
    (:object-as-id-b u32r)
    (:long eval (let ((a (bindat-get-field struct :object-as-id-a))
                      (b (bindat-get-field struct :object-as-id-b)))
                  (logior (lsh b 32) a)))))

(defconst gdscript-debug--encode-mask #xff)
(defconst gdscript-debug--encode-flag-64 (lsh 1 16))

(defvar godot-data-bindat-spec
  '((:type-data    u32r)
    (:type         eval (logand last gdscript-debug--encode-mask))
    (:flag-64      eval (logand (bindat-get-field struct :type-data) gdscript-debug--encode-flag-64))
    (:object-as-id eval (logand (bindat-get-field struct :type-data) gdscript-debug--encode-flag-64))
    (union (:type)
           (0 nil)
           (1 (struct gdscript-debug--boolean-spec))
           ((eval (and (eq 2 tag) (equal 0 (bindat-get-field struct :flag-64)))) (struct gdscript-debug--integer-spec))
           (2 (struct gdscript-debug--integer-64-spec))
           ((eval (and (eq 3 tag) (equal 0 (bindat-get-field struct :flag-64)))) (struct gdscript-debug--float-spec))
           (3 (struct gdscript-debug--float-64-spec))
           (4 (struct gdscript-debug--string-spec))
           (5 (struct gdscript-debug--vector2-spec))
           (7 (struct gdscript-debug--vector3-spec))
           (8 (struct gdscript-debug--transform2d-spec))
           (9 (struct gdscript-debug--plane-spec))
           (11 (struct gdscript-debug--aabb-spec))
           (12 (struct gdscript-debug--basis-spec))
           (14 (struct gdscript-debug--color-spec))
           (15 (struct gdscript-debug--node-path-spec))
           (16 (struct gdscript-debug--rid-spec))
           ((eval (and (eq 17 tag) (equal 0 (bindat-get-field struct :object-as-id)))) (error "[ObjectId] Not implemented yet"))
           (17 (struct gdscript-debug--object-as-id))
           (18 (struct gdscript-debug--dictionary-spec))
           (19 (struct gdscript-debug--array-spec))
           (21 (struct gdscript-debug--pool-int-array-spec))
           (23 (struct gdscript-debug--pool-string-array-spec))
           (24 (struct gdscript-debug--pool-vector-2-array-spec))
           (26 (struct gdscript-debug--pool-color-array-spec))
           (t (eval (error "Unknown type: %s" tag))))))

(defvar gdscript-debug--previous-packet-data nil)
(defvar gdscript-debug--data-needed nil)
(defvar gdscript-debug--offset 0)
(defvar gdscript-debug--inspected-objects (make-hash-table))

(defvar gdscript-debug--packet-length-bindat-spec
  '((:packet-length u32r)))

(defun gdscript-debug--current-packet (content offset)
  (bindat-unpack gdscript-debug--packet-length-bindat-spec content offset))

(defun gdscript-debug--process-packet (content offset)
  (bindat-unpack godot-data-bindat-spec content offset))

(iter-defun gdscript-debug--command-iter ()
  (setq gdscript-debug--data-needed
        (catch 'not-enough-data-to-process-packed
          (while (< gdscript-debug--offset (length gdscript-debug--previous-packet-data))
            (let* ((content gdscript-debug--previous-packet-data)
                   (content-length (length content))
                   (packet-length-data (gdscript-debug--current-packet content gdscript-debug--offset))
                   (packet-length (bindat-get-field packet-length-data :packet-length))
                   (next-packet-offset (+ 4 gdscript-debug--offset packet-length)))
              (if (<= next-packet-offset content-length)
                  (let ((packet-data (gdscript-debug--process-packet content (+ 4 gdscript-debug--offset))))
                    (setq gdscript-debug--offset next-packet-offset)
                    (iter-yield packet-data))
                (throw 'not-enough-data-to-process-packed next-packet-offset))))))
  (setq gdscript-debug--offset 0))

(defsubst get-boolean (struct-data)
  (bindat-get-field struct-data :boolean-data))

(defsubst get-integer (struct-data)
  (bindat-get-field struct-data :integer-data))

(defsubst get-float (struct-data)
  (bindat-get-field struct-data :float-value))

(defsubst get-string (struct-data)
  (bindat-get-field struct-data :string-data))

(defsubst get-array (struct-data)
  (bindat-get-field struct-data :items))

(defsubst to-plane (struct)
  (let ((normal-x (bindat-get-field struct :normal-x))
        (normal-y (bindat-get-field struct :normal-y))
        (normal-z (bindat-get-field struct :normal-z))
        (distance (bindat-get-field struct :distance)))
    (plane-create
     :normal-x normal-x
     :normal-y normal-y
     :normal-z normal-z
     :distance distance)))

(defsubst to-aabb (struct)
  (let ((x-coordinate (bindat-get-field struct :x-coordinate))
        (y-coordinate (bindat-get-field struct :y-coordinate))
        (z-coordinate (bindat-get-field struct :z-coordinate))
        (x-size (bindat-get-field struct :x-size))
        (y-size (bindat-get-field struct :y-size))
        (z-size (bindat-get-field struct :z-size)))
    (aabb-create
     :x-coordinate x-coordinate
     :y-coordinate y-coordinate
     :z-coordinate z-coordinate
     :x-size x-size
     :y-size y-size
     :z-size z-size)))

(defsubst to-basis (struct)
  (let ((xx (bindat-get-field struct :xx))
        (yx (bindat-get-field struct :yx))
        (zx (bindat-get-field struct :zx))
        (xy (bindat-get-field struct :xy))
        (yy (bindat-get-field struct :yy))
        (zy (bindat-get-field struct :zy))
        (xz (bindat-get-field struct :xz))
        (yz (bindat-get-field struct :yz))
        (zz (bindat-get-field struct :zz)))
    (basis-create
     :x (vector3-create :x xx :y yx :z zx)
     :y (vector3-create :x xy :y yy :z zy)
     :z (vector3-create :x xz :y yz :z zz))))

(defsubst to-color (struct)
  (let ((red (bindat-get-field struct :red))
        (green (bindat-get-field struct :green))
        (blue (bindat-get-field struct :blue))
        (alpha (bindat-get-field struct :alpha)))
    (color-create :red red :green green :blue blue :alpha alpha)))

(defsubst to-node-path (struct)
  (let ((path (mapcar 'to-string (bindat-get-field struct :items)))
        (subpath nil) ;; TODO what is subpath
        (absolute (bindat-get-field struct :absolute)))
    (node-path-create :path path :subpath subpath :absolute absolute)))

(defsubst to-rid (struct-data)
  (rid-create))

(defsubst to-vector2 (struct-data)
  (let ((x (bindat-get-field struct-data :x))
        (y (bindat-get-field struct-data :y)))
    (vector2-create :x x :y y)))

(defsubst to-vector3 (struct-data)
  (let ((x (bindat-get-field struct-data :x))
        (y (bindat-get-field struct-data :y))
        (z (bindat-get-field struct-data :z)))
    (vector3-create :x x :y y :z z)))

(defsubst to-transform2d (struct-data)
  (let ((xx (bindat-get-field struct-data :xx))
        (yx (bindat-get-field struct-data :yx))
        (xy (bindat-get-field struct-data :xy))
        (yy (bindat-get-field struct-data :yy))
        (x-origin (bindat-get-field struct-data :x-origin))
        (y-origin (bindat-get-field struct-data :y-origin)))
    (transform2d-create
     :x (vector2-create :x xx :y yx)
     :y (vector2-create :x xy :y yy)
     :origin (vector2-create :x :x-origin :y :y-origin))))

(defsubst to-null (struct-data)
  (prim-null-create))

(defsubst to-boolean (struct-data)
  (prim-bool-create :value (if (eq 1 (get-boolean struct-data))
                               t
                             nil)))

(defsubst shared-to-boolean (shared)
  (prim-bool-create :value (if (eq 0 shared) nil t)))

(defsubst to-integer (struct-data)
  (prim-integer-create :value (get-integer struct-data)))

(defsubst to-float (struct-data)
  (prim-float-create :value (get-float struct-data)))

(defsubst to-string (struct-data)
  (prim-string-create :value (get-string struct-data)))

(defsubst to-object-id (struct-data)
  (object-id-create :value (bindat-get-field struct-data :long)))

(defsubst to-dictionary (struct-data)
  (let* ((shared (bindat-get-field struct-data :shared))
         (items (bindat-get-field struct-data :items)))
    (dictionary-create :shared (shared-to-boolean shared) :elements (to-dic items))))

(defun to-dic (xs)
  (cl-loop for (key value) on xs by 'cddr
           collect (from-key-value key value)))

(defsubst to-array (struct-data)
  (let ((shared (bindat-get-field struct-data :shared))
        (items (bindat-get-field struct-data :items)))
    (prim-array-create :shared (shared-to-boolean shared) :elements (mapcar 'from-variant items))))

(defsubst to-pool-int-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (pool-int-array-create :elements (mapcar 'to-integer items))))

(defsubst to-pool-string-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (pool-string-array-create :elements (mapcar 'to-string items))))

(defsubst to-pool-vector2-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (pool-vector2-array-create :elements (mapcar 'to-vector2 items))))

(defsubst to-pool-color-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (pool-color-array-create :elements (mapcar 'to-color items))))

(defun from-key-value (key value)
  (let* ((var-name (bindat-get-field key :string-data))
         (var-val (from-variant value)))
    `(,var-name . ,var-val)))

(defun from-variant (struct)
  (let ((type (bindat-get-field struct :type))
        (object-as-id (bindat-get-field struct :object-as-id)))
    (pcase type
      (0 (to-null struct))
      (1 (to-boolean struct))
      (2 (to-integer struct))
      (3 (to-float struct))
      (4 (to-string struct))
      (5 (to-vector2 struct))
      (7 (to-vector3 struct))
      (8 (to-transform2d struct))
      (9 (to-plane struct))
      (11 (to-aabb struct))
      (12 (to-basis struct))
      (14 (to-color struct))
      (15 (to-node-path struct))
      (16 (to-rid struct))
      (17 (if (eq 0 object-as-id) (error "TODO object as not ID")
            (to-object-id struct)))
      (18 (to-dictionary struct))
      (19 (to-array struct))
      (21 (to-pool-int-array struct))
      (23 (to-pool-string-array struct))
      (24 (to-pool-vector2-array struct))
      (26 (to-pool-color-array struct))
      (_ (error "[from-variant] Unknown type %s" type)))))

(defun to-stack-dump (stack-data level)
  (pcase stack-data
    (`(,file-key, file-value, line-key, line-value, function-key, function-value, id-key, id-value)
     (stack-dump-create
      :file (get-string file-value)
      :line (get-integer line-value)
      :function-name (get-string function-value)
      :level level))))

(defun error-data-to-plist (error-data)
  (pcase error-data
    (`(,hr, min, sec, msec, source-func, source-file, source-line, error-msg, error-descr, warning)
     `(hr ,(get-integer hr)
          min ,(get-integer min)
          sec ,(get-integer sec)
          msec ,(get-integer msec)
          source-func ,(get-string source-func)
          source-file ,(get-string source-file)
          source-line ,(get-integer source-line)
          error-msg ,(get-string error-msg)
          error-descr ,(get-string error-descr)
          warning, (get-boolean warning)))))

(defun mk-error (iter)
  (let ((callstack-size (bindat-get-field (iter-next iter) :integer-data))
        (error-data (bindat-get-field (iter-next iter) :items))
        (error-callstack-size (bindat-get-field (iter-next iter) :integer-data))
        ;; TODO process call stack
        ;; (error-callstack-size (bindat-get-field (iter-next iter) :integer-data))
        )
    `(command "error" callstack-size ,callstack-size error-data ,(error-data-to-plist error-data) error-callstack-size, error-callstack-size)))

(defun mk-performance (iter)
  (let ((skip-this (iter-next iter))
        (performance-data (bindat-get-field (iter-next iter) :items)))
    `(command "performace" performance-data ,performance-data)))

(defun read-var-names (iter count)
  (let ((variables))
    (dotimes (i count)
      (let* ((var-name (bindat-get-field (iter-next iter) :string-data))
             (var-value (iter-next iter))
             (var-val (from-variant var-value)))
        (push `(,var-name . ,var-val) variables)))
    (reverse variables)))

(defun mk-stack-frame-vars (iter)
  (let* ((total-size (get-integer (iter-next iter)))
         (locals-size (get-integer (iter-next iter)))
         (locals (read-var-names iter locals-size))
         (members-size (get-integer (iter-next iter)))
         (members (read-var-names iter members-size))
         (globals-size (get-integer (iter-next iter)))
         (globals (read-var-names iter globals-size)))
    (stack-frame-vars-create :locals locals :members members :globals globals)))

(defun to-property-info (properties)
  (let ((property-info))
    (dolist (property properties)
      (cond ((eq 6 (bindat-get-field property :array-length))
             (let* ((data (bindat-get-field property :items))
                    (name (bindat-get-field (car data) :string-data))
                    (type (bindat-get-field (nth 1 data) :integer-data))
                    (hint (bindat-get-field (nth 2 data) :integer-data))
                    (hint-string (bindat-get-field (nth 3 data) :string-data))
                    (usage (bindat-get-field (nth 4 data) :integer-data))
                    (variant (from-variant (nth 5 data)))
                    (new-prop (property-info-create
                               :name name
                               :type type
                               :hint hint
                               :hint-string hint-string
                               :usage usage
                               :variant variant)))
               (push new-prop property-info)))
            (t (message "Ignoring property %s" property))))
    property-info))

(defun mk-inspect-object (iter)
  (let ((three (get-integer (iter-next iter)))
        (object-id (get-integer (iter-next iter)))
        (class (get-string (iter-next iter)))
        (properties (get-array (iter-next iter))))
    (inspect-object-create :object-id object-id :class class :properties (to-property-info properties))))

(defun mk-stack-dump (iter)
  (let ((stack-level-count (get-integer (iter-next iter)))
        (outputs))
    (dotimes (level stack-level-count)
      (let ((stack-data (bindat-get-field (iter-next iter) :items)))
        (push (to-stack-dump stack-data level) outputs)))
    (reverse outputs)))

(defun mk-output (iter)
  (let ((output-count (bindat-get-field (iter-next iter) :integer-data))
        (outputs))
    ;(message "output-count: %s %s" output-count (type-of output-count))
    (dotimes (i output-count)
      (let* ((data (iter-next iter))
             (output (bindat-get-field data :items 0 :string-data)))
        (setq outputs (cons output outputs))))
    `(command "output" outputs, outputs)))

(defun mk-debug-enter (iter)
  (let ((skip-this (iter-next iter))
        (can-continue (bindat-get-field (iter-next iter) :boolean-data))
        (reason (bindat-get-field (iter-next iter) :string-data)))
    (debug-enter-create :can-continue can-continue :reason reason)))

(defun mk-debug-exit (iter)
  (let ((skip-this (iter-next iter)))
    '(command "debug_exit")))

(defun line-posns (line)
  "Return a pair of LINE beginning and end positions."
  (let ((offset (1+ (- line (line-number-at-pos)))))
    (cons
     (line-beginning-position offset)
     (line-end-position offset))))

(defsubst gdscript-debug--drop-res (file-path)
  (substring file-path (length "res://")))

(defun gdscript-debug--on-stack-dump (stack-dump project-root)
  (let* ((file (stack-dump->file stack-dump))
         (line (stack-dump->line stack-dump))
         (full-file-path (concat project-root (gdscript-debug--drop-res file))))
    (if (not project-root)
        (error "Project for file %s not found." file)
      (with-current-buffer (find-file full-file-path)
        (let* ((posns (line-posns line))
               (start-posn (car posns)))
          (set-marker gdscript-debug--thread-position start-posn (current-buffer))
          (goto-char gdscript-debug--thread-position))))))

(defmacro gdscript-debug--command-handler (&rest body)
  `(progn
     ,@body
     (setq gdscript-debug--previous-packet-data
           (substring gdscript-debug--previous-packet-data gdscript-debug--offset (length gdscript-debug--previous-packet-data)))
     (setq gdscript-debug--offset 0)))

(defun gdscript-debug--handle-server-reply (process content)
  "Gets invoked whenever the server sends data to the client."
  ;;(message "(DATA received): %s" (length content))
  ;;(message "(Old DATA): %s" (length gdscript-debug--previous-packet-data))
  (setq gdscript-debug--previous-packet-data (concat gdscript-debug--previous-packet-data content))
  (when (or (null gdscript-debug--data-needed)
            (<= gdscript-debug--data-needed (length gdscript-debug--previous-packet-data)))
    (condition-case x
        (let ((iter (gdscript-debug--command-iter)))
          (while t
            (let* ((next-data (iter-next iter))
                   (str (bindat-get-field next-data :string-data)))
              (pcase str
                ("debug_enter"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'debug_enter' command")
                  (let ((cmd (mk-debug-enter iter)))
                    ;;(message "-- cmd: %s" cmd)
                    (pcase (debug-enter->reason cmd)
                      ("Breakpoint"
                       (gdscript-debug-get-stack-dump)
                       (message "Breakpoint encountered."))
                      (other
                       (gdscript-debug-get-stack-dump)
                       (message "%s" other))))))
                ("debug_exit"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'debug_exit' command")
                  (let ((cmd (mk-debug-exit iter)))
                    ;; (message "Debug_exit: %s " cmd)
                    )))
                ("output"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'output' command")
                  (let ((cmd (mk-output iter)))
                    ;;(message "Output: %s" (plist-get cmd 'outputs))
                    ;; (dolist (element (plist-get cmd 'outputs))
                    ;;   (message "output: %s" element))
                    )))
                ("error"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'error' command")
                  (let ((cmd (mk-error iter)))
                    ;;(message "Error: %s" cmd)
                    )))
                ("performance"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'performance' command")
                  (let ((cmd (mk-performance iter)))
                    ;; (message "Performace: %s" cmd)
                    )))
                ("stack_dump"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'stack_dump' command")
                  (let ((cmd (mk-stack-dump iter)))
                    ;;(message "[stack_dump] cmd: %s" cmd)
                    (gdscript-debug--refresh-stack-frame-vars-buffer cmd (process-get process 'project))
                    (gdscript-debug--on-stack-dump (car cmd) (process-get process 'project))
                    (gdscript-debug-get-stack-frame-vars (stack-dump->level (car cmd))))))
                ("stack_frame_vars"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'stack_frame_vars' command")
                  (let ((cmd (mk-stack-frame-vars iter)))
                    (with-current-buffer (gdscript-debug--get-stack-frame-vars-buffer)
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert "Locals:\n")
                        (dolist (local (stack-frame-vars->locals cmd))
                          (insert (gdscript-debug--variable-name (car local)))
                          (insert (format ": %s\n" (gdscript-debug--stringify (cdr local))))
                          (gdscript-debug--fetch-object-id-data (cdr local)))
                        (insert "\nMembers:\n")
                        (dolist (member (stack-frame-vars->members cmd))
                          (insert (gdscript-debug--variable-name (car member)))
                          (insert (format ": %s\n" (gdscript-debug--stringify (cdr member))))
                          (gdscript-debug--fetch-object-id-data (cdr member)))
                        (insert "\nGlobals:\n")
                        (dolist (global (stack-frame-vars->globals cmd))
                          (insert (gdscript-debug--variable-name (car global)))
                          (insert (format ": %s\n" (gdscript-debug--stringify (cdr global))))))
                      (display-buffer (current-buffer)))
                    ;; (message "Stack frame vars %s" cmd)
                    )))
                ("message:inspect_object"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'message:inspect_object' command")
                  (let ((cmd (mk-inspect-object iter)))
                    (with-current-buffer (gdscript-debug--get-stack-frame-vars-buffer)
                      (let* ((object-id (inspect-object->object-id cmd))
                             (object-id-property (gdscript-debug--object-id-property object-id))
                             (position-start 1)
                             (position-end 1))
                        (puthash object-id cmd gdscript-debug--inspected-objects)
                        (while position-start
                          (setq position-start (next-single-property-change position-start object-id-property))
                          (when position-start
                            (setq position-end (next-single-property-change position-start object-id-property))
                            (let ((inhibit-read-only t)
                                  (class-name (concat (propertize (inspect-object->class cmd) 'face font-lock-type-face) " - "))
                                  (properties (list 'object-id object-id object-id-property nil)))
                              (save-excursion
                                (goto-char position-start)
                                (insert class-name)
                                (let ((new-position-end (+ 1 position-end (length class-name))))
                                  (add-text-properties position-start new-position-end properties)
                                  (setq position-start new-position-end))))))))
                    (gdscript-debug--refresh-inspector-buffer cmd))))
                (_ (error "Unknown command %s" str))))))
      ;;(iter-end-of-sequence (message "No more packets to process %s" x))
      (iter-end-of-sequence nil))))

(defun gdscript-debug--object-id-property (object-id)
  (intern (format "object-id-%s" object-id)))

(defun gdscript-debug--stringify (object)
  (cond ((object-id-p object)
         (let* ((object-id (object-id->value object))
                (object-id-data (gethash object-id gdscript-debug--inspected-objects)))
           (if object-id-data
               (let ((class-name (concat (propertize (inspect-object->class object-id-data) 'face font-lock-type-face) " - ")))
                 (propertize (concat class-name (format "ObjectID: %s" (number-to-string object-id)))
                             'object-id object-id))
             (propertize (format "ObjectID: %s" (number-to-string object-id))
                         'object-id object-id
                         (gdscript-debug--object-id-property object-id) t))))
        (t object)))

(defun gdscript-debug--variable-name (var-name)
  (propertize (format "%25s" var-name) 'font-lock-face font-lock-variable-name-face))

(defun gdscript-debug--fetch-object-id-data (object)
  (when (and
         (object-id-p object)
         (null (gethash (object-id->value object) gdscript-debug--inspected-objects)))
    (gdscript-debug-inspect-object (object-id->value object))))

(defvar server-clients '()
  "List with client processes")

(defun gdscript-debug--sentinel-function (process event)
  "Gets called when the status of the network connection changes."
  (message "[sentinel] process: %s" process)
  (message "[sentinel] event  : %s" event)
  (cond
   ((string-match "open from .*\n" event)
    (push process server-clients))
   ((or
     (string= event "connection broken by remote peer\n")
     (string= event "deleted\n"))
    (message "Resetting server to accept data.")
    (set-marker gdscript-debug--thread-position nil)
    (setq gdscript-debug--previous-packet-data nil
          gdscript-debug--offset 0
          gdscript-debug--data-needed nil
          gdscript-debug--inspected-objects (make-hash-table)
          server-clients '()))
   ((eq (process-status process) 'closed)
    (message "EHHHH ???"))))

(defmacro gdscript-debug--send-command (&rest body)
  "Todo"
  (declare (indent 0) (debug t))
  `(pcase server-clients
     (`() (message "No game process is running."))
     (`(,server-process)
      (let ((command (progn ,@body)))
        (process-send-string server-process command)))
     (_ (message "More than one game process running"))))

(defmacro gdscript-debug--if-server-process (&rest body)
  "Todo"
  (declare (indent 0) (debug t))
  `(pcase server-clients
     (`(,server-process) (progn ,@body))))

;;(print (macroexpand '(gdscript-debug--send-command server-process (message "HIII %s" server-process))))

(defun gdscript-debug-inspect-object(object-id)
  (gdscript-debug--send-command
    (gdscript-debug--inspect-object object-id)))

(defun gdscript-debug-get-stack-dump()
  (interactive)
  (gdscript-debug--send-command (gdscript-debug--command "get_stack_dump")))

(defun gdscript-debug-continue()
  (interactive)
  (gdscript-debug--send-command (gdscript-debug--command "continue")))

(defun gdscript-debug-next()
  (interactive)
  (gdscript-debug--send-command (gdscript-debug--command "next")))

(defun gdscript-debug-step()
  (interactive)
  (gdscript-debug--send-command (gdscript-debug--command "step")))

(defun gdscript-debug-process-name (project-root)
  (format "*Godot debugger %s*" project-root))

;;;###autoload
(defun gdscript-debug-make-server()
  (interactive)

  (setq gdscript-debug--thread-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'gdscript-debug--thread-position)

  (let ((project-root (gdscript-util--find-project-configuration-file)))
    (if (not project-root)
        (error "Not in Godot project!")
      (let ((server-process
             (make-network-process
              :name (gdscript-debug-process-name project-root)
              :buffer nil
              :server t
              :host "127.0.0.1"
              :service gdscript-debug-port
              :coding 'binary
              :family 'ipv4
              :filter #'gdscript-debug--handle-server-reply
              :filter-multibyte nil
              :sentinel #'gdscript-debug--sentinel-function)))
        (process-put server-process 'project project-root)
        (message "Debugger server started - project: %s" project-root)))))

(defun gdscript-debug--inspect-object-definition (command-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:command-type u32r)
    (:command-length u32r)
    (:command str ,command-length)
    (align 4)
    (:object-id-type u32r)
    (:object-id u32r)))

(defun gdscript-debug--get-stack-frame-vars-definition (command-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:command-type u32r)
    (:command-length u32r)
    (:command str ,command-length)
    (align 4)
    (:frame-type u32r)
    (:frame u32r)))

(defun gdscript-debug--breakpoint-packet-definition (command-length file-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:command-type u32r)
    (:command-length u32r)
    (:command str ,command-length)
    (align 4)
    (:file-type u32r)
    (:file-length u32r)
    (:file str ,file-length)
    (align 4)
    (:line-type u32r)
    (:line u32r)
    (:boolean-type u32r)
    (:boolean u32r)))

(defun gdscript-debug--set-skip-breakpoints-packet-definition (command-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:command-type u32r)
    (:command-length u32r)
    (:command str ,command-length)
    (align 4)
    (:boolean-type u32r)
    (:boolean u32r)))

(defconst variant-bool 1 "bool")
(defconst variant-integer 2 "integer")
(defconst variant-float 3 "float")
(defconst variant-string 4 "string")
(defconst variant-array 19 "array")

(defun boolean-to-integer (b)
  (if (null b) 0 1))

;; (print (symbol-function 'gdscript-debug--get-stack-frame-vars))

(defun gdscript-debug--inspect-object (object-id)
  (let* ((command "inspect_object")
         (command-length (length command))
         (command-alength (align-length command))
         (packet-length (+ (* 6 4) command-alength))
         (spec (gdscript-debug--inspect-object-definition command-length)))
    (bindat-pack spec
     `((:packet-length . ,packet-length)
       (:array-type . ,variant-array)
       (:elements-count . 2)
       (:command-type . ,variant-string)
       (:command-length . ,command-length)
       (:command . ,command)
       (:object-id-type . ,variant-integer)
       (:object-id . ,object-id)))))

(defun gdscript-debug--get-stack-frame-vars (frame)
  (let* ((command "get_stack_frame_vars")
         (command-length (length command))
         (command-alength (align-length command))
         (packet-length (+ (* 6 4) command-alength))
         (spec (gdscript-debug--get-stack-frame-vars-definition command-length)))
    (bindat-pack spec
     `((:packet-length . ,packet-length)
       (:array-type . ,variant-array)
       (:elements-count . 2)
       (:command-type . ,variant-string)
       (:command-length . ,command-length)
       (:command . ,command)
       (:frame-type . ,variant-integer)
       (:frame . ,frame)))))

(defun gdscript-debug--breakpoint-command (file line add-or-remove)
  (let* ((command "breakpoint")
         (command-length (length command))
         (command-alength (align-length command))
         (file-length (length file))
         (packet-length (+ (* 10 4) command-alength file-length))
         (spec (gdscript-debug--breakpoint-packet-definition command-length file-length)))
    (bindat-pack spec
     `((:packet-length . ,packet-length)
       (:array-type . ,variant-array)
       (:elements-count . 4)
       (:command-type . ,variant-string)
       (:command-length . ,command-length)
       (:command . ,command)
       (:file-type . ,variant-string)
       (:file-length . ,file-length)
       (:file . ,file)
       (:line-type . ,variant-integer)
       (:line . ,line)
       (:boolean-type . ,variant-bool)
       (:boolean . ,(boolean-to-integer add-or-remove))))))

(defun gdscript-debug--set-skip-breakpoints-command (skip)
  (let* ((command "set_skip_breakpoints")
         (command-length (length command))
         (command-alength (align-length command))
         (packet-length (+ (* 6 4) command-alength))
         (spec (gdscript-debug--set-skip-breakpoints-packet-definition command-length)))
    (bindat-pack spec
     `((:packet-length . ,packet-length)
       (:array-type . ,variant-array)
       (:elements-count . 2)
       (:command-type . ,variant-string)
       (:command-length . ,command-length)
       (:command . ,command)
       (:boolean-type . ,variant-bool)
       (:boolean . ,(boolean-to-integer skip))))))

(defun gdscript-debug--packet-definition (string-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:type u32r)
    (:string-length u32r)
    (:string-data str ,string-length)
    (align 4)))

(defun gdscript-debug--command (command)
  (let* ((command-alength (align-length command))
         (packet-length (+ (* 4 4) command-alength)))
    (bindat-pack
     (gdscript-debug--packet-definition (length command))
     `((:packet-length . ,packet-length)
       (:array-type . ,variant-array)
       (:elements-count . 1)
       (:type . ,variant-string)
       (:string-length . ,(length command))
       (:string-data . ,command)))))

(defun align-length (string)
  (let ((len (length string)))
    (while (/= (% len 4) 0)
      (setq len (1+ len)))
    len))

(defun gdscript-debug--add-fringe(pos enabled &rest sprops)
  (interactive)
  (let* ((string (make-string 1 ?x))
         (buffer (current-buffer))
         (prop `(left-fringe breakpoint ,(if enabled 'breakpoint-enabled 'breakpoint-disabled)))
         (overlay (make-overlay pos pos buffer)))
    (put-text-property 0 1 'display prop string)
    (if sprops
        (add-text-properties 0 1 sprops string))
    (overlay-put overlay 'put-break t)
    (overlay-put overlay 'before-string string)))

(defun gdscript-debug--remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only strings that were put in BUFFER with calls to `gdscript-debug--add-fringe'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'put-break)
      (delete-overlay overlay))))

;;(make-overlay (line-beginning-position) (line-beginning-position) 'before-string)

(defun gdscript-debug--absolute-path ()
  "Return the absolute path of current gdscript file."
  (when (and buffer-file-name
             (file-exists-p buffer-file-name)
             (string= (file-name-extension buffer-file-name) "gd"))
    buffer-file-name))

(defun gdscript-debug--current-file ()
  (let ((current-file (gdscript-util--get-godot-project-file-path-relative buffer-file-name)))
    (when current-file
      (let ((extension (file-name-extension buffer-file-name)))
        (when (string= extension "gd")
          (concat "res://" current-file ".gd"))))))

(defmacro gdscript-debug--with-gdscript-file (file-info body)
  (declare (indent 1) (debug t))
  `(let ((,file-info (cons (gdscript-debug--current-file) (gdscript-debug--absolute-path))))
     (if (not (car ,file-info))
         (message "No GDScript file.")
       ,body)))

;; (print (macroexpand '(gdscript-debug--with-gdscript-file gdscript-file
;;  (let ((line (line-number-at-pos)))
;;    (message "No breakpoint at %s:%s" line gdscript-file)))))

(defun gdscript-debug-remove-breakpoint ()
  (interactive)
  (gdscript-debug--with-gdscript-file file-info
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (line (line-number-at-pos))
           (file (car file-info))
           (file-absolute (cdr file-info))
           (breakpoint (breakpoint-create :file file :file-absolute file-absolute :line line)))
      (if (not (member breakpoint gdscript-debug--breakpoints))
          (message "No breakpoint at %s:%s" file line)
        (gdscript-debug--remove-strings start end)
        (gdscript-debug--remove-breakpoint-from-buffer breakpoint)
        (gdscript-debug--if-server-process
          (gdscript-debug--send-command
            (gdscript-debug--breakpoint-command file line nil)))))))

(defun gdscript-debug-add-breakpoint ()
  (interactive)
  (gdscript-debug--with-gdscript-file file-info
    (let* ((line (line-number-at-pos))
           (file (car file-info))
           (file-absolute (cdr file-info))
           (breakpoint (breakpoint-create :file file :file-absolute file-absolute :line line)))
      (if (member breakpoint gdscript-debug--breakpoints)
          (message "Breakpoint already present at %s:%s" file line)
        (gdscript-debug--add-fringe (line-beginning-position) (not gdscript-debug--skip-breakpoints) 'gdb-bptno 1)
        (gdscript-debug--add-breakpoint-to-buffer breakpoint)
        (refresh-breakpoints-buffer)
        (gdscript-debug--if-server-process
          (gdscript-debug--send-command
            (gdscript-debug--breakpoint-command file line t)))))))

(defun set-left-fringe-breakpoints (enabled)
  (refresh-breakpoints-buffer)
  (dolist (breakpoint gdscript-debug--breakpoints)
    (let ((file (breakpoint->file-absolute breakpoint))
          (line (breakpoint->line breakpoint)))
      (save-selected-window
        (let ((buffer (find-file-noselect file)))
          (save-excursion
            (with-current-buffer buffer
              (goto-char (point-min))
              (forward-line (1- line))
              (let ((start (line-beginning-position))
                    (end (line-end-position)))
                (dolist (overlay (overlays-in start end))
                  (when (overlay-get overlay 'put-break)
                    (let* ((string (overlay-get overlay 'before-string))
                           (display-property (get-text-property 0 'display string))
                           (prop `(left-fringe breakpoint ,(if (not enabled) 'breakpoint-enabled 'breakpoint-disabled))))
                      (put-text-property 0 1 'display prop string))))))))))))

(defun gdscript-debug-get-stack-frame-vars (level)
  (gdscript-debug--send-command
    (gdscript-debug--get-stack-frame-vars level)))

(cl-defstruct (prim-null (:constructor prim-null-create)
                         (:copier nil)))

(cl-defstruct (prim-bool (:constructor prim-bool-create)
                         (:copier nil)
                         (:conc-name boolean->))
  value)

(cl-defstruct (prim-integer (:constructor prim-integer-create)
                            (:copier nil)
                            (:conc-name integer->))
  value)

(cl-defstruct (prim-float (:constructor prim-float-create)
                          (:copier nil)
                          (:conc-name float->))
  value)

(cl-defstruct (prim-string (:constructor prim-string-create)
                           (:copier nil)
                           (:conc-name string->))
  value)

(cl-defstruct (plane (:constructor plane-create)
                     (:copier nil)
                     (:conc-name plane->))
  normal-x normal-y normal-z distance)

(cl-defstruct (aabb (:constructor aabb-create)
                    (:copier nil)
                    (:conc-name aabb->))
  x-coordinate y-coordinate z-coordinate x-size y-size z-size)

(cl-defstruct (basis (:constructor basis-create)
                     (:copier nil)
                     (:conc-name basis->))
  x y z)

(cl-defstruct (color (:constructor color-create)
                         (:copier nil)
                         (:conc-name color->))
  red green blue alpha)

(cl-defstruct (node-path (:constructor node-path-create)
                         (:copier nil)
                         (:conc-name node-path->))
  path subpath absolute)

(cl-defstruct (rid (:constructor rid-create)
                   (:copier nil)))

(cl-defstruct (object-id (:constructor object-id-create)
                         (:copier nil)
                         (:conc-name object-id->))
  value)

(cl-defstruct (dictionary (:constructor dictionary-create)
                          (:copier nil)
                          (:conc-name dictionary->))
  shared elements)

(cl-defstruct (vector2 (:constructor vector2-create)
                       (:copier nil)
                       (:conc-name vector2->))
  x y)

(cl-defstruct (vector3 (:constructor vector3-create)
                       (:copier nil)
                       (:conc-name vector3->))
  x y z)

(cl-defstruct (transform2d (:constructor transform2d-create)
                           (:copier nil)
                           (:conc-name transform2d->))
  x y origin)

(cl-defstruct (prim-array (:constructor prim-array-create)
                          (:copier nil)
                          (:conc-name prim-array->))
  shared elements)

(cl-defstruct (pool-int-array (:constructor pool-int-array-create)
                              (:copier nil)
                              (:conc-name pool-int-array->))
  elements)

(cl-defstruct (pool-string-array (:constructor pool-string-array-create)
                                 (:copier nil)
                                 (:conc-name pool-string-array->))
  elements)

(cl-defstruct (pool-vector2-array (:constructor pool-vector2-array-create)
                                  (:copier nil)
                                  (:conc-name pool-vector2-array->))
  elements)

(cl-defstruct (pool-color-array (:constructor pool-color-array-create)
                                (:copier nil)
                                (:conc-name pool-color-array->))
  elements)

(cl-defstruct (stack-frame-vars (:constructor stack-frame-vars-create)
                                (:copier nil)
                                (:conc-name stack-frame-vars->))
  locals members globals)

(cl-defstruct (stack-dump (:constructor stack-dump-create)
                          (:copier nil)
                          (:conc-name stack-dump->))
  file line function-name level)

(cl-defstruct (inspect-object (:constructor inspect-object-create)
                              (:copier nil)
                              (:conc-name inspect-object->))
  object-id class properties)

(cl-defstruct (property-info (:constructor property-info-create)
                             (:copier nil)
                             (:conc-name property-info->))
  name type hint hint-string usage variant)

(cl-defstruct (breakpoint (:constructor breakpoint-create)
                          (:copier nil)
                          (:conc-name breakpoint->))
  file file-absolute line)

(cl-defstruct (debug-enter (:constructor debug-enter-create)
                           (:copier nil)
                           (:conc-name debug-enter->))
  can-continue reason)

(defun gdscript-debug--parent-mode ()
  "Generic mode to derive all other buffer modes from."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun gdscript-debug-enable-breakpoints ()
  (interactive)
  (set-left-fringe-breakpoints nil)
  (gdscript-debug--send-command
    (gdscript-debug--set-skip-breakpoints-command nil)))

(defun gdscript-debug-skip-breakpoints ()
  (interactive)
  (set-left-fringe-breakpoints t)
  (gdscript-debug--send-command
    (gdscript-debug--set-skip-breakpoints-command t)))

(defun gdscript-debug-toggle-breakpoint ()
  (interactive)
  (setq gdscript-debug--skip-breakpoints (not gdscript-debug--skip-breakpoints))
  (set-left-fringe-breakpoints gdscript-debug--skip-breakpoints)
  (gdscript-debug--send-command
    (gdscript-debug--set-skip-breakpoints-command gdscript-debug--skip-breakpoints)))

(defun gdscript-debug-delete-breakpoint ()
  (interactive)
  (let ((breakpoint (get-text-property (point) 'gdscript-debug--breakpoint)))
    (if breakpoint
        (let ((file (breakpoint->file-absolute breakpoint))
              (line (breakpoint->line breakpoint)))
          (save-selected-window
            (let ((buffer (find-file-noselect file)))
              (with-current-buffer buffer
                (goto-char (point-min))
                (forward-line (1- line))
                (gdscript-debug-remove-breakpoint)))))
      (error "Not recognized as breakpoint line"))))

(defun gdscript-debug-goto-breakpoint ()
  (interactive)
  (let ((breakpoint (get-text-property (point) 'gdscript-debug--breakpoint)))
    (if breakpoint
        (let ((file (breakpoint->file-absolute breakpoint))
              (line (breakpoint->line breakpoint)))
          (save-selected-window
            (let* ((buffer (find-file-noselect file))
                   (window (display-buffer buffer)))
              (with-current-buffer buffer
                (goto-char (point-min))
                (forward-line (1- line))
                (set-window-point window (point))))))
      (error "Not recognized as breakpoint line"))))

(defun gdscript-debug-inspect-object-id ()
  (interactive)
  (save-excursion
    (let ((object-id (get-text-property (point) 'object-id)))
      (if object-id
          (progn
            (gdscript-debug-inspect-object object-id)
            (save-selected-window
              (let* ((buffer (gdscript-debug--get-buffer-create 'inspector-buffer))
                     (window (display-buffer buffer)))
                (with-current-buffer buffer
                  (set-window-point window (point))))))
        (error "Not recognized as object-id line")))))

(defun gdscript-debug-show-stack-frame-vars ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((stack (get-text-property (point) 'gdscript-debug--stack-dump)))
      (if stack
          (gdscript-debug-get-stack-frame-vars (stack-dump->level stack))
        (error "Not recognized as stack-frame line")))))

(defun gdscript-debug-jump-to-stack-point ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((stack-dump (get-text-property (point) 'gdscript-debug--stack-dump))
          (project-root (get-text-property (point) 'gdscript-debug--project-root)))
      (if stack-dump
          (let* ((file (stack-dump->file stack-dump))
                 (line (stack-dump->line stack-dump))
                 (full-file-path (concat project-root (gdscript-debug--drop-res file))))
            (if (not project-root)
                (error "Project for file %s not found." file)
              (with-current-buffer (find-file full-file-path)
                (let* ((posns (line-posns line))
                       (start-posn (car posns)))
                  (goto-char start-posn)))))
        (error "Not recognized as stack-frame line")))))

(defvar gdscript-debug--stack-dump-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    (define-key map " " 'gdscript-debug-jump-to-stack-point)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "\r" 'gdscript-debug-show-stack-frame-vars)
    (define-key map "\t" 'gdscript-debug-display-stack-frame-vars-buffer)
    (define-key map "?" 'describe-mode)
    map))

(defvar gdscript-debug--breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map " " 'gdscript-debug-toggle-breakpoint)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "D" 'gdscript-debug-delete-breakpoint)
    (define-key map "\r" 'gdscript-debug-goto-breakpoint)
    (define-key map "\t" 'gdscript-debug-display-stack-dump-buffer)
    (define-key map "?" 'describe-mode)
    map))

(defvar gdscript-debug--stack-frame-vars-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "\r" 'gdscript-debug-inspect-object-id)
    (define-key map "\t" 'gdscript-debug-display-inspector-buffer)
    (define-key map "?" 'describe-mode)
    map))

(defvar gdscript-debug--inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "\t" 'gdscript-debug-display-breakpoint-buffer)
    (define-key map "?" 'describe-mode)
    map))

(defvar-local gdscript-debug--buffer-type nil
  "One of the symbols bound in `gdscript-debug--get-buffer-create'.")

(defun gdscript-debug--get-buffer (buffer-type)
  "Get a specific buffer.

In that buffer, `gdscript-debug--buffer-type' must be equal to BUFFER-TYPE."
  (catch 'found
    (dolist (buffer (buffer-list) nil)
      (with-current-buffer buffer
        (when (eq gdscript-debug--buffer-type buffer-type)
          (throw 'found buffer))))))

(defun gdscript-debug--get-stack-frame-vars-buffer ()
  (gdscript-debug--get-buffer-create 'stack-frame-vars-buffer))

(defun gdscript-debug-display-stack-dump-buffer ()
  "Display stack dump."
  (interactive)
  (display-buffer (gdscript-debug--get-buffer-create 'stack-dump-buffer)))

(defun gdscript-debug-display-stack-frame-vars-buffer ()
  "Display the variables of current stack."
  (interactive)
  (display-buffer (gdscript-debug--get-buffer-create 'stack-frame-vars-buffer)))

(defun gdscript-debug-display-breakpoint-buffer ()
  "Display the breakpoints."
  (interactive)
  (display-buffer (gdscript-debug--get-buffer-create 'breakpoints-buffer))
  (refresh-breakpoints-buffer))

(defun gdscript-debug-display-inspector-buffer ()
  "Display the inspector."
  (interactive)
  (display-buffer (gdscript-debug--get-buffer-create 'inspector-buffer)))

(defun gdscript-debug--remove-breakpoint-from-buffer (breakpoint)
  (setq gdscript-debug--breakpoints (remove breakpoint gdscript-debug--breakpoints))
  (refresh-breakpoints-buffer))

(defun gdscript-debug--add-breakpoint-to-buffer (breakpoint)
  (unless (member breakpoint gdscript-debug--breakpoints)
    (push breakpoint gdscript-debug--breakpoints)))

(defun gdscript-debug--refresh-stack-frame-vars-buffer (stack-dump project-root)
  (with-current-buffer (gdscript-debug--get-buffer-create 'stack-dump-buffer)
    (let ((inhibit-read-only t)
          (longest-file-name 0))
      (dolist (stack stack-dump)
        (let* ((file (stack-dump->file stack))
               (line (stack-dump->line stack))
               (len (+ (length file) (length (number-to-string line)))))
          (when (< longest-file-name len)
            (setq longest-file-name len))))
      (erase-buffer)
      (dolist (stack stack-dump)
        (let ((ident (format "%s:%s" (stack-dump->file stack) (stack-dump->line stack))))
          (insert (propertize
                   (concat
                    (format (concat "%s - %-" (number-to-string (1+ longest-file-name)) "s - ") (stack-dump->level stack) ident)
                    (propertize
                     (format "%s\n" (stack-dump->function-name stack)) 'font-lock-face font-lock-function-name-face))
                   'gdscript-debug--stack-dump stack
                   'gdscript-debug--project-root project-root)))))))

(defun gdscript-debug--refresh-inspector-buffer (inspect-object)
  (with-current-buffer (gdscript-debug--get-buffer-create 'inspector-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%s %s\n" (inspect-object->object-id inspect-object) (inspect-object->class inspect-object)))
      (dolist (property (inspect-object->properties inspect-object))
        (insert (format "%s\n" property))))))

(defun refresh-breakpoints-buffer ()
  (with-current-buffer (gdscript-debug--get-buffer-create 'breakpoints-buffer)
    (let* ((inhibit-read-only t)
           (window (get-buffer-window (current-buffer) 0))
           (start (window-start window))
           (p (window-point window)))
      (erase-buffer)
      (insert "Enb Location\n")
      (dolist (breakpoint gdscript-debug--breakpoints)
        (let ((indicator (if (not gdscript-debug--skip-breakpoints)
                             (propertize (format "%-4s" "y") 'font-lock-face
                                         font-lock-warning-face)
                           (propertize (format "%-4s" "n") 'font-lock-face
                                       font-lock-comment-face))))
          (insert (propertize (format "%s%s:%s\n"
                                      indicator
                                      (breakpoint->file breakpoint)
                                      (breakpoint->line breakpoint))
                              'gdscript-debug--breakpoint breakpoint))))
      (set-window-start window start) ;; Forces fringe icons to refresh
      (set-window-point window p))))

(defun gdscript-debug--get-buffer-create (buffer-type)
  (or (gdscript-debug--get-buffer buffer-type)
      (let ((rules (assoc buffer-type gdscript-debug--buffer-rules))
            (new (generate-new-buffer "limbo")))
        (with-current-buffer new
          (let ((mode (gdscript-debug--rules-buffer-mode rules)))
            (when mode (funcall mode))
            (setq gdscript-debug--buffer-type buffer-type)
            (rename-buffer (funcall (gdscript-debug--rules-name-maker rules)))
            (current-buffer))))))

(define-derived-mode gdscript-debug--stack-dump-mode gdscript-debug--parent-mode "Stack Dump"
  "Major mode for stack dump."
  (setq header-line-format "Stack dump"))

(define-derived-mode gdscript-debug--stack-frame-vars-mode gdscript-debug--parent-mode "Stack Frame Vars"
  "Major mode for stack frame variables."
  (setq header-line-format "Stack frame vars"))

(define-derived-mode gdscript-debug--breakpoints-mode gdscript-debug--parent-mode "Breakpoints"
  "Major mode for breakpoints management."
  (setq header-line-format "Breakpoints"))

(define-derived-mode gdscript-debug--inspector-mode gdscript-debug--parent-mode "Inspector"
  "Major mode for inspector management."
  (setq header-line-format "Inspector"))

(defun gdscript-debug--stack-dump-buffer-name ()
  (concat "* Stack dump *"))

(defun gdscript-debug--stack-frame-vars-buffer-name ()
  (concat "* Stack frame vars *"))

(defun gdscript-debug--breakpoints-buffer-name ()
  (concat "* Breakpoints *"))

(defun gdscript-debug--inspector-buffer-name ()
  (concat "* Inspector *"))

(defvar gdscript-debug--buffer-rules '())
(defvar gdscript-debug--breakpoints '())
(defvar gdscript-debug--skip-breakpoints nil)

(defun gdscript-debug--rules-name-maker (rules-entry)
  (cadr rules-entry))
(defun gdscript-debug--rules-buffer-mode (rules-entry)
  (nth 2 rules-entry))

(defun gdscript-debug--set-buffer-rules (buffer-type &rest rules)
  (let ((binding (assoc buffer-type gdscript-debug--buffer-rules)))
    (if binding
	(setcdr binding rules)
      (push (cons buffer-type rules)
	    gdscript-debug--buffer-rules))))

(gdscript-debug--set-buffer-rules
 'stack-frame-vars-buffer
 'gdscript-debug--stack-frame-vars-buffer-name
 'gdscript-debug--stack-frame-vars-mode)

(gdscript-debug--set-buffer-rules
 'stack-dump-buffer
 'gdscript-debug--stack-dump-buffer-name
 'gdscript-debug--stack-dump-mode)

(gdscript-debug--set-buffer-rules
 'breakpoints-buffer
 'gdscript-debug--breakpoints-buffer-name
 'gdscript-debug--breakpoints-mode)

(gdscript-debug--set-buffer-rules
 'inspector-buffer
 'gdscript-debug--inspector-buffer-name
 'gdscript-debug--inspector-mode)

(provide 'gdscript-debug)
