;;; gdscript-debug.el --- Debugger for Godot -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest

;; Author: Josef Vlach <vlach.josef@gmail.com>
;; URL: https://github.com/godotengine/emacs-gdscript-mode/
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: vlach.josef@gmail.com
;; Created: Oct 2020
;; Keywords: languages

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Inspired by gdb-mi.
;;
;;; Code:

(require 'bindat)
(require 'generator)
(require 'gdscript-customization)
;;(require 'gdscript-hydra) -- this causes cyclic dependency
(require 'gdscript-utils)

(eval-when-compile
  (require 'subr-x))

(defcustom gdscript-debug-emacs-executable "Emacs"
  "The name of Emacs application. Used for focusing Emacs
when breakpoint is encountered in Godot."
  :type 'string
  :group 'gdscript)

;; Overlay arrow markers
(defvar gdscript-debug--thread-position nil)

(defvar gdscript-debug--boolean-bindat-spec
  '((:boolean-data u32r)))

(defvar gdscript-debug--integer-bindat-spec
  `((:data u32r)
    (:integer-data eval (- (logand last ,(lognot (lsh 1 31))) (logand last ,(lsh 1 31))))))

(defvar gdscript-debug--integer-64-bindat-spec
  `((:data-a u32r)
    (:data-b u32r)
    (:data eval (let ((a (bindat-get-field struct :data-a))
                      (b (bindat-get-field struct :data-b)))
                  (logior (lsh b 32) a)))
    (:integer-data eval (let ((a (bindat-get-field struct :data)))
                          (- (logand a ,(lognot (lsh 1 63))) (logand a ,(lsh 1 63)))))))

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

(defvar gdscript-debug--float-bindat-spec
  '((:vect vec 4 byte)
    (:float-value eval (let ((alist (reverse (append (bindat-get-field struct :vect) nil))))
                         (apply 'gdscript-debug--load-f32 alist)))))

(defvar gdscript-debug--float-64-bindat-spec
  '((:vect vec 8 byte)
    (:float-value eval (let ((alist (reverse (append (bindat-get-field struct :vect) nil))))
                         (apply 'gdscript-debug--load-f64 alist)))))

(defvar gdscript-debug--string-bindat-spec
  '((:data-length u32r)
    (:string-data str (:data-length))
    (align 4)))

(defvar gdscript-debug--string-z-bindat-spec
  '((:data-length u32r)
    (:string-data strz (:data-length))
    (align 4)))

(defvar gdscript-debug--vector2-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :x)
    ,@(gdscript-debug--capture-float-spec :y)))

(defvar gdscript-debug--rect2-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :x-coordinate)
    ,@(gdscript-debug--capture-float-spec :y-coordinate)
    ,@(gdscript-debug--capture-float-spec :x-size)
    ,@(gdscript-debug--capture-float-spec :y-size)))

(defvar gdscript-debug--vector3-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :x)
    ,@(gdscript-debug--capture-float-spec :y)
    ,@(gdscript-debug--capture-float-spec :z)))

(defvar gdscript-debug--transform2d-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :xx)
    ,@(gdscript-debug--capture-float-spec :xy)
    ,@(gdscript-debug--capture-float-spec :yx)
    ,@(gdscript-debug--capture-float-spec :yy)
    ,@(gdscript-debug--capture-float-spec :x-origin)
    ,@(gdscript-debug--capture-float-spec :y-origin)))

(defvar gdscript-debug--dictionary-bindat-spec
  '((:data u32r)
    (:shared   eval (logand (bindat-get-field struct :data) #x80000000))
    (:elements eval (logand (bindat-get-field struct :data) #x7fffffff))
    (:dictionary-length eval (* 2 last))
    (:items repeat (:dictionary-length) (struct gdscript-godot-data-bindat-spec))))

(defvar gdscript-debug--array-bindat-spec
  '((:data u32r)
    (:shared       eval (logand (bindat-get-field struct :data) #x80000000))
    (:array-length eval (logand (bindat-get-field struct :data) #x7fffffff))
    (:items repeat (:array-length) (struct gdscript-godot-data-bindat-spec))))

(defvar gdscript-debug--pool-byte-array-bindat-spec
  '((:array-length u32r)
    (:items vec (:array-length) byte)
    (align 4)))

(defvar gdscript-debug--pool-int-array-bindat-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--integer-bindat-spec))))

(defvar gdscript-debug--pool-real-array-bindat-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--float-bindat-spec))))

(defvar gdscript-debug--pool-string-array-bindat-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--string-z-bindat-spec))))

(defvar gdscript-debug--pool-vector2-array-bindat-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--vector2-bindat-spec))))

(defvar gdscript-debug--pool-vector3-array-bindat-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--vector3-bindat-spec))))

(defvar gdscript-debug--pool-color-array-bindat-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debug--color-bindat-spec))))

(defvar gdscript-debug--plane-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :normal-x)
    ,@(gdscript-debug--capture-float-spec :normal-y)
    ,@(gdscript-debug--capture-float-spec :normal-z)
    ,@(gdscript-debug--capture-float-spec :distance)))

(defvar gdscript-debug--quat-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :x-imaginary)
    ,@(gdscript-debug--capture-float-spec :y-imaginary)
    ,@(gdscript-debug--capture-float-spec :z-imaginary)
    ,@(gdscript-debug--capture-float-spec :real-w)))

(defvar gdscript-debug--aabb-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :x-coordinate)
    ,@(gdscript-debug--capture-float-spec :y-coordinate)
    ,@(gdscript-debug--capture-float-spec :z-coordinate)
    ,@(gdscript-debug--capture-float-spec :x-size)
    ,@(gdscript-debug--capture-float-spec :y-size)
    ,@(gdscript-debug--capture-float-spec :z-size)))

(defvar gdscript-debug--basis-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :xx)
    ,@(gdscript-debug--capture-float-spec :xy)
    ,@(gdscript-debug--capture-float-spec :xz)
    ,@(gdscript-debug--capture-float-spec :yx)
    ,@(gdscript-debug--capture-float-spec :yy)
    ,@(gdscript-debug--capture-float-spec :yz)
    ,@(gdscript-debug--capture-float-spec :zx)
    ,@(gdscript-debug--capture-float-spec :zy)
    ,@(gdscript-debug--capture-float-spec :zz)))

(defvar gdscript-debug--transform-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :xx)
    ,@(gdscript-debug--capture-float-spec :xy)
    ,@(gdscript-debug--capture-float-spec :xz)
    ,@(gdscript-debug--capture-float-spec :yx)
    ,@(gdscript-debug--capture-float-spec :yy)
    ,@(gdscript-debug--capture-float-spec :yz)
    ,@(gdscript-debug--capture-float-spec :zx)
    ,@(gdscript-debug--capture-float-spec :zy)
    ,@(gdscript-debug--capture-float-spec :zz)
    ,@(gdscript-debug--capture-float-spec :x-origin)
    ,@(gdscript-debug--capture-float-spec :y-origin)
    ,@(gdscript-debug--capture-float-spec :z-origin)))

(defvar gdscript-debug--color-bindat-spec
  `(,@(gdscript-debug--capture-float-spec :red)
    ,@(gdscript-debug--capture-float-spec :green)
    ,@(gdscript-debug--capture-float-spec :blue)
    ,@(gdscript-debug--capture-float-spec :alpha)))

(defvar gdscript-debug--node-path-bindat-spec
  '((:data-length u32r)
    (:new-format eval (logand (bindat-get-field struct :data-length) #x80000000))
    (:name-count eval (logand (bindat-get-field struct :data-length) #x7FFFFFFF))
    (:subname-count u32r)
    (:flags u32r)
    (:absolute eval (not (eq 0 (logand (bindat-get-field struct :flags) #x1))))
    (:names repeat (:name-count) (struct gdscript-debug--string-bindat-spec))
    (:subnames repeat (:subname-count) (struct gdscript-debug--string-bindat-spec))))

(defvar gdscript-debug--rid-bindat-spec nil) ;; unsupported

(defvar gdscript-debug--object-as-id-bindat-spec
  '((:object-as-id-a u32r)
    (:object-as-id-b u32r)
    (:long eval (let ((a (bindat-get-field struct :object-as-id-a))
                      (b (bindat-get-field struct :object-as-id-b)))
                  (logior (lsh b 32) a)))))

(defconst gdscript-debug--encode-mask #xff)
(defconst gdscript-debug--encode-flag-64 (lsh 1 16))

(defvar gdscript-godot-data-bindat-spec
  '((:type-data    u32r)
    (:type         eval (logand last gdscript-debug--encode-mask))
    (:flag-64      eval (logand (bindat-get-field struct :type-data) gdscript-debug--encode-flag-64))
    (:object-as-id eval (logand (bindat-get-field struct :type-data) gdscript-debug--encode-flag-64))
    (union (:type)
           (0 nil)
           (1 (struct gdscript-debug--boolean-bindat-spec))
           ((eval (and (eq 2 tag) (equal 0 (bindat-get-field struct :flag-64)))) (struct gdscript-debug--integer-bindat-spec))
           (2 (struct gdscript-debug--integer-64-bindat-spec))
           ((eval (and (eq 3 tag) (equal 0 (bindat-get-field struct :flag-64)))) (struct gdscript-debug--float-bindat-spec))
           (3 (struct gdscript-debug--float-64-bindat-spec))
           (4 (struct gdscript-debug--string-bindat-spec))
           (5 (struct gdscript-debug--vector2-bindat-spec))
           (6 (struct gdscript-debug--rect2-bindat-spec))
           (7 (struct gdscript-debug--vector3-bindat-spec))
           (8 (struct gdscript-debug--transform2d-bindat-spec))
           (9 (struct gdscript-debug--plane-bindat-spec))
           (10 (struct gdscript-debug--quat-bindat-spec))
           (11 (struct gdscript-debug--aabb-bindat-spec))
           (12 (struct gdscript-debug--basis-bindat-spec))
           (13 (struct gdscript-debug--transform-bindat-spec))
           (14 (struct gdscript-debug--color-bindat-spec))
           (15 (struct gdscript-debug--node-path-bindat-spec))
           (16 (struct gdscript-debug--rid-bindat-spec))
           ((eval (and (eq 17 tag) (equal 0 (bindat-get-field struct :object-as-id)))) (error "[ObjectId] Not implemented yet"))
           (17 (struct gdscript-debug--object-as-id-bindat-spec))
           (18 (struct gdscript-debug--dictionary-bindat-spec))
           (19 (struct gdscript-debug--array-bindat-spec))
           (20 (struct gdscript-debug--pool-byte-array-bindat-spec))
           (21 (struct gdscript-debug--pool-int-array-bindat-spec))
           (22 (struct gdscript-debug--pool-real-array-bindat-spec))
           (23 (struct gdscript-debug--pool-string-array-bindat-spec))
           (24 (struct gdscript-debug--pool-vector2-array-bindat-spec))
           (25 (struct gdscript-debug--pool-vector3-array-bindat-spec))
           (26 (struct gdscript-debug--pool-color-array-bindat-spec))
           (t (eval (error "Unknown type: %s" tag))))))

(defvar gdscript-debug--previous-packet-data nil)
(defvar gdscript-debug--data-needed nil)
(defvar gdscript-debug--offset 0)
(defvar gdscript-debug--packet-length-bindat-spec '((:packet-length u32r)))

(defun gdscript-debug--current-packet (content offset)
  (bindat-unpack gdscript-debug--packet-length-bindat-spec content offset))

(defun gdscript-debug--process-packet (content offset)
  (bindat-unpack gdscript-godot-data-bindat-spec content offset))

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

(defsubst gdscript-debug--get-boolean (struct-data)
  (bindat-get-field struct-data :boolean-data))

(defsubst gdscript-debug--get-integer (struct-data)
  (bindat-get-field struct-data :integer-data))

(defsubst gdscript-debug--get-float (struct-data)
  (bindat-get-field struct-data :float-value))

(defsubst gdscript-debug--get-string (struct-data)
  (bindat-get-field struct-data :string-data))

(defsubst gdscript-debug--get-array (struct-data)
  (bindat-get-field struct-data :items))

(defsubst gdscript-debug--to-plane (struct)
  (let ((normal-x (bindat-get-field struct :normal-x))
        (normal-y (bindat-get-field struct :normal-y))
        (normal-z (bindat-get-field struct :normal-z))
        (distance (bindat-get-field struct :distance)))
    (gdscript-plane-create
     :normal (gdscript-vector3-create
              :x normal-x
              :y normal-y
              :z normal-z)
     :distance distance)))

(defsubst gdscript-debug--to-quat (struct)
  (let ((x-imaginary (bindat-get-field struct :x-imaginary))
        (y-imaginary (bindat-get-field struct :y-imaginary))
        (z-imaginary (bindat-get-field struct :z-imaginary))
        (real-w (bindat-get-field struct :real-w)))
    (gdscript-quat-create
     :x-imaginary x-imaginary
     :y-imaginary y-imaginary
     :z-imaginary z-imaginary
     :real-w real-w)))

(defsubst gdscript-debug--to-aabb (struct)
  (let ((x-coordinate (bindat-get-field struct :x-coordinate))
        (y-coordinate (bindat-get-field struct :y-coordinate))
        (z-coordinate (bindat-get-field struct :z-coordinate))
        (x-size (bindat-get-field struct :x-size))
        (y-size (bindat-get-field struct :y-size))
        (z-size (bindat-get-field struct :z-size)))
    (gdscript-aabb-create
     :position (gdscript-vector3-create
                :x x-coordinate
                :y y-coordinate
                :z z-coordinate)
     :size (gdscript-vector3-create
            :x x-size
            :y y-size
            :z z-size))))

(defsubst gdscript-debug--to-basis (struct)
  (let ((xx (bindat-get-field struct :xx))
        (xy (bindat-get-field struct :xy))
        (xz (bindat-get-field struct :xz))
        (yx (bindat-get-field struct :yx))
        (yy (bindat-get-field struct :yy))
        (yz (bindat-get-field struct :yz))
        (zx (bindat-get-field struct :zx))
        (zy (bindat-get-field struct :zy))
        (zz (bindat-get-field struct :zz)))
    (gdscript-basis-create
     :x (gdscript-vector3-create :x xx :y xy :z xz)
     :y (gdscript-vector3-create :x yx :y yy :z yz)
     :z (gdscript-vector3-create :x zx :y zy :z zz))))

(defsubst gdscript-debug--to-transform (struct)
  (let ((basis (gdscript-debug--to-basis struct))
        (x-origin (bindat-get-field struct :x-origin))
        (y-origin (bindat-get-field struct :y-origin))
        (z-origin (bindat-get-field struct :z-origin)))
    (gdscript-transform-create
     :basis basis
     :origin (gdscript-vector3-create
              :x x-origin
              :y y-origin
              :z z-origin))))

(defsubst gdscript-debug--to-color (struct)
  (let ((red (bindat-get-field struct :red))
        (green (bindat-get-field struct :green))
        (blue (bindat-get-field struct :blue))
        (alpha (bindat-get-field struct :alpha)))
    (gdscript-color-create :red red :green green :blue blue :alpha alpha)))

(defsubst gdscript-debug--to-node-path (struct)
  (let ((names (mapcar 'gdscript-debug--to-string (bindat-get-field struct :names)))
        (subnames (mapcar 'gdscript-debug--to-string (bindat-get-field struct :subnames)))
        (absolute (bindat-get-field struct :absolute)))
    (gdscript-node-path-create :names names :subnames subnames :absolute (gdscript-bool-create :value absolute))))

(defsubst gdscript-debug--to-rid (_struct-data)
  (gdscript-rid-create))

(defsubst gdscript-debug--to-vector2 (struct-data)
  (let ((x (bindat-get-field struct-data :x))
        (y (bindat-get-field struct-data :y)))
    (gdscript-vector2-create :x x :y y)))

(defsubst gdscript-debug--to-rect2 (struct)
  (let ((x-coordinate (bindat-get-field struct :x-coordinate))
        (y-coordinate (bindat-get-field struct :y-coordinate))
        (x-size (bindat-get-field struct :x-size))
        (y-size (bindat-get-field struct :y-size)))
    (gdscript-rect2-create
     :coordinate (gdscript-vector2-create :x x-coordinate :y y-coordinate)
     :size (gdscript-vector2-create :x x-size :y y-size))))

(defsubst gdscript-debug--to-vector3 (struct-data)
  (let ((x (bindat-get-field struct-data :x))
        (y (bindat-get-field struct-data :y))
        (z (bindat-get-field struct-data :z)))
    (gdscript-vector3-create :x x :y y :z z)))

(defsubst gdscript-debug--to-transform2d (struct-data)
  (let ((xx (bindat-get-field struct-data :xx))
        (xy (bindat-get-field struct-data :xy))
        (yx (bindat-get-field struct-data :yx))
        (yy (bindat-get-field struct-data :yy))
        (x-origin (bindat-get-field struct-data :x-origin))
        (y-origin (bindat-get-field struct-data :y-origin)))
    (gdscript-transform2d-create
     :x (gdscript-vector2-create :x xx :y xy)
     :y (gdscript-vector2-create :x yx :y yy)
     :origin (gdscript-vector2-create :x x-origin :y y-origin))))

(defsubst gdscript-debug--to-null (_struct-data)
  (gdscript-null-create))

(defsubst gdscript-debug--to-boolean (struct-data)
  (gdscript-bool-create :value (if (eq 1 (gdscript-debug--get-boolean struct-data))
                                   t
                                 nil)))

(defsubst gdscript-debug--shared-to-boolean (shared)
  (gdscript-bool-create :value (if (eq 0 shared) nil t)))

(defsubst gdscript-debug--to-integer (struct-data)
  (gdscript-integer-create :value (gdscript-debug--get-integer struct-data)))

(defsubst gdscript-debug--to-float (struct-data)
  (gdscript-float-create :value (gdscript-debug--get-float struct-data)))

(defsubst gdscript-debug--to-string (struct-data)
  (gdscript-string-create :value (gdscript-debug--get-string struct-data)))

(defsubst gdscript-debug--to-object-id (struct-data)
  (gdscript-object-id-create :value (bindat-get-field struct-data :long)))

(defsubst gdscript-debug--to-dictionary (struct-data)
  (let* ((shared (bindat-get-field struct-data :shared))
         (items (bindat-get-field struct-data :items)))
    (gdscript-dictionary-create :shared (gdscript-debug--shared-to-boolean shared) :elements (gdscript-debug--to-dic items))))

(defun gdscript-debug--to-dic (xs)
  (cl-loop for (key value) on xs by 'cddr
           collect (gdscript-debug--from-key-value key value)))

(defsubst gdscript-debug--to-array (struct-data)
  (let ((shared (bindat-get-field struct-data :shared))
        (items (bindat-get-field struct-data :items)))
    (gdscript-array-create :shared (gdscript-debug--shared-to-boolean shared) :elements (mapcar 'gdscript-debug--from-variant items))))

(defsubst gdscript-debug--to-pool-byte-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (gdscript-pool-byte-array-create :elements items)))

(defsubst gdscript-debug--to-pool-int-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (gdscript-pool-int-array-create :elements (mapcar 'gdscript-debug--to-integer items))))

(defsubst gdscript-debug--to-pool-real-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (gdscript-pool-real-array-create :elements (mapcar 'gdscript-debug--to-float items))))

(defsubst gdscript-debug--to-pool-string-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (gdscript-pool-string-array-create :elements (mapcar 'gdscript-debug--to-string items))))

(defsubst gdscript-debug--to-pool-vector2-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (gdscript-pool-vector2-array-create :elements (mapcar 'gdscript-debug--to-vector2 items))))

(defsubst gdscript-debug--to-pool-vector3-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (gdscript-pool-vector3-array-create :elements (mapcar 'gdscript-debug--to-vector3 items))))

(defsubst gdscript-debug--to-pool-color-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (gdscript-pool-color-array-create :elements (mapcar 'gdscript-debug--to-color items))))

(defun gdscript-debug--from-key-value (key value)
  (let* ((var-name (gdscript-debug--from-variant key))
         (var-val (gdscript-debug--from-variant value)))
    `(,var-name . ,var-val)))

(defun gdscript-debug--from-variant (struct)
  (let ((type (bindat-get-field struct :type))
        (object-as-id (bindat-get-field struct :object-as-id)))
    (pcase type
      (0 (gdscript-debug--to-null struct))
      (1 (gdscript-debug--to-boolean struct))
      (2 (gdscript-debug--to-integer struct))
      (3 (gdscript-debug--to-float struct))
      (4 (gdscript-debug--to-string struct))
      (5 (gdscript-debug--to-vector2 struct))
      (6 (gdscript-debug--to-rect2 struct))
      (7 (gdscript-debug--to-vector3 struct))
      (8 (gdscript-debug--to-transform2d struct))
      (9 (gdscript-debug--to-plane struct))
      (10 (gdscript-debug--to-quat struct))
      (11 (gdscript-debug--to-aabb struct))
      (12 (gdscript-debug--to-basis struct))
      (13 (gdscript-debug--to-transform struct))
      (14 (gdscript-debug--to-color struct))
      (15 (gdscript-debug--to-node-path struct))
      (16 (gdscript-debug--to-rid struct))
      (17 (if (eq 0 object-as-id) (error "TODO object as not ID")
            (gdscript-debug--to-object-id struct)))
      (18 (gdscript-debug--to-dictionary struct))
      (19 (gdscript-debug--to-array struct))
      (20 (gdscript-debug--to-pool-byte-array struct))
      (21 (gdscript-debug--to-pool-int-array struct))
      (22 (gdscript-debug--to-pool-real-array struct))
      (23 (gdscript-debug--to-pool-string-array struct))
      (24 (gdscript-debug--to-pool-vector2-array struct))
      (25 (gdscript-debug--to-pool-vector3-array struct))
      (26 (gdscript-debug--to-pool-color-array struct))
      (_ (error "[gdscript-debug--from-variant] Unknown type %s" type)))))

(defun gdscript-debug--to-stack-dump (stack-data level)
  (pcase stack-data
    (`(,_file-key, file-value, _line-key, line-value, _function-key, function-value, _id-key, _id-value)
     (gdscript-stack-dump-create
      :file (gdscript-debug--get-string file-value)
      :line (gdscript-debug--get-integer line-value)
      :function-name (gdscript-debug--get-string function-value)
      :level level))))

(defun gdscript-debug-error-pretty (output-error)
  (format "%s:%s %s - %s (%s) - %s"
          (gdscript-string->value (gdscript-output-error->source-file output-error))
          (gdscript-integer->value (gdscript-output-error->source-line output-error))
          (gdscript-string->value (gdscript-output-error->source-func output-error))
          (gdscript-string->value (gdscript-output-error->error output-error))
          (gdscript-string->value (gdscript-output-error->error-descr output-error))
          (gdscript-output-error->callstack output-error)))

(defun gdscript-debug--to-output-error (error-data stack-infos)
  (pcase error-data
    (`(,hr, min, sec, msec, source-func, source-file, source-line, error-msg, error-descr, warning)
     (gdscript-output-error-create
      :hr (gdscript-debug--to-integer hr)
      :min (gdscript-debug--to-integer min)
      :sec (gdscript-debug--to-integer sec)
      :msec (gdscript-debug--to-integer msec)
      :source-file (gdscript-debug--to-string source-file)
      :source-func (gdscript-debug--to-string source-func)
      :source-line (gdscript-debug--to-integer source-line)
      :error (gdscript-debug--to-string error-msg)
      :error-descr (gdscript-debug--to-string error-descr)
      :warning (gdscript-debug--to-boolean warning)
      :callstack stack-infos))))

(defun gdscript-debug--error-callstack (iter)
  (let ((file (bindat-get-field (iter-next iter) :string-data))
        (func (bindat-get-field (iter-next iter) :string-data))
        (line (bindat-get-field (iter-next iter) :integer-data)))
    (sgdscript-tack-info-create :file file :func func :line line)))

(defun gdscript-debug--mk-error (iter)
  (let ((_callstack-size (bindat-get-field (iter-next iter) :integer-data))
        (error-data (bindat-get-field (iter-next iter) :items))
        (error-callstack-size (bindat-get-field (iter-next iter) :integer-data)))
    (let ((stack-infos))
      (dotimes (_i (/ error-callstack-size 3))
        (push (gdscript-debug--error-callstack iter) stack-infos))
      (gdscript-debug--to-output-error error-data (reverse stack-infos)))))

(defun gdscript-debug--mk-performance (iter)
  (let ((_skip-this (iter-next iter))
        (performance-data (bindat-get-field (iter-next iter) :items)))
    `(command "performace" performance-data ,performance-data)))

(defun gdscript-debug--read-var-names (iter count)
  (let ((variables))
    (dotimes (_i count)
      (let* ((var-name (bindat-get-field (iter-next iter) :string-data))
             (var-value (iter-next iter))
             (var-val (gdscript-debug--from-variant var-value)))
        (push `(,var-name . ,var-val) variables)))
    (reverse variables)))

(defun gdscript-debug--mk-stack-frame-vars (iter)
  (let* ((_total-size (gdscript-debug--get-integer (iter-next iter)))
         (locals-size (gdscript-debug--get-integer (iter-next iter)))
         (locals (gdscript-debug--read-var-names iter locals-size))
         (members-size (gdscript-debug--get-integer (iter-next iter)))
         (members (gdscript-debug--read-var-names iter members-size))
         (globals-size (gdscript-debug--get-integer (iter-next iter)))
         (globals (gdscript-debug--read-var-names iter globals-size)))
    (gdscript-stack-frame-vars-create :locals locals :members members :globals globals)))

(defun gdscript-debug--to-property-info (properties)
  (let ((property-info))
    (dolist (property properties)
      (cond ((eq 6 (bindat-get-field property :array-length))
             (let* ((data (bindat-get-field property :items))
                    (name (bindat-get-field (car data) :string-data))
                    (type (bindat-get-field (nth 1 data) :integer-data))
                    (hint (bindat-get-field (nth 2 data) :integer-data))
                    (hint-string (bindat-get-field (nth 3 data) :string-data))
                    (usage (bindat-get-field (nth 4 data) :integer-data))
                    (variant (gdscript-debug--from-variant (nth 5 data)))
                    (new-prop (gdscript-property-info-create
                               :name name
                               :type type
                               :hint hint
                               :hint-string hint-string
                               :usage usage
                               :variant variant)))
               (push new-prop property-info)))
            (t (message "Ignoring property %s" property))))
    (reverse property-info)))

(defun gdscript-debug--get-children (iter)
  (let ((child-count (gdscript-debug--get-integer (iter-next iter)))
        (node-name (gdscript-debug--get-string (iter-next iter)))
        (node-class (gdscript-debug--get-string (iter-next iter)))
        (instance-id (gdscript-debug--get-integer (iter-next iter))))
    (let ((children))
      (dotimes (_i child-count)
        (push (gdscript-debug--get-children iter) children))
      (gdscript-scene-tree-level-edge-create :item (gdscript-scene-tree-node-create
                                                    :node-name node-name
                                                    :node-class node-class
                                                    :instance-id instance-id)
                                             :children children))))

(cl-defstruct (gdscript-scene-tree-level-edge (:constructor gdscript-scene-tree-level-edge-create)
                                              (:copier nil)
                                              (:conc-name gdscript-scene-tree-level-edge->))
  item children)

(cl-defstruct (gdscript-scene-tree-node (:constructor gdscript-scene-tree-node-create)
                                        (:copier nil)
                                        (:conc-name gdscript-scene-tree-node->))
  node-name node-class instance-id)

(defun gdscript-debug--mk-scene-tree (iter)
  (let ((_array-size (gdscript-debug--get-integer (iter-next iter))))
    (gdscript-debug--get-children iter)))

(defun gdscript-debug--mk-inspect-object (iter)
  (let ((_three (gdscript-debug--get-integer (iter-next iter)))
        (object-id (gdscript-debug--get-integer (iter-next iter)))
        (class (gdscript-debug--get-string (iter-next iter)))
        (properties (gdscript-debug--get-array (iter-next iter))))
    (gdscript-inspect-object-create :object-id object-id :class class :properties (gdscript-debug--to-property-info properties))))

(defun gdscript-debug--mk-stack-dump (iter)
  (let ((stack-level-count (gdscript-debug--get-integer (iter-next iter)))
        (outputs))
    (dotimes (level stack-level-count)
      (let ((stack-data (bindat-get-field (iter-next iter) :items)))
        (push (gdscript-debug--to-stack-dump stack-data level) outputs)))
    (reverse outputs)))

(defun gdscript-debug--mk-output (iter)
  (let ((output-count (bindat-get-field (iter-next iter) :integer-data))
        (outputs))
    (dotimes (_i output-count)
      (let* ((data (iter-next iter))
             (output (bindat-get-field data :items 0 :string-data)))
        (setq outputs (cons output outputs))))
    `(command "output" outputs, outputs)))

(defun gdscript-debug--mk-debug-enter (iter)
  (let ((_skip-this (iter-next iter))
        (can-continue (bindat-get-field (iter-next iter) :boolean-data))
        (reason (bindat-get-field (iter-next iter) :string-data)))
    (gdscript-debug-enter-create :can-continue can-continue :reason reason)))

(defun gdscript-debug--mk-debug-exit (iter)
  (let ((_skip-this (iter-next iter)))
    '(command "debug_exit")))

(defun gdscript-debug--line-posns (line)
  "Return a pair of LINE beginning and end positions."
  (let ((offset (1+ (- line (line-number-at-pos)))))
    (cons
     (line-beginning-position offset)
     (line-end-position offset))))

(defsubst gdscript-debug--drop-res (file-path)
  (substring file-path (length "res://")))

(defun gdscript-debug--on-stack-dump (stack-dump project-root)
  (let* ((file (gdscript-stack-dump->file stack-dump))
         (line (gdscript-stack-dump->line stack-dump))
         (full-file-path (concat project-root (gdscript-debug--drop-res file))))
    (if (not project-root)
        (error "Project for file %s not found" file)
      (with-current-buffer (find-file-noselect full-file-path)
        (let* ((posns (gdscript-debug--line-posns line))
               (start-posn (car posns)))
          (set-marker gdscript-debug--thread-position start-posn)
          (goto-char gdscript-debug--thread-position)
          (current-buffer))))))

(defmacro gdscript-debug--command-handler (&rest body)
  `(progn
     ,@body
     (setq gdscript-debug--previous-packet-data
           (substring gdscript-debug--previous-packet-data gdscript-debug--offset (length gdscript-debug--previous-packet-data)))
     (setq gdscript-debug--offset 0)))

(defun gdscript-debug--switch-to-emacs ()
  (do-applescript (format "tell application \"%s\" to activate" gdscript-debug-emacs-executable)))

(defun gdscript-debug--handle-server-reply (process content)
  "Gets invoked whenever the server sends data to the client."
  (setq gdscript-debug--previous-packet-data (concat gdscript-debug--previous-packet-data content))
  (when (or (null gdscript-debug--data-needed)
            (<= gdscript-debug--data-needed (length gdscript-debug--previous-packet-data)))
    (condition-case _x
        (let ((iter (gdscript-debug--command-iter)))
          (while t
            (let* ((next-data (iter-next iter))
                   (str (bindat-get-field next-data :string-data)))
              (pcase str
                ("debug_enter"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'debug_enter' command")
                  (let ((cmd (gdscript-debug--mk-debug-enter iter)))
                    (setq gdscript-debug--debug-enter cmd)
                    (gdscript-debug-get-stack-dump))))
                ("debug_exit"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'debug_exit' command")
                  (let ((_cmd (gdscript-debug--mk-debug-exit iter))))))
                ("output"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'output' command")
                  (let ((_cmd (gdscript-debug--mk-output iter))))))
                ("error"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'error' command")
                  (let ((cmd (gdscript-debug--mk-error iter)))
                    (message "%s" (gdscript-debug-error-pretty cmd)))))
                ("performance"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'performance' command")
                  (let ((_cmd (gdscript-debug--mk-performance iter))))))
                ("stack_dump"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'stack_dump' command")
                  (let ((cmd (gdscript-debug--mk-stack-dump iter))
                        (project-root (process-get process 'project)))
                    (pcase (gdscript-debug-enter->reason gdscript-debug--debug-enter)
                      ("Breakpoint"
                       (gdscript-debug--refresh-stack-dump-buffer cmd project-root)
                       (setq gdscript-debug--stack-dump (cons cmd project-root))
                       (let ((top-stack-dump (car cmd)))
                         (setq gdscript-debug--selected-stack-dump top-stack-dump)
                         (gdscript-debug--on-stack-dump top-stack-dump project-root)
                         (gdscript-debug-get-stack-frame-vars (gdscript-stack-dump->level top-stack-dump)))
                       (gdscript-debug-hydra))
                      (other
                       (select-window (display-buffer (gdscript-debug--on-stack-dump (car cmd) project-root)))
                       (message "%s" other)))
                    (run-at-time "0.25 sec" nil #'gdscript-debug--switch-to-emacs))))
                ("stack_frame_vars"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'stack_frame_vars' command")
                  (let ((cmd (gdscript-debug--mk-stack-frame-vars iter)))
                    (setq gdscript-debug--stack-frame-vars cmd
                          gdscript-debug--inspected-objects (make-hash-table)
                          gdscript-debug--object-ids-to-fetch nil
                          gdscript-debug-state nil)
                    (gdscript-debug--construct-stack-var-buffer cmd)
                    (let* ((self (cdar (gdscript-stack-frame-vars->members cmd)))
                           (object-id (gdscript-object-id->value self)))
                      (setq gdscript-debug--current-self object-id
                            gdscript-debug--inspector-focused-object-id object-id)
                      (if (and gdscript-debug--pinned-object-id
                               (not (eq gdscript-debug--current-self gdscript-debug--pinned-object-id)))
                          (progn
                            (gdscript-debug-inspect-object gdscript-debug--pinned-object-id)
                            (gdscript-debug-inspect-object gdscript-debug--current-self)
                            (setq gdscript-debug-state :expect-two))
                        (gdscript-debug-inspect-object gdscript-debug--current-self)
                        (setq gdscript-debug--inspector-focused-object-id gdscript-debug--current-self
                              gdscript-debug-state :expect-one)))))
                 (gdscript-debug-windows))
                ("message:inspect_object"
                 (gdscript-debug--command-handler
                  ;;(message "Received 'message:inspect_object' command")
                  (let* ((cmd (gdscript-debug--mk-inspect-object iter))
                         (object-id (gdscript-inspect-object->object-id cmd)))
                    (puthash object-id cmd gdscript-debug--inspected-objects)
                    (cond ((eq gdscript-debug-state :fetching)
                           (setq gdscript-debug--object-ids-to-fetch (delete (gdscript-object-id-create :value object-id) gdscript-debug--object-ids-to-fetch))
                           (when (null gdscript-debug--object-ids-to-fetch)
                             (setq gdscript-debug-state nil)
                             (gdscript-debug--construct-stack-var-buffer gdscript-debug--stack-frame-vars)
                             (gdscript-debug--refresh-inspector-buffer gdscript-debug--inspector-focused-object-id)))
                          ((eq gdscript-debug-state :refresh-inspector)
                           (setq gdscript-debug-state nil)
                           (gdscript-debug--refresh-inspector-buffer object-id))
                          ((and
                            (eq gdscript-debug-state :expect-two)
                            (eq object-id gdscript-debug--pinned-object-id))
                           (setq gdscript-debug-state :expect-one))
                          ((and
                            (eq gdscript-debug-state :expect-two)
                            (eq object-id gdscript-debug--current-self))
                           ;; Expected `gdscript-debug--pinned-object-id' but got `gdscript-debug--current-self'
                           ;; instead. That means `gdscript-debug--pinned-object-id' doesn't exists anymore.
                           (setq gdscript-debug--pinned-object-id nil
                                 gdscript-debug--inspector-stack nil
                                 gdscript-debug-state nil)
                           (gdscript-debug--refresh-inspector-buffer object-id))
                          ((eq gdscript-debug-state :expect-one)
                           (setq gdscript-debug-state nil
                                 gdscript-debug--inspector-stack nil)
                           (gdscript-debug--refresh-inspector-buffer (if gdscript-debug--pinned-object-id
                                                                         gdscript-debug--pinned-object-id
                                                                       object-id)))))))
                ("message:scene_tree"
                 (gdscript-debug--command-handler
                  (let* ((cmd (gdscript-debug--mk-scene-tree iter)))
                    (gdscript-debug--refresh-scene-tree-buffer cmd)
                    (gdscript-debug-display-scene-tree-buffer))))
                (_ (error "Unknown command %s" str))))))
      ;;(iter-end-of-sequence (message "No more packets to process %s" x))
      (iter-end-of-sequence nil))))

(defun gdscript-debug-pin-inspected-object-id ()
  (interactive)
  (setq gdscript-debug--pinned-object-id gdscript-debug--inspector-focused-object-id)
  (gdscript-debug--refresh-inspector-buffer gdscript-debug--pinned-object-id))

(defun gdscript-debug-pin-current-self ()
  (interactive)
  (setq gdscript-debug--pinned-object-id gdscript-debug--current-self
        gdscript-debug--inspector-stack nil)
  (gdscript-debug--refresh-inspector-buffer gdscript-debug--pinned-object-id))

(defun gdscript-debug-unpin ()
  (interactive)
  (setq gdscript-debug--pinned-object-id nil
        gdscript-debug--inspector-stack nil)
  (gdscript-debug--refresh-inspector-buffer gdscript-debug--current-self))

(defun gdscript-debug-fetch-object-ids-detail ()
  (interactive)
  (setq gdscript-debug-state :fetching)
  (mapc #'gdscript-debug-inspect-object (mapcar #'gdscript-object-id->value gdscript-debug--object-ids-to-fetch)))

(defvar gdscript-debug--debug-enter nil
  "Stores last received `debug_enter' command data.")
(defvar gdscript-debug--stack-frame-vars nil
  "Stores last received `stack_frame_vars' command data.")
(defvar gdscript-debug--inspector-focused-object-id nil
  "Stores `object-id' to display in * Inspector * buffer.")
(defvar gdscript-debug--pinned-object-id nil
  "Stores pinned `object-id' to display in * Inspector * buffer.
Pinned `object-id' is explicitely controlled by user.")
(defvar gdscript-debug--stack-dump nil
  "Stores last received `stack_dump' command data.")
(defvar gdscript-debug--selected-stack-dump nil
  "Stores selected `stack-dump' data.")
(defvar gdscript-debug--current-self nil
  "Stores selected `stack-dump' data.")
(defvar gdscript-debug--inspected-objects (make-hash-table)
  "Mapping from `object-id' to `inspect-object' struct")
(defvar gdscript-debug-state nil)
(defvar gdscript-debug--object-ids-to-fetch nil
  "List of all object-id to fetch on demand")
(defvar gdscript-debug--multiline-on (make-hash-table :test #'equal)
  "Stores mapping from (`buffer' . `property-name') to bool indicating that `property-name'
in buffer `buffer' should be rendered multiline.")
(defvar gdscript-debug--inspector-stack nil
  "A stack of inspected objects for breadcrumb rendering.")

(defun gdscript-debug--construct-stack-var-buffer (stack-frame-vars)
  (let ((table (gdscript-debug-table-create)))
    (gdscript-debug--table-add-row table (list "Locals:" "" "") nil)
    (gdscript-debug--add-stack-var-to-table table (gdscript-stack-frame-vars->locals stack-frame-vars))
    (gdscript-debug--table-add-row table (list "Members:" "" "") nil)
    (gdscript-debug--add-stack-var-to-table table (gdscript-stack-frame-vars->members stack-frame-vars))
    (gdscript-debug--table-add-row table (list "Globals:" "" "") nil)
    (gdscript-debug--add-stack-var-to-table table (gdscript-stack-frame-vars->globals stack-frame-vars))
    (with-current-buffer (gdscript-debug--get-stack-frame-vars-buffer)
      (let ((inhibit-read-only t)
            (point (point)))
        (setq header-line-format
              (format "Stack frame vars - %s:%s %s"
                      (gdscript-stack-dump->file gdscript-debug--selected-stack-dump)
                      (gdscript-stack-dump->line gdscript-debug--selected-stack-dump)
                      (gdscript-stack-dump->function-name gdscript-debug--selected-stack-dump)))
        (erase-buffer)
        (insert (gdscript-debug--table-string table " "))
        ;; Keep point in `stack-frame-vars-buffer' as close as possible to previous state
        (goto-char point)
        (set-window-point (get-buffer-window (current-buffer)) (line-beginning-position))))))

(defun gdscript-debug--add-stack-var-to-table (table items)
  (dolist (item items)
    (pcase-let* ((`(,variable . ,variant) item)
                 (print-data (gdscript-debug--pure-stringify variant variable 'stack-frame-vars-buffer)))
      (gdscript-debug--table-add-row
       table
       (list
        (gdscript-debug-variable-face variable)
        (gdscript-print-data->type-name print-data)
        (gdscript-print-data->string-repr print-data))
       (append
        (list 'property-name variable)
        (cond ((gdscript-object-id-p variant)
               (list 'object-id (gdscript-object-id->value variant))))))
      (when (gdscript-object-id-p variant)
        (unless (gethash (gdscript-object-id->value variant) gdscript-debug--inspected-objects)
          (push variant gdscript-debug--object-ids-to-fetch))))))

(defun gdscript-debug--refresh-inspector-buffer (object-id)
  (when-let* ((inspect-object (gethash object-id gdscript-debug--inspected-objects))
              (table (gdscript-debug-table-create)))
    (dolist (property (gdscript-inspect-object->properties inspect-object))
      (let* ((variant (gdscript-property-info->variant property))
             (usage (gdscript-property-info->usage property))
             (_hint (gdscript-property-info->hint property))
             (name (gdscript-property-info->name property))
             (print-data (gdscript-debug--pure-stringify variant name 'inspector-buffer)))
        (when-let ((variant (gdscript-property-info->variant property))
                   (is-object-id (gdscript-object-id-p variant)))
          (unless (gethash (gdscript-object-id->value variant) gdscript-debug--inspected-objects)
            (push variant gdscript-debug--object-ids-to-fetch)))
        (gdscript-debug--table-add-row
         table
         (cond ((eq 256 (logand 256 usage))
                (list
                 (propertize name 'font-lock-face 'bold)
                 ""
                 ""))
               (t (list
                   ;;(concat (format "[%s]" usage) (format "[%s] " hint) name)
                   name
                   (gdscript-print-data->type-name print-data)
                   (gdscript-print-data->string-repr print-data))))
         (append
          (list 'property-name name)
          (cond ((equal name "Node/path")
                 (list 'node-path (substring-no-properties (gdscript-print-data->string-repr print-data)) 'keymap gdscript-debug--show-in-scene-tree-map))
                ((gdscript-object-id-p variant)
                 (list 'object-id (gdscript-object-id->value variant)))
                (t nil))))))
    (delete-dups gdscript-debug--object-ids-to-fetch)
    (with-current-buffer (gdscript-debug--get-inspector-buffer)
      (let ((inhibit-read-only t)
            (class (gdscript-inspect-object->class inspect-object))
            (object-id (gdscript-inspect-object->object-id inspect-object))
            (inspector-stack-car (car gdscript-debug--inspector-stack)))
        (setq header-line-format
              (if gdscript-debug--pinned-object-id
                  (format "Inspector - Pinned: %s" gdscript-debug--pinned-object-id)
                "Inspector"))
        (unless (and
                 inspector-stack-car
                 (equal class     (gdscript-debug-breadcrumb-entry->class inspector-stack-car))
                 (equal object-id (gdscript-debug-breadcrumb-entry->object-id inspector-stack-car)))
          ;; Add new entry to the stack only when it differs from the tip (car)
          ;; to preserve `gdscript-debug-breadcrumb-entry->point' of the tip
          (push (gdscript-debug-breadcrumb-entry-create :class class :object-id object-id :point (point)) gdscript-debug--inspector-stack))
        (erase-buffer)
        (insert (gdscript-debug--inspector-bread-crumb))
        (insert "\n")
        (insert (gdscript-debug--table-string table " "))
        (goto-char (point-min))
        (gdscript-debug--display-buffer (current-buffer))))))

(defun gdscript-debug--show-in-scene-tree ()
  (interactive)
  (if-let* ((node-path (get-text-property (point) 'node-path)))
      (gdscript-debug--jump-to-node-path node-path)
    (message "Not recognized as node-path line")))

(defun gdscript-debug--jump-to-node-path (node-path)
  (with-current-buffer (gdscript-debug--get-scene-tree-buffer)
    (if (equal node-path "/root")
        (let ((window (display-buffer (current-buffer) '((display-buffer-same-window)))))
          (set-window-point window 1))
      (let ((change-pos 1))
        (while change-pos
          (setq change-pos (next-single-property-change change-pos 'node-path))
          (if change-pos
              (if (equal (get-text-property change-pos 'node-path) node-path)
                  (let ((window (display-buffer (current-buffer) '((display-buffer-same-window)))))
                    (set-window-point window change-pos)
                    (setq change-pos nil))
                (setq change-pos (1+ change-pos)))
            (setq gdscript-debug--after-refresh-function (lambda () (gdscript-debug--jump-to-node-path node-path)))
            (gdscript-debug-request-scene-tree)))))))

(defvar gdscript-debug--show-in-scene-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap gdscript-debug-inspect-object-id] 'gdscript-debug--show-in-scene-tree)
    map)
  "Keymap for `Node/path' in `Inspector' buffer.")

(defun gdscript-debug--inspector-bread-crumb ()
  (mapconcat (lambda (breadcrumb-entry)
               (propertize (format "%s: %s"
                                   (gdscript-debug-breadcrumb-entry->class breadcrumb-entry)
                                   (gdscript-debug-breadcrumb-entry->object-id breadcrumb-entry))
                           'font-lock-face 'bold))
             (reverse gdscript-debug--inspector-stack)
             " > "))

(defun gdscript-debug--table-string (table &optional sep)
  "Return TABLE as a string with columns separated with SEP."
  (let ((column-sizes (gdscript-debug-table->column-sizes table)))
    (mapconcat
     'identity
     (cl-mapcar
      (lambda (row properties)
        (apply 'propertize
               (mapconcat 'identity
                          (cl-mapcar (lambda (s x) (gdscript-debug-pad-string s x))
                                     row column-sizes)
                          sep)
               properties))
      (gdscript-debug-table->rows table)
      (gdscript-debug-table->row-properties table))
     "\n")))

(defun gdscript-debug-pad-string (string padding)
  (format (concat "%" (number-to-string padding) "s") string))

(defun gdscript-debug--pure-stringify (variant property-name buffer-type &optional inline-only)
  (gdscript-debug--to-print-data
   (cond ((gdscript-null-p variant)
          (list
           (gdscript-debug--builtin-face "null")))
         ((gdscript-bool-p variant)
          (list
           (gdscript-debug--builtin-face "bool")
           (gdscript-debug--gdscript-bool-to-string variant)))
         ((gdscript-integer-p variant)
          (list
           (gdscript-debug--builtin-face "int")
           (number-to-string (gdscript-integer->value variant))))
         ((gdscript-float-p variant)
          (list
           (gdscript-debug--builtin-face "float")
           (number-to-string (gdscript-float->value variant))))
         ((gdscript-string-p variant)
          (list
           (gdscript-debug-type-face "String")
           (gdscript-debug--string-face (gdscript-string->value variant))))
         ((gdscript-vector2-p variant)
          (list
           (gdscript-debug-type-face "Vector2")
           (gdscript-debug--vector2-to-string variant)))
         ((gdscript-rect2-p variant)
          (list
           (gdscript-debug-type-face "Rect2")
           (concat (gdscript-debug--vector2-to-string (gdscript-rect2->coordinate variant)) " " (gdscript-debug--vector2-to-string (gdscript-rect2->size variant)))))
         ((gdscript-vector3-p variant)
          (list
           (gdscript-debug-type-face "Vector3")
           (gdscript-debug--vector3-to-string variant)))
         ((gdscript-transform2d-p variant)
          (list
           (gdscript-debug-type-face "Transform2D")
           (mapconcat #'gdscript-debug--vector2-to-string (list (gdscript-transform2d->x variant) (gdscript-transform2d->y variant) (gdscript-transform2d->origin variant)) " ")))
         ((gdscript-plane-p variant)
          (list
           (gdscript-debug-type-face "Plane")
           (concat (gdscript-debug--vector3-to-string (gdscript-plane->normal variant)) " " (number-to-string (gdscript-plane->distance variant)))))
         ((gdscript-quat-p variant)
          (list
           (gdscript-debug-type-face "Quat")
           (mapconcat #'number-to-string (list (gdscript-quat->x-imaginary variant) (gdscript-quat->y-imaginary variant) (gdscript-quat->z-imaginary variant) (gdscript-quat->real-w variant)) " ")))
         ((gdscript-aabb-p variant)
          (list
           (gdscript-debug-type-face "AABB")
           (mapconcat #'gdscript-debug--vector3-to-string (list (gdscript-aabb->position variant) (gdscript-aabb->size variant)) " ")))
         ((gdscript-basis-p variant)
          (list
           (gdscript-debug-type-face "Basis")
           (gdscript-debug--basis-to-string variant)))
         ((gdscript-transform-p variant)
          (list
           (gdscript-debug-type-face "Transform")
           (concat (gdscript-debug--basis-to-string (gdscript-transform->basis variant)) " " (gdscript-debug--vector3-to-string (gdscript-transform->origin variant)))))
         ((gdscript-color-p variant)
          (list
           (gdscript-debug-type-face "Color")
           (gdscript-debug--color-to-string variant)))
         ((gdscript-node-path-p variant)
          (list
           (gdscript-debug-type-face "NodePath")
           (gdscript-debug--constant-face (concat (when (gdscript-bool->value (gdscript-node-path->absolute variant)) "/") (mapconcat #'gdscript-string->value (gdscript-node-path->names variant) "/") (when-let* ((subnames (gdscript-node-path->subnames variant))) (concat ":" (mapconcat #'gdscript-string->value subnames ":")))))))
         ((gdscript-rid-p variant)
          (list
           (gdscript-debug-type-face "RID")))
         ((gdscript-object-id-p variant)
          (let* ((object-id (gdscript-object-id->value variant))
                 (object-id-data (gethash object-id gdscript-debug--inspected-objects)))
            (list
             (if object-id-data (gdscript-debug-type-face (gdscript-inspect-object->class object-id-data)) "ObjectID")
             (concat (gdscript-debug--comment-face (format "ObjectID: %s" (number-to-string object-id)))
                     (when object-id-data
                       (let* ((property (car (gdscript-inspect-object->properties object-id-data)))
                              (name (gdscript-property-info->name property)))
                         (when (equal name "Node/path")
                           (concat " " (gdscript-print-data->string-repr (gdscript-debug--pure-stringify (gdscript-property-info->variant property) property-name buffer-type t))))))))))
         ((gdscript-dictionary-p variant)
          (list
           (gdscript-debug-type-face "Dictionary")
           (gdscript-debug--dictionary-inline-string variant property-name buffer-type)
           (gdscript-debug--dictionary-multiline-string variant property-name buffer-type)))
         ((gdscript-array-p variant)
          (list
           (gdscript-debug-type-face "Array")
           (concat "[" (mapconcat (lambda (element) (gdscript-print-data->string-repr (gdscript-debug--pure-stringify element property-name buffer-type t))) (gdscript-array->elements variant) ", ") "]")
           (concat "[" (mapconcat (lambda (element) (gdscript-print-data->string-repr (gdscript-debug--pure-stringify element property-name buffer-type t))) (gdscript-array->elements variant) ", ") "]")))
         ((gdscript-pool-byte-array-p variant)
          (list
           (gdscript-debug-type-face "PoolByteArray")
           (concat "[" (mapconcat #'number-to-string (gdscript-pool-byte-array->elements variant) " ") "]")
           (concat "[" (mapconcat #'number-to-string (gdscript-pool-byte-array->elements variant) " ") "]")))
         ((gdscript-pool-int-array-p variant)
          (list
           (gdscript-debug-type-face "PoolIntArray")
           (concat "[" (mapconcat (lambda (int) (number-to-string (gdscript-integer->value int))) (gdscript-pool-int-array->elements variant) " ") "]")
           (concat "[" (mapconcat (lambda (int) (number-to-string (gdscript-integer->value int))) (gdscript-pool-int-array->elements variant) " ") "]")))
         ((gdscript-pool-real-array-p variant)
          (list
           (gdscript-debug-type-face "PoolRealArray")
           (gdscript-debug--pool-real-array-inline-string variant)
           (gdscript-debug--pool-real-array-multiline-string variant)))
         ((gdscript-pool-string-array-p variant)
          (list
           (gdscript-debug-type-face "PoolStringArray")
           (gdscript-debug--pool-string-array-inline-string variant)
           (gdscript-debug--pool-string-array-multiline-string variant)))
         ((gdscript-pool-vector2-array-p variant)
          (list
           (gdscript-debug-type-face "PoolVector2Array")
           (gdscript-debug--pool-vector2-array-inline-string variant)
           (gdscript-debug--pool-vector2-array-multiline-string variant)))
         ((gdscript-pool-vector3-array-p variant)
          (list
           (gdscript-debug-type-face "PoolVector3Array")
           (gdscript-debug--pool-vector3-array-inline-string variant)
           (gdscript-debug--pool-vector3-array-multiline-string variant)))
         ((gdscript-pool-color-array-p variant)
          (list
           (gdscript-debug-type-face "PoolColorArray")
           (gdscript-debug--pool-color-array-inline-string variant)
           (gdscript-debug--pool-color-array-multiline-string variant)))
         (t (error "[gdscript-debug--pure-stringify] Invalid type %s" variant)))
   property-name
   buffer-type
   inline-only))

(defun gdscript-debug--initial-inline-visibility (string init)
  (propertize string 'invisible init 'inline t))

(defun gdscript-debug--initial-multiline-visibility (string init)
  (propertize string 'invisible init 'multiline t))

(defun gdscript-debug--dictionary-inline-string (is-object-id property-name buffer-type)
  (concat "{" (mapconcat (lambda (key-value)
                           (gdscript-debug--key-value-to-string key-value property-name buffer-type)) (gdscript-dictionary->elements is-object-id) ", ") "}"))

(defun gdscript-debug--dictionary-multiline-string (dictionary property-name buffer-type)
  (let ((table (gdscript-debug-table-create)))
    (dolist (element (gdscript-dictionary->elements dictionary))
      (pcase-let ((`(,key . ,value) element))
        (gdscript-debug--table-add-row
         table
         (list
          (concat "  "(gdscript-print-data->string-repr (gdscript-debug--pure-stringify key property-name buffer-type t)) ":")
          (gdscript-print-data->string-repr (gdscript-debug--pure-stringify value property-name buffer-type t)))
         (cond ((gdscript-object-id-p value)
                (list 'object-id (gdscript-object-id->value value)))
               (t nil)))))
    (concat "{\n" (gdscript-debug--table-string table " ") "\n}")))

(defun gdscript-debug--pool-real-array-inline-string (is-object-id)
  (concat "[" (mapconcat (lambda (real) (number-to-string (gdscript-float->value real))) (gdscript-pool-real-array->elements is-object-id) " ") "]"))

(defun gdscript-debug--pool-real-array-multiline-string (is-object-id)
  (concat "[\n" (mapconcat (lambda (real) (concat "  " (number-to-string (gdscript-float->value real)) "\n")) (gdscript-pool-real-array->elements is-object-id) "") "]"))

(defun gdscript-debug--pool-string-array-inline-string (is-object-id)
  (concat "[" (mapconcat (lambda (o)
                           (gdscript-debug--string-face (gdscript-string->value o))) (gdscript-pool-string-array->elements is-object-id) " ") "]"))

(defun gdscript-debug--pool-string-array-multiline-string (is-object-id)
  (concat "[\n" (mapconcat (lambda (o)
                             (concat "  " (gdscript-debug--string-face (gdscript-string->value o)) "\n")) (gdscript-pool-string-array->elements is-object-id) "") "]"))

(defun gdscript-debug--pool-vector2-array-inline-string (is-object-id)
  (concat "[" (mapconcat #'gdscript-debug--vector2-to-string (gdscript-pool-vector2-array->elements is-object-id) " ") "]"))

(defun gdscript-debug--pool-vector2-array-multiline-string (is-object-id)
  (concat "[\n" (mapconcat (lambda (o)
                             (concat "  " (gdscript-debug--vector2-to-string o) "\n")) (gdscript-pool-vector2-array->elements is-object-id) "") "]"))

(defun gdscript-debug--pool-vector3-array-inline-string (is-object-id)
  (concat "[" (mapconcat #'gdscript-debug--vector3-to-string (gdscript-pool-vector3-array->elements is-object-id) " ") "]"))

(defun gdscript-debug--pool-vector3-array-multiline-string (is-object-id)
  (concat "[\n" (mapconcat (lambda (o)
                             (concat "  " (gdscript-debug--vector3-to-string o) "\n")) (gdscript-pool-vector3-array->elements is-object-id) "") "]"))

(defun gdscript-debug--pool-color-array-inline-string (is-object-id)
  (concat "[" (mapconcat #'gdscript-debug--color-to-string (gdscript-pool-color-array->elements is-object-id) " ") "]"))

(defun gdscript-debug--pool-color-array-multiline-string (is-object-id)
  (concat "[\n" (mapconcat (lambda (o)
                             (concat "  " (gdscript-debug--color-to-string o) "\n")) (gdscript-pool-color-array->elements is-object-id) "") "]"))

(defun gdscript-debug--to-print-data (args property-name buffer-type &optional inline-only)
  (pcase args
    (`(,type-name)
     (gdscript-print-data-create
      :type-name type-name
      :string-repr ""))
    (`(,type-name ,inline-string-repr)
     (gdscript-print-data-create
      :type-name type-name
      :string-repr (gdscript-debug--initial-inline-visibility inline-string-repr nil)))
    (`(,type-name ,inline-string-repr ,multiline-string-repr)
     (let ((multiline-invisible (gethash (cons buffer-type property-name) gdscript-debug--multiline-on)))
       (gdscript-print-data-create
        :type-name type-name
        :string-repr (concat
                      (gdscript-debug--initial-inline-visibility inline-string-repr multiline-invisible)
                      (unless inline-only
                        (gdscript-debug--initial-multiline-visibility multiline-string-repr (not multiline-invisible)))))))))

(cl-defstruct (gdscript-print-data (:constructor gdscript-print-data-create)
                                   (:copier nil)
                                   (:conc-name gdscript-print-data->))
  type-name string-repr)

(defun gdscript-debug--keyword-face (string)
  (propertize string 'font-lock-face font-lock-keyword-face))

(defun gdscript-debug--string-face (string)
  (propertize string 'font-lock-face font-lock-string-face))

(defun gdscript-debug--constant-face (string)
  (propertize string 'font-lock-face font-lock-constant-face))

(defun gdscript-debug--comment-face (string)
  (propertize string 'font-lock-face font-lock-comment-face))

(defun gdscript-debug--builtin-face (string)
  (propertize string 'font-lock-face font-lock-builtin-face))

(defun gdscript-debug-type-face (string)
  (propertize string 'font-lock-face font-lock-type-face))

(defun gdscript-debug-variable-face (string)
  (propertize string 'font-lock-face font-lock-variable-name-face))

(defun gdscript-debug--key-value-to-string (key-value property-name buffer-type)
  (let ((key (car key-value))
        (value (cdr key-value)))
    (concat (gdscript-print-data->string-repr (gdscript-debug--pure-stringify key property-name buffer-type t))
            ":"
            (gdscript-print-data->string-repr (gdscript-debug--pure-stringify value property-name buffer-type t)))))

(defun gdscript-debug--gdscript-bool-to-string (gdscript-bool)
  (if (gdscript-bool->value gdscript-bool) (gdscript-debug--keyword-face "true") (gdscript-debug--keyword-face "false")))

(defun gdscript-debug--color-to-string (color)
  (format "(%s)" (mapconcat #'number-to-string (list (gdscript-color->red color) (gdscript-color->green color) (gdscript-color->blue color) (gdscript-color->alpha color)) ", ")))

(defun gdscript-debug--basis-to-string (basis)
  (mapconcat #'gdscript-debug--vector3-to-string (list (gdscript-basis->x basis) (gdscript-basis->y basis) (gdscript-basis->z basis)) " "))

(defun gdscript-debug--vector2-to-string (vector2)
  (concat "(" (number-to-string (gdscript-vector2->x vector2)) ", " (number-to-string (gdscript-vector2->y vector2)) ")"))

(defun gdscript-debug--vector3-to-string (vector3)
  (concat "(" (number-to-string (gdscript-vector3->x vector3)) ", " (number-to-string (gdscript-vector3->y vector3)) ", " (number-to-string (gdscript-vector3->z vector3)) ")"))

(defvar gdscript-server-clients '()
  "List with client processes")

(defun gdscript-debug--sentinel-function (process event)
  "Gets called when the status of the network connection changes."
  ;; (message "[sentinel] process: %s" process)
  ;; (message "[sentinel] event  : %s" event)
  (cond
   ((string-match "open from .*\n" event)
    (push process gdscript-server-clients))
   ((or
     (string= event "connection broken by remote peer\n")
     (string= event "deleted\n"))
    (set-marker gdscript-debug--thread-position nil)
    (setq gdscript-debug--previous-packet-data nil
          gdscript-debug--offset 0
          gdscript-debug--data-needed nil
          gdscript-debug--inspected-objects (make-hash-table)
          gdscript-debug--multiline-on (make-hash-table :test #'equal)
          gdscript-debug--inspector-stack nil
          gdscript-debug--inspector-focused-object-id nil
          gdscript-debug--pinned-object-id nil
          gdscript-server-clients (seq-remove (lambda (client-process)
                                                (string=
                                                 (process-name client-process)
                                                 (process-name process)))
                                              gdscript-server-clients))
    (message "Resetting server to accept data."))
   ((eq (process-status process) 'closed)
    (message "Process Closed"))))

(defmacro gdscript-debug--send-command (&rest body)
  "Todo"
  (declare (indent 0) (debug t))
  `(pcase gdscript-server-clients
     (`() (message "No game process is running."))
     (`(,server-process)
      (let ((command (progn ,@body)))
        (process-send-string server-process command)))
     (_ (message "More than one game process running"))))

(defmacro gdscript-debug--if-server-process (&rest body)
  "Todo"
  (declare (indent 0) (debug t))
  `(pcase gdscript-server-clients
     (`(,server-process) (progn ,@body))))

(defun gdscript-debug-inspect-object (object-id)
  (gdscript-debug--send-command
    (gdscript-debug--inspect-object object-id)))

(defun gdscript-debug-request-scene-tree()
  (interactive)
  (gdscript-debug--send-command (gdscript-debug--command "request_scene_tree")))

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

  (if-let* ((project-root (gdscript-util--find-project-configuration-file)))
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
        (message "Debugger server started - project: %s" project-root))
    (error "Not in Godot project!")))

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

(defconst gdscript-variant-bool 1 "bool")
(defconst gdscript-variant-integer 2 "integer")
(defconst gdscript-variant-string 4 "string")
(defconst gdscript-variant-array 19 "array")

(defun gdscript-debug--boolean-to-integer (b)
  (if (null b) 0 1))

(defun gdscript-debug--inspect-object (object-id)
  (let* ((command "inspect_object")
         (command-length (length command))
         (command-alength (gdscript-debug-align-length command))
         (packet-length (+ (* 6 4) command-alength))
         (spec (gdscript-debug--inspect-object-definition command-length)))
    (bindat-pack spec
                 `((:packet-length . ,packet-length)
                   (:array-type . ,gdscript-variant-array)
                   (:elements-count . 2)
                   (:command-type . ,gdscript-variant-string)
                   (:command-length . ,command-length)
                   (:command . ,command)
                   (:object-id-type . ,gdscript-variant-integer)
                   (:object-id . ,object-id)))))

(defun gdscript-debug--get-stack-frame-vars (frame)
  (let* ((command "get_stack_frame_vars")
         (command-length (length command))
         (command-alength (gdscript-debug-align-length command))
         (packet-length (+ (* 6 4) command-alength))
         (spec (gdscript-debug--get-stack-frame-vars-definition command-length)))
    (bindat-pack spec
                 `((:packet-length . ,packet-length)
                   (:array-type . ,gdscript-variant-array)
                   (:elements-count . 2)
                   (:command-type . ,gdscript-variant-string)
                   (:command-length . ,command-length)
                   (:command . ,command)
                   (:frame-type . ,gdscript-variant-integer)
                   (:frame . ,frame)))))

(defun gdscript-debug--breakpoint-command (file line add-or-remove)
  (let* ((command "breakpoint")
         (command-length (length command))
         (command-alength (gdscript-debug-align-length command))
         (file-length (length file))
         (file-alength (gdscript-debug-align-length file))
         (packet-length (+ (* 10 4) command-alength file-alength))
         (spec (gdscript-debug--breakpoint-packet-definition command-length file-length)))
    (bindat-pack spec
                 `((:packet-length . ,packet-length)
                   (:array-type . ,gdscript-variant-array)
                   (:elements-count . 4)
                   (:command-type . ,gdscript-variant-string)
                   (:command-length . ,command-length)
                   (:command . ,command)
                   (:file-type . ,gdscript-variant-string)
                   (:file-length . ,file-length)
                   (:file . ,file)
                   (:line-type . ,gdscript-variant-integer)
                   (:line . ,line)
                   (:boolean-type . ,gdscript-variant-bool)
                   (:boolean . ,(gdscript-debug--boolean-to-integer add-or-remove))))))

(defun gdscript-debug--set-skip-breakpoints-command (skip)
  (let* ((command "set_skip_breakpoints")
         (command-length (length command))
         (command-alength (gdscript-debug-align-length command))
         (packet-length (+ (* 6 4) command-alength))
         (spec (gdscript-debug--set-skip-breakpoints-packet-definition command-length)))
    (bindat-pack spec
                 `((:packet-length . ,packet-length)
                   (:array-type . ,gdscript-variant-array)
                   (:elements-count . 2)
                   (:command-type . ,gdscript-variant-string)
                   (:command-length . ,command-length)
                   (:command . ,command)
                   (:boolean-type . ,gdscript-variant-bool)
                   (:boolean . ,(gdscript-debug--boolean-to-integer skip))))))

(defun gdscript-debug--packet-definition (string-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:type u32r)
    (:string-length u32r)
    (:string-data str ,string-length)
    (align 4)))

(defun gdscript-debug--command (command)
  (let* ((command-alength (gdscript-debug-align-length command))
         (packet-length (+ (* 4 4) command-alength)))
    (bindat-pack
     (gdscript-debug--packet-definition (length command))
     `((:packet-length . ,packet-length)
       (:array-type . ,gdscript-variant-array)
       (:elements-count . 1)
       (:type . ,gdscript-variant-string)
       (:string-length . ,(length command))
       (:string-data . ,command)))))

(defun gdscript-debug-align-length (string)
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

(defun gdscript-debug-get-stack-frame-vars (level)
  (gdscript-debug--send-command
    (gdscript-debug--get-stack-frame-vars level)))

(cl-defstruct (gdscript-null (:constructor gdscript-null-create)
                             (:copier nil)))

(cl-defstruct (gdscript-bool (:constructor gdscript-bool-create)
                             (:copier nil)
                             (:conc-name gdscript-bool->))
  value)

(cl-defstruct (gdscript-integer (:constructor gdscript-integer-create)
                                (:copier nil)
                                (:conc-name gdscript-integer->))
  value)

(cl-defstruct (gdscript-float (:constructor gdscript-float-create)
                              (:copier nil)
                              (:conc-name gdscript-float->))
  value)

(cl-defstruct (gdscript-string (:constructor gdscript-string-create)
                               (:copier nil)
                               (:conc-name gdscript-string->))
  value)

(cl-defstruct (gdscript-plane (:constructor gdscript-plane-create)
                              (:copier nil)
                              (:conc-name gdscript-plane->))
  normal distance)

(cl-defstruct (gdscript-quat (:constructor gdscript-quat-create)
                             (:copier nil)
                             (:conc-name gdscript-quat->))
  x-imaginary y-imaginary z-imaginary real-w)

(cl-defstruct (gdscript-aabb (:constructor gdscript-aabb-create)
                             (:copier nil)
                             (:conc-name gdscript-aabb->))
  position size)

(cl-defstruct (gdscript-basis (:constructor gdscript-basis-create)
                              (:copier nil)
                              (:conc-name gdscript-basis->))
  x y z)

(cl-defstruct (gdscript-transform (:constructor gdscript-transform-create)
                                  (:copier nil)
                                  (:conc-name gdscript-transform->))
  basis origin)

(cl-defstruct (gdscript-color (:constructor gdscript-color-create)
                              (:copier nil)
                              (:conc-name gdscript-color->))
  red green blue alpha)

(cl-defstruct (gdscript-node-path (:constructor gdscript-node-path-create)
                                  (:copier nil)
                                  (:conc-name gdscript-node-path->))
  names subnames absolute)

(cl-defstruct (gdscript-rid (:constructor gdscript-rid-create)
                            (:copier nil)))

(cl-defstruct (gdscript-object-id (:constructor gdscript-object-id-create)
                                  (:copier nil)
                                  (:conc-name gdscript-object-id->))
  value)

(cl-defstruct (gdscript-dictionary (:constructor gdscript-dictionary-create)
                                   (:copier nil)
                                   (:conc-name gdscript-dictionary->))
  shared elements)

(cl-defstruct (gdscript-vector2 (:constructor gdscript-vector2-create)
                                (:copier nil)
                                (:conc-name gdscript-vector2->))
  x y)

(cl-defstruct (gdscript-rect2 (:constructor gdscript-rect2-create)
                              (:copier nil)
                              (:conc-name gdscript-rect2->))
  coordinate size)

(cl-defstruct (gdscript-vector3 (:constructor gdscript-vector3-create)
                                (:copier nil)
                                (:conc-name gdscript-vector3->))
  x y z)

(cl-defstruct (gdscript-transform2d (:constructor gdscript-transform2d-create)
                                    (:copier nil)
                                    (:conc-name gdscript-transform2d->))
  x y origin)

(cl-defstruct (gdscript-array (:constructor gdscript-array-create)
                              (:copier nil)
                              (:conc-name gdscript-array->))
  shared elements)

(cl-defstruct (gdscript-pool-byte-array (:constructor gdscript-pool-byte-array-create)
                                        (:copier nil)
                                        (:conc-name gdscript-pool-byte-array->))
  elements)

(cl-defstruct (gdscript-pool-int-array (:constructor gdscript-pool-int-array-create)
                                       (:copier nil)
                                       (:conc-name gdscript-pool-int-array->))
  elements)

(cl-defstruct (gdscript-pool-real-array (:constructor gdscript-pool-real-array-create)
                                        (:copier nil)
                                        (:conc-name gdscript-pool-real-array->))
  elements)

(cl-defstruct (gdscript-pool-string-array (:constructor gdscript-pool-string-array-create)
                                          (:copier nil)
                                          (:conc-name gdscript-pool-string-array->))
  elements)

(cl-defstruct (gdscript-pool-vector2-array (:constructor gdscript-pool-vector2-array-create)
                                           (:copier nil)
                                           (:conc-name gdscript-pool-vector2-array->))
  elements)

(cl-defstruct (gdscript-pool-vector3-array (:constructor gdscript-pool-vector3-array-create)
                                           (:copier nil)
                                           (:conc-name gdscript-pool-vector3-array->))
  elements)

(cl-defstruct (gdscript-pool-color-array (:constructor gdscript-pool-color-array-create)
                                         (:copier nil)
                                         (:conc-name gdscript-pool-color-array->))
  elements)

(cl-defstruct (gdscript-stack-frame-vars (:constructor gdscript-stack-frame-vars-create)
                                         (:copier nil)
                                         (:conc-name gdscript-stack-frame-vars->))
  locals members globals)

(cl-defstruct (gdscript-stack-dump (:constructor gdscript-stack-dump-create)
                                   (:copier nil)
                                   (:conc-name gdscript-stack-dump->))
  file line function-name level)

(cl-defstruct (gdscript-output-error (:constructor gdscript-output-error-create)
                                     (:copier nil)
                                     (:conc-name gdscript-output-error->))
  ;; See struct OutputError in script_debugger_remote.h
  hr          ;; int
  min         ;; int
  sec         ;; int
  msec        ;; int
  source-file ;; String
  source-func ;; String
  source-line ;; int
  error       ;; String
  error-descr ;; String
  warning     ;; bool
  callstack)  ;; Array

(cl-defstruct (gdscript-stack-info (:constructor sgdscript-tack-info-create)
                                   (:copier nil)
                                   (:conc-name gdscript-stack-info->))
  file func line)

(cl-defstruct (gdscript-inspect-object (:constructor gdscript-inspect-object-create)
                                       (:copier nil)
                                       (:conc-name gdscript-inspect-object->))
  object-id class properties)

(cl-defstruct (gdscript-property-info (:constructor gdscript-property-info-create)
                                      (:copier nil)
                                      (:conc-name gdscript-property-info->))
  name
  type
  hint ;; see enum PropertyHint in object.h
  hint-string
  usage ;; see enum PropertyUsageFlags in object.h
  variant)

(cl-defstruct (gdscript-breakpoint (:constructor gdscript-breakpoint-create)
                                   (:copier nil)
                                   (:conc-name gdscript-breakpoint->))
  file file-absolute line)

(cl-defstruct (gdscript-debug-enter (:constructor gdscript-debug-enter-create)
                                    (:copier nil)
                                    (:conc-name gdscript-debug-enter->))
  can-continue reason)

(cl-defstruct (gdscript-debug-table (:constructor gdscript-debug-table-create)
                                    (:copier nil)
                                    (:conc-name gdscript-debug-table->))
  column-sizes
  rows
  row-properties
  right-align)

(cl-defstruct (gdscript-debug-breadcrumb-entry (:constructor gdscript-debug-breadcrumb-entry-create)
                                               (:copier nil)
                                               (:conc-name gdscript-debug-breadcrumb-entry->))
  class object-id point)

(defun gdscript-debug--table-add-row (table row &optional properties)
  "Add ROW of string to TABLE and recalculate column sizes.

When non-nil, PROPERTIES will be added to the whole row when
calling `gdscript-debug--table-string'."
  (let ((rows (gdscript-debug-table->rows table))
        (row-properties (gdscript-debug-table->row-properties table))
        (column-sizes (gdscript-debug-table->column-sizes table))
        (right-align (gdscript-debug-table->right-align table)))
    (when (not column-sizes)
      (setf (gdscript-debug-table->column-sizes table)
            (make-list (length row) 0)))
    (setf (gdscript-debug-table->rows table)
          (append rows (list row)))
    (setf (gdscript-debug-table->row-properties table)
          (append row-properties (list properties)))
    (setf (gdscript-debug-table->column-sizes table)
          (cl-mapcar (lambda (x s)
                       (let ((new-x
                              (max (abs x) (string-width (or s "")))))
                         (if right-align new-x (- new-x))))
                     (gdscript-debug-table->column-sizes table)
                     row))
    ;; Avoid trailing whitespace at eol
    (if (not (gdscript-debug-table->right-align table))
        (setcar (last (gdscript-debug-table->column-sizes table)) 0))))

(defun gdscript-debug--parent-mode ()
  "Generic mode to derive all other buffer modes from."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun gdscript-debug-toggle-breakpoint ()
  "Add or remove a breakpoint on the line at POINT."
  (interactive)
  (let* ((breakpoint (gdscript-debug--construct-breakpoint-at-point)))
    (if (gdscript-debug--is-existing-breakpoint-p breakpoint)
      (gdscript-debug--remove-breakpoint breakpoint)
      (gdscript-debug--add-breakpoint-to-line breakpoint))))

(defun gdscript-debug--construct-breakpoint-at-point ()
  "Construct a breakpoint object for the line at POINT and return it.
May match an existing breakpoint."
  (gdscript-debug--with-gdscript-file file-info
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (line (line-number-at-pos))
           (file (car file-info))
           (file-absolute (cdr file-info))
           (breakpoint (gdscript-breakpoint-create :file file :file-absolute file-absolute :line line)))
      breakpoint)))

(defun gdscript-debug--is-existing-breakpoint-p (breakpoint)
  "Return t if `BREAKPOINT' is an existing breakpoint in the project."
  (member breakpoint gdscript-debug--breakpoints))

(defun gdscript-debug--add-breakpoint-to-line (breakpoint)
  "Register `BREAKPOINT' to the current line in a GDScript buffer."
  (gdscript-debug--with-gdscript-file file-info
    (let* ((line (line-number-at-pos))
           (file (car file-info)))
      (if (member breakpoint gdscript-debug--breakpoints)
          (message "Breakpoint already present at %s:%s" file line)
        (gdscript-debug--add-fringe (line-beginning-position) (not gdscript-debug--skip-breakpoints) 'gdb-bptno 1)
        (gdscript-debug--add-breakpoint-to-buffer breakpoint)
        (gdscript-debug-refresh-breakpoints-buffer)
        (gdscript-debug--if-server-process
          (gdscript-debug--send-command
            (gdscript-debug--breakpoint-command file line t)))))))

(defun gdscript-debug--remove-breakpoint (breakpoint)
  "Remove `BREAKPOINT' to the current line in a GDScript buffer."
  (gdscript-debug--with-gdscript-file file-info
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (line (line-number-at-pos))
           (file (car file-info)))
      (if (not (member breakpoint gdscript-debug--breakpoints))
          (message "No breakpoint at %s:%s" file line)
        (gdscript-debug--remove-strings start end)
        (gdscript-debug--remove-breakpoint-from-buffer breakpoint)
        (gdscript-debug--if-server-process
          (gdscript-debug--send-command
            (gdscript-debug--breakpoint-command file line nil)))))))

(defun gdscript-debug--set-left-fringe-breakpoints (enabled)
  (gdscript-debug-refresh-breakpoints-buffer)
  (dolist (breakpoint gdscript-debug--breakpoints)
    (let ((file (gdscript-breakpoint->file-absolute breakpoint))
          (line (gdscript-breakpoint->line breakpoint)))
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
                           (prop `(left-fringe breakpoint ,(if (not enabled) 'breakpoint-enabled 'breakpoint-disabled))))
                      (put-text-property 0 1 'display prop string))))))))))))

(defun gdscript-debug-toggle-skip-breakpoints ()
  "Toggle the execution of all breakpoints without removing them.
Like Godot's Skip Breakpoints button."
  (interactive)
  (setq gdscript-debug--skip-breakpoints (not gdscript-debug--skip-breakpoints))
  (gdscript-debug--set-left-fringe-breakpoints gdscript-debug--skip-breakpoints)
  (gdscript-debug--send-command
    (gdscript-debug--set-skip-breakpoints-command gdscript-debug--skip-breakpoints)))

(defun gdscript-debug-delete-breakpoint ()
  (interactive)
  (if-let* ((breakpoint (get-text-property (point) 'gdscript-debug--breakpoint)))
      (let ((file (gdscript-breakpoint->file-absolute breakpoint))
            (line (gdscript-breakpoint->line breakpoint)))
        (save-selected-window
          (let ((buffer (find-file-noselect file)))
            (with-current-buffer buffer
              (goto-char (point-min))
              (forward-line (1- line))
              (gdscript-debug--remove-breakpoint breakpoint)))))
    (message "Not recognized as breakpoint line")))

(defun gdscript-debug-goto-breakpoint ()
  (interactive)
  (if-let* ((breakpoint (get-text-property (point) 'gdscript-debug--breakpoint)))
      (let ((file (gdscript-breakpoint->file-absolute breakpoint))
            (line (gdscript-breakpoint->line breakpoint)))
        (save-selected-window
          (let* ((buffer (find-file-noselect file))
                 (window (gdscript-debug--display-buffer buffer)))
            (with-current-buffer buffer
              (goto-char (point-min))
              (forward-line (1- line))
              (set-window-point window (point))))))
    (message "Not recognized as breakpoint line")))

(defun gdscript-debug-inspect-object-id ()
  (interactive)
  (save-excursion
    (when (or (derived-mode-p 'gdscript-debug--scene-tree-mode)
              (derived-mode-p 'gdscript-debug--stack-frame-vars-mode))
      (setq gdscript-debug--inspector-stack nil))
    (if-let* ((object-id (get-text-property (point) 'object-id)))
        (progn
          (setq gdscript-debug--inspector-focused-object-id object-id)
          (gdscript-debug--show-object-id object-id))
      (message "Not recognized as object-id line"))))

(defun gdscript-debug--show-object-id (object-id)
  (if (gethash object-id gdscript-debug--inspected-objects)
      (gdscript-debug--refresh-inspector-buffer object-id)
    (if gdscript-debug-state
        (message "Cannot inspect object-id now")
      (setq gdscript-debug-state :refresh-inspector)
      (gdscript-debug-inspect-object object-id))))

(defun gdscript-debug-show-stack-frame-vars ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if-let* ((stack (get-text-property (point) 'gdscript-debug--stack-dump)))
        (progn (setq gdscript-debug--selected-stack-dump stack)
               (gdscript-debug-get-stack-frame-vars (gdscript-stack-dump->level stack)))
      (message "Not recognized as stack-frame line"))))

(defun gdscript-debug-jump-to-stack-point ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if-let* ((stack-dump (get-text-property (point) 'gdscript-debug--stack-dump))
              (project-root (get-text-property (point) 'gdscript-debug--project-root)))
        (let* ((file (gdscript-stack-dump->file stack-dump))
               (line (gdscript-stack-dump->line stack-dump))
               (full-file-path (concat project-root (gdscript-debug--drop-res file))))
          (with-current-buffer (find-file full-file-path)
            (let* ((posns (gdscript-debug--line-posns line))
                   (start-posn (car posns)))
              (goto-char start-posn))))
      (message "Not recognized as stack-frame line"))))

(defun gdscript-debug-go-back ()
  (interactive)
  (cond
   ((>= 1 (length gdscript-debug--inspector-stack))
    (switch-to-buffer (gdscript-debug--get-stack-frame-vars-buffer)))
   (t
    (let* ((last-breadcrumb (pop gdscript-debug--inspector-stack))
           (show-breadcrumb (car gdscript-debug--inspector-stack))
           (object-id (gdscript-debug-breadcrumb-entry->object-id show-breadcrumb)))
      (setq gdscript-debug--inspector-focused-object-id object-id)
      (gdscript-debug--show-object-id object-id)
      (goto-char (gdscript-debug-breadcrumb-entry->point last-breadcrumb))))))

(defun gdscript-debug-toggle-visibility ()
  (interactive)
  (when-let ((is-on-multiline (get-pos-property (point) 'multiline)))
    (goto-char (previous-single-property-change (point) 'multiline)))
  (when (get-text-property (line-end-position) 'multiline)
    (let* ((inhibit-read-only t)
           (start (line-beginning-position))
           (end (line-end-position))
           (property-name (get-text-property start 'property-name))
           (inline-start (next-single-property-change start 'inline (current-buffer) end))
           (inline-end (next-single-property-change inline-start 'inline))
           (multiline-start inline-end)
           (multiline-end (next-single-property-change multiline-start 'multiline))
           (inline-invisible (get-pos-property inline-end 'invisible))
           (multiline-invisible (get-pos-property multiline-end 'invisible)))
      (put-text-property inline-start inline-end 'invisible (not inline-invisible))
      (put-text-property multiline-start multiline-end 'invisible (not multiline-invisible))
      (puthash (cons gdscript-debug--buffer-type property-name) multiline-invisible gdscript-debug--multiline-on)
      (when inline-invisible (goto-char start)))))

(defvar gdscript-debug--stack-dump-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    (define-key map " " 'gdscript-debug-jump-to-stack-point)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "l" 'gdscript-debug-display-stack-frame-vars-buffer)
    (define-key map "\r" 'gdscript-debug-show-stack-frame-vars)
    (define-key map "?" 'describe-mode)
    (define-key map (kbd "C-c n") 'gdscript-debug-hydra)
    (define-key map (kbd "C-c r") 'gdscript-hydra-show)
    map))

(defvar gdscript-debug--breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map " " 'gdscript-debug-toggle-skip-breakpoints)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "D" 'gdscript-debug-delete-breakpoint)
    (define-key map "\r" 'gdscript-debug-goto-breakpoint)
    (define-key map "\t" 'gdscript-debug-display-stack-dump-buffer)
    (define-key map "?" 'describe-mode)
    (define-key map (kbd "C-c n") 'gdscript-debug-hydra)
    (define-key map (kbd "C-c r") 'gdscript-hydra-show)
    map))

(defvar gdscript-debug--stack-frame-vars-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "d" 'gdscript-debug-fetch-object-ids-detail)
    (define-key map "l" 'gdscript-debug-display-stack-dump-buffer)
    (define-key map "n" 'next-line)
    (define-key map "o" 'gdscript-debug-pin-inspected-object-id)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "u" 'gdscript-debug-unpin)
    (define-key map "\r" 'gdscript-debug-inspect-object-id)
    (define-key map "\t" 'gdscript-debug-toggle-visibility)
    (define-key map "?" 'describe-mode)
    (define-key map (kbd "C-c n") 'gdscript-debug-hydra)
    (define-key map (kbd "C-c r") 'gdscript-hydra-show)
    map))

(defvar gdscript-debug--inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "d" 'gdscript-debug-fetch-object-ids-detail)
    (define-key map "l" 'gdscript-debug-go-back)
    (define-key map "n" 'next-line)
    (define-key map "u" 'gdscript-debug-unpin)
    (define-key map "o" 'gdscript-debug-pin-inspected-object-id)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "\t" 'gdscript-debug-toggle-visibility)
    (define-key map "\r" 'gdscript-debug-inspect-object-id)
    (define-key map "?" 'describe-mode)
    (define-key map (kbd "C-c n") 'gdscript-debug-hydra)
    (define-key map (kbd "C-c r") 'gdscript-hydra-show)
    map))

(defvar gdscript-debug--scene-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "g" 'gdscript-debug-request-scene-tree)
    (define-key map "\r" 'gdscript-debug-inspect-object-id)
    (define-key map "?" 'describe-mode)
    (define-key map (kbd "C-c n") 'gdscript-debug-hydra)
    (define-key map (kbd "C-c r") 'gdscript-hydra-show)
    map))

(defvar-local gdscript-debug--buffer-type nil
  "One of the symbols bound in `gdscript-debug--get-buffer-create'.")

(defvar-local gdscript-debug--after-refresh-function nil
  "Function to call after command to popular buffer is received.")

(defun gdscript-debug--get-buffer (buffer-type)
  "Get a specific buffer.

In that buffer, `gdscript-debug--buffer-type' must be equal to BUFFER-TYPE."
  (catch 'found
    (dolist (buffer (buffer-list) nil)
      (with-current-buffer buffer
        (when (eq gdscript-debug--buffer-type buffer-type)
          (throw 'found buffer))))))

(defun gdscript-debug--get-breakpoint-buffer()
  (gdscript-debug--get-buffer-create 'breakpoints-buffer))

(defun gdscript-debug--get-stack-dump-buffer ()
  (gdscript-debug--get-buffer-create 'stack-dump-buffer))

(defun gdscript-debug--get-stack-frame-vars-buffer ()
  (gdscript-debug--get-buffer-create 'stack-frame-vars-buffer))

(defun gdscript-debug--get-inspector-buffer ()
  (gdscript-debug--get-buffer-create 'inspector-buffer))

(defun gdscript-debug--get-scene-tree-buffer ()
  (gdscript-debug--get-buffer-create 'scene-tree-buffer))

(defun gdscript-debug--many-windows (buffer-to-display _action-alist)
  (let ((mode-to-display (with-current-buffer buffer-to-display major-mode))
        (window-to-switch))
    (when
        (or (eq mode-to-display 'gdscript-debug--breakpoints-mode)
            (eq mode-to-display 'gdscript-debug--stack-dump-mode)
            (eq mode-to-display 'gdscript-debug--stack-frame-vars-mode)
            (eq mode-to-display 'gdscript-debug--inspector-mode)
            (eq mode-to-display 'gdscript-debug--scene-tree-mode))
      (mapc (lambda (window)
              (with-current-buffer (window-buffer window)
                (when (not
                       (or (eq major-mode 'gdscript-mode)
                           (eq window lv-wnd)))
                  (setq window-to-switch window)
                  (set-window-buffer window buffer-to-display))))
            (window-list)))
    window-to-switch))

(defun gdscript-debug--stack-dump-file-path (stack-dump)
    (concat (cdr gdscript-debug--stack-dump) (gdscript-debug--drop-res (gdscript-stack-dump->file stack-dump))))

(defun gdscript-debug--set-window-buffer (window stack-dump)
  (let ((buffer (find-file-noselect (gdscript-debug--stack-dump-file-path stack-dump))))
    (set-window-buffer window buffer)
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (1-  (gdscript-stack-dump->line stack-dump)))
      (point))))

(defun gdscript-debug-windows ()
  (delete-other-windows)
  (gdscript-debug-hydra)
  (pcase (car gdscript-debug--stack-dump)
    (`() nil)
    (`(,stack-dump)
     (let* ((top-left (selected-window))
            (bottom-left (split-window))
            (bottom-right (split-window bottom-left nil t)))
       (set-window-buffer top-left (find-file-noselect (gdscript-debug--stack-dump-file-path stack-dump)))
       (set-window-buffer bottom-left (gdscript-debug--get-inspector-buffer))
       (set-window-buffer bottom-right (gdscript-debug--get-stack-frame-vars-buffer))))
    (_
     (let* ((stack-dump-a (caar gdscript-debug--stack-dump))
            (stack-dump-b (cadr (car gdscript-debug--stack-dump)))
            (top-left (selected-window))
            (bottom-left (split-window))
            (top-right (split-window nil nil t))
            (bottom-right (split-window bottom-left nil t))
            (top-left-point (gdscript-debug--set-window-buffer top-left stack-dump-b))
            (top-right-point (gdscript-debug--set-window-buffer top-right stack-dump-a)))
       (set-window-point top-left top-left-point)
       (set-window-point top-right top-right-point)
       (set-window-buffer bottom-left (gdscript-debug--get-inspector-buffer))
       (set-window-buffer bottom-right (gdscript-debug--get-stack-frame-vars-buffer))
       (select-window top-right)))))

(defun gdscript-debug--display-buffer (buffer)
  (display-buffer buffer
                  '((display-buffer-reuse-window
                     gdscript-debug--many-windows))))

(defun gdscript-debug-display-stack-dump-buffer ()
  "Display stack dump."
  (interactive)
  (display-buffer (gdscript-debug--get-stack-dump-buffer) '((display-buffer-same-window))))

(defun gdscript-debug-display-stack-frame-vars-buffer ()
  "Display the variables of current stack."
  (interactive)
  (display-buffer (gdscript-debug--get-stack-frame-vars-buffer) '((display-buffer-same-window))))

(defun gdscript-debug-display-breakpoint-buffer ()
  "Display the breakpoints."
  (interactive)
  (display-buffer (gdscript-debug--get-breakpoint-buffer) '((display-buffer-same-window)))
  (gdscript-debug-refresh-breakpoints-buffer))

(defun gdscript-debug-display-inspector-buffer ()
  "Display the inspector."
  (interactive)
  (gdscript-debug--display-buffer (gdscript-debug--get-inspector-buffer)))

(defun gdscript-debug-display-scene-tree-buffer ()
  "Display the Scene tree."
  (interactive)
  (display-buffer (gdscript-debug--get-scene-tree-buffer) '((display-buffer-same-window))))

(defun gdscript-debug-display-source-buffer ()
  "Using stack dump jump to the source"
  (interactive)
  (with-current-buffer (gdscript-debug--get-stack-dump-buffer)
    (goto-char (point-min))
    (gdscript-debug-jump-to-stack-point)))

(defun gdscript-debug--remove-breakpoint-from-buffer (breakpoint)
  (setq gdscript-debug--breakpoints (remove breakpoint gdscript-debug--breakpoints))
  (gdscript-debug-refresh-breakpoints-buffer))

(defun gdscript-debug--add-breakpoint-to-buffer (breakpoint)
  (unless (member breakpoint gdscript-debug--breakpoints)
    (push breakpoint gdscript-debug--breakpoints)))

(defun gdscript-debug--refresh-scene-tree-buffer (scene-tree-data)
  (with-current-buffer (gdscript-debug--get-scene-tree-buffer)
    (let ((inhibit-read-only t)
          (point (point)))
      (erase-buffer)
      (gdscript-debug--scene-tree-row scene-tree-data 0 "")
      (goto-char point)
      (when gdscript-debug--after-refresh-function
        (funcall gdscript-debug--after-refresh-function)
        (setq gdscript-debug--after-refresh-function nil)))))

(defun gdscript-debug--scene-tree-row (scene-tree-level-edge level node-path)
  (let* ((node (gdscript-scene-tree-level-edge->item scene-tree-level-edge))
         (children (gdscript-scene-tree-level-edge->children scene-tree-level-edge))
         (node-name (gdscript-scene-tree-node->node-name node))
         (node-class (gdscript-scene-tree-node->node-class node))
         (path (concat node-path "/" node-name)))
    (insert (propertize (format "%s %s %s %s\n"
                                (gdscript-debug-pad-string (if children "+" " ") (* 4 level))
                                (gdscript-debug-variable-face node-name)
                                (gdscript-debug-type-face node-class)
                                (gdscript-scene-tree-node->instance-id node))
                        'object-id (gdscript-scene-tree-node->instance-id node)
                        'node-path path))
    (dolist (child children)
      (gdscript-debug--scene-tree-row child (1+ level) path))))

(defun gdscript-debug--refresh-stack-dump-buffer (stack-dump project-root)
  (with-current-buffer (gdscript-debug--get-stack-dump-buffer)
    (let ((inhibit-read-only t)
          (longest-file-name 0))
      (dolist (stack stack-dump)
        (let* ((file (gdscript-stack-dump->file stack))
               (line (gdscript-stack-dump->line stack))
               (len (+ (length file) (length (number-to-string line)))))
          (when (< longest-file-name len)
            (setq longest-file-name len))))
      (erase-buffer)
      (dolist (stack stack-dump)
        (let ((ident (format "%s:%s" (gdscript-stack-dump->file stack) (gdscript-stack-dump->line stack))))
          (insert (propertize
                   (concat
                    (format (concat "%s - %-" (number-to-string (1+ longest-file-name)) "s - ") (gdscript-stack-dump->level stack) ident)
                    (propertize
                     (format "%s\n" (gdscript-stack-dump->function-name stack)) 'font-lock-face font-lock-function-name-face))
                   'gdscript-debug--stack-dump stack
                   'gdscript-debug--project-root project-root)))))))

(defun gdscript-debug-refresh-breakpoints-buffer ()
  (with-current-buffer (gdscript-debug--get-breakpoint-buffer)
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
                                      (gdscript-breakpoint->file breakpoint)
                                      (gdscript-breakpoint->line breakpoint))
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

(define-derived-mode gdscript-debug--scene-tree-mode gdscript-debug--parent-mode "Scene Tree"
  "Major mode for scene tree."
  (setq header-line-format "Scene Tree"))

(defun gdscript-debug--stack-dump-buffer-name ()
  (concat "* Stack dump *"))

(defun gdscript-debug--stack-frame-vars-buffer-name ()
  (concat "* Stack frame vars *"))

(defun gdscript-debug--breakpoints-buffer-name ()
  (concat "* Breakpoints *"))

(defun gdscript-debug--inspector-buffer-name ()
  (concat "* Inspector *"))

(defun gdscript-debug--scene-tree-buffer-name ()
  (concat "* Scene tree *"))

(defvar gdscript-debug--buffer-rules '())
(defvar gdscript-debug--breakpoints '())
(defvar gdscript-debug--skip-breakpoints nil)

(defun gdscript-debug--rules-name-maker (rules-entry)
  (cadr rules-entry))
(defun gdscript-debug--rules-buffer-mode (rules-entry)
  (nth 2 rules-entry))

(defun gdscript-debug--set-buffer-rules (buffer-type &rest rules)
  (if-let* ((binding (assoc buffer-type gdscript-debug--buffer-rules)))
      (setcdr binding rules)
    (push (cons buffer-type rules)
	  gdscript-debug--buffer-rules)))

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

(gdscript-debug--set-buffer-rules
 'scene-tree-buffer
 'gdscript-debug--scene-tree-buffer-name
 'gdscript-debug--scene-tree-mode)

(ignore-errors
  ;; Don't signal an error when hydra.el is not present
  (defhydra gdscript-debug--hydra (:hint none)
    "
_n_ next _c_ continue _m_ step _b_ breakpoints _s_ stack _v_ vars _i_ inspector _t_ scene-tree _d_ details _o_ pin _u_ unpin _q_ quit
"
    ("n" (gdscript-debug-next))
    ("c" (gdscript-debug-continue))
    ("m" (gdscript-debug-step))
    ("s" (gdscript-debug-display-stack-dump-buffer))
    ("b" (gdscript-debug-display-breakpoint-buffer))
    ("v" (gdscript-debug-display-stack-frame-vars-buffer))
    ("i" (gdscript-debug-display-inspector-buffer))
    ("t" (gdscript-debug-request-scene-tree))
    ("d" (gdscript-debug-fetch-object-ids-detail))
    ("o" (gdscript-debug-pin-current-self))
    ("u" (gdscript-debug-unpin))
    ("q" nil)))

(defun gdscript-debug-hydra ()
  "Show debug hydra."
  (interactive)
  (gdscript-util--with-available-hydra (gdscript-debug--hydra/body)))

(provide 'gdscript-debug)
