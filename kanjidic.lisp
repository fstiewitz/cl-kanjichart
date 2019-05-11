(ql:quickload :S-XML)
(defpackage :PW.STIEWITZ.KANJICHART.KANJIDIC
  (:use :COMMON-LISP :S-XML)
  (:export
   :read-kanjidic2.xml
   :intersect
   :*kanji*
   :kanji-literal
   :kanji-grade
   :kanji-strokes
   :kanji-frequency
   :kanji-jlpt
   :kanji-meanings
   :kanji-readings
   :kanji-nanori))
(in-package :PW.STIEWITZ.KANJICHART.KANJIDIC)
(defclass kanji ()
  ((literal :accessor kanji-literal :initarg :literal :initform nil)
   (grade :accessor kanji-grade :initarg :grade :initform 11)
   (strokes :accessor kanji-strokes :initarg :strokes :initform nil)
   (frequency :accessor kanji-frequency :initarg :frequency :initform nil)
   (jltp :accessor kanji-jlpt :initarg :jlpt :initform nil)
   (meanings :accessor kanji-meanings :initarg :meanings :initform nil)
   (readings :accessor kanji-readings :initarg :readings :initform nil)
   (nanori :accessor kanji-nanori :initarg :nanori :initform nil)))
(defvar *kanji* nil
  "List of all kanji")
(defun intersect (l)
  (loop for kanji in *kanji*
        when (find (kanji-literal kanji) l)
        collect kanji))
(defun add-kanji (kanji)
  (push kanji *kanji*))
(defun kanjidic2.xml-new-element-hook (name attributes seed)
  (declare (ignore attributes))
  (cond
    ((string= name "character") (make-instance 'kanji))
    (t seed)))
(defvar *last-text*)
(defun kanjidic2.xml-finish-element-hook (name attributes parent-seed seed)
  (if (string= name "character")
      (push seed *kanji*)
      (cond
        ((string= name "literal") (setf (kanji-literal parent-seed) (elt *last-text* 0)))
        ((string= name "grade") (setf (kanji-grade parent-seed) (or (parse-integer *last-text*) 11)))
        ((string= name "stroke_count") (setf (kanji-strokes parent-seed) (parse-integer *last-text*)))
        ((string= name "freq") (setf (kanji-frequency parent-seed) *last-text*))
        ((string= name "jltp") (setf (kanji-jlpt parent-seed) *last-text*))
        ((string= name "meaning") (if (= 0 (length attributes)) (push *last-text* (kanji-meanings parent-seed))))
        ((string= name "reading") (when (and (= 1 (length attributes))
                                             (string= (caar attributes) "r_type")
                                             (or (string= (cdar attributes) "ja_on") (string= (cdar attributes) "ja_kun")))
                                      (push *last-text* (kanji-readings parent-seed))))
        ((string= name "nanori") (push *last-text* (kanji-meanings parent-seed)))))
  parent-seed)
(defun kanjidic2.xml-text-hook (string seed)
  (setf *last-text* string)
  seed)
(defun read-kanjidic2.xml (path)
  (setf *kanji* nil)
  (with-open-file (file path)
    (start-parse-xml file (make-instance 'xml-parser-state
                                         :seed nil
                                         :new-element-hook 'kanjidic2.xml-new-element-hook
                                         :finish-element-hook 'kanjidic2.xml-finish-element-hook
                                         :text-hook 'kanjidic2.xml-text-hook))
    (setf *kanji* (sort *kanji* (lambda (a b) (if (= (kanji-grade a) (kanji-grade b))
                                                  (< (kanji-strokes a) (kanji-strokes b))
                                                  (< (kanji-grade a) (kanji-grade b))))))
    nil))
