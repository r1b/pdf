;;;
;;; pdf-base.scm -- PDF document generation
;;;
;;; Copy of the original licence from Marc Battyani:
;;;
;;;
;;;  cl-pdf is a Common Lisp library for generating PDF files.
;;;
;;;  It is distributed under a FreeBSD style license
;;;  (if you want another license contact me) marc.battyani@fractalconcept.com
;;;
;;;  Copyright (c) 2002 Marc Battyani. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without modification, are
;;;  permitted provided that the following conditions are met:
;;;
;;;  Redistributions of source code must retain the above copyright notice, this list of
;;;  conditions and the following disclaimer.
;;;
;;;  Redistributions in binary form must reproduce the above copyright notice, this list of
;;;  conditions and the following disclaimer in the documentation and/or other materials 
;;;  provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE MARC BATTYANI ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;;  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
;;;  AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MARC BATTYANI OR
;;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;;;  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;;  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;  The latest version is at http://www.fractalconcept.com/asp/html/cl-pdf.html
;;;  You can contact me at marc.battyani@fractalconcept.com or marc@battyani.net

;;;
;;; Author: Bruce Butterfield <bab@entricom.com>
;;;
;;; Commentary:
;;;
;;; The port from Common Lisp was done as "Scheme-ishly" as possible; most of the changes 
;;; from the original code involved mapping CLOS objects to structures and associated
;;; functions. I would have used the PLT class library but I wanted to be able to use this
;;; code in other Scheme implementations; structures/records are a bit more universal.
;;;
;;; Ported to Chicken Scheme by Matt Gushee <matt@gushee.net>


(define fmt format)


;; structure defs
(define-record doc catalog root-page pages xref objects fonts)
(define-record indirect-obj obj-number gen-number content name)
(define-record dictionary values)
(define-record pdf-stream content)

;; constants
(define *unit-size* 72)  ; default 72 points per inch
(define *default-width* 612)  ; in units, 8.5x11
(define *default-height* 792) ; in units, 8.5x11

;; handy parameters

(define *output* (make-parameter #f))
(define *document* (make-parameter #f))  
(define *page* (make-parameter #f))
(define *next-obj-number* (make-parameter 0))
(define *next-var-number* (make-parameter 100))
(define *page-width* (make-parameter *default-width*))
(define *page-height* (make-parameter *default-height*))

(define (reset-parameters)
  (*output* #f)
  (*document* #f)
  (*page* #f)
  (*next-obj-number* 0)
  (*next-var-number* 100)
  (*page-height* *default-height*)
  (*page-width* *default-width*))
  

;; structure builder funcs

(define-syntax enforce-/
  (syntax-rules ()
    ((_ arg)
     (unless (char=? (string-ref arg 0) #\/)
       (set! arg (string-append "/" arg))))
    ((_ arg1 arg2 ...)
     (begin
       (enforce-/ arg1)
       (enforce-/ arg2 ...)))))

(define (build-indirect-obj content)
  (let ((obj (make-indirect-obj (get-next-obj-number) 0 content "indirect-obj")))
    (if (*document*)
        (doc-objects-set! (*document*) (cons obj (doc-objects (*document*)))))
    obj))

(define (build-dictionary values)
  (let ((obj (make-dictionary values)))
    obj))

(define (build-pdf-stream content)
  (let ((obj (make-pdf-stream content)))
    obj))

(define (build-font base-font)
  (enforce-/ base-font)
  (let ((obj (build-indirect-obj
              (build-dictionary `(("/Type" . "/Font")
                                  ("/Subtype" . "/Type1")
                                  ("/BaseFont" . ,base-font)
                                  ("/Encoding" . "/WinAnsiEncoding"))))))
    (indirect-obj-name-set! obj (gen-name "/CLF"))
    (doc-fonts-set! (*document*) (cons obj (doc-fonts (*document*))))
    obj))

(define (build-page width height content)
  (let* ((root-page (doc-root-page (*document*)))
         (res-obj (build-dictionary `(("/Xobject" . ,(build-dictionary '()))
                                      ("/Font" . ,(lambda () (get-document-font-refs))))))
         (obj (build-indirect-obj
               (build-dictionary `(("/Type" . "/Page")
                                   ("/Parent" . ,(lambda () (get-obj-ref root-page)))
                                   ("/MediaBox" . #(0 0 ,width ,height))
                                   ("/Resources" . ,res-obj)
                                   ("/Contents" . ,(lambda () (get-obj-ref content))))))))
    obj))

(define (build-doc)
  (let* ((root-page (build-indirect-obj 
                     (build-dictionary `(("/Type" . "/Pages")
                                         ("/Count" . ,(lambda () (page-count)))
                                         ("/Kids" . ,(lambda () (page-refs)))))))
         (catalog (build-indirect-obj
                   (build-dictionary `(("/Type" . "/Catalog")
                                       ("/Pages" . ,(lambda () (get-obj-ref root-page))))))))
    (let ((obj (make-doc catalog root-page '() '((0 65535 f)) '() '())))
      obj)))


;; writers

(define (write-obj obj)
  (cond ((indirect-obj? obj)
         (write-indirect-obj obj))
        ((dictionary? obj)
         (write-dictionary obj))
        ((pdf-stream? obj)
         (write-pdf-stream obj))
        ((procedure? obj)
         (write-obj (obj)))
        ((vector? obj)
         (fmt (*output*) "[ ")
         (for-each
          (lambda (x)
            (write-obj x))
          (vector->list obj))
         (fmt (*output*) "] "))
        (else
         (fmt (*output*) "~a " obj))))

(define (write-dictionary obj)
  (fmt (*output*) "<< ")
  (for-each
   (lambda (x)
     (write-obj (car x))
     (write-obj (cdr x))
     (fmt (*output*) "~%"))
   (dictionary-values obj))
  (fmt (*output*) ">> "))

(define (write-indirect-obj obj)
  (let ((offset (file-position (*output*))))
    (doc-xref-set! (*document*) (cons (list offset 0 'n) (doc-xref (*document*))))
    (fmt (*output*) "~d ~d obj~%" (indirect-obj-obj-number obj) (indirect-obj-gen-number obj))
    (write-obj (indirect-obj-content obj))
    (fmt (*output*) "~%endobj~%")))

(define (write-pdf-stream obj)
  (let ((content (pdf-stream-content obj)))
    (fmt (*output*) "<< /Length ~d~%>>~%stream~%~a~%endstream~%"
         (string-length content)
         content)))

(define write-document
  (lambda (file)
    ;(*output* (open-output-file file 'truncate)) 
    (*output* (open-output-file file)) ; Same effect??
    (fmt (*output*) "%PDF-1.3~%")
    (write-obj (doc-root-page (*document*)))
    (write-obj (doc-catalog (*document*)))
    (for-each
     (lambda (x)
       (write-obj x))
     (reverse (doc-objects (*document*))))
    (let ((xref-offset (file-position (*output*))))
      (fmt (*output*) "xref~%~d ~d~%" 0 (length (doc-xref (*document*))))
      (for-each
       (lambda (x)
         (fmt (*output*) "~10,'0d ~5,'0d ~a ~%" (car x) (cadr x) (caddr x)))
       (reverse (doc-xref (*document*))))
      (fmt (*output*) "trailer ~%<< /Size ~d /Root ~a~%>>~%" 
           (+ (length (doc-objects (*document*))) 1)
           (get-obj-ref (doc-catalog (*document*))))
      (fmt (*output*) "startxref~%~d~%%%EOF~%" xref-offset))
    (close-output-port (*output*))))


;; utilities

(define (add-page page)
  (doc-pages-set! (*document*) (cons page (doc-pages (*document*)))))

(define (page-count)
  (if (*document*)
      (length (doc-pages (*document*)))
      0))

(define (page-refs)
  (if (*document*)
      (list->vector (map get-obj-ref (reverse (doc-pages (*document*)))))
      (list->vector '())))

(define (add-dictionary-item dict name value)
  (dictionary-values-set! dict (cons (cons name value) (dictionary-values dict))))

(define (get-dictionary-value dict name)
  (cdr (assoc name (dictionary-values dict))))

(define (set-dictionary-value dict name value)
  (set-cdr! (assoc name (dictionary-values dict)) value))

(define (get-obj-ref obj)
  (cond ((indirect-obj? obj)
         (fmt #f "~d ~d R" 
              (indirect-obj-obj-number obj)
              (indirect-obj-gen-number obj)))
        ((procedure? obj)
         (get-obj-ref (obj)))
        (else
         (error 'get-obj-ref "~s not an indirect-obj" obj))))

(define (get-font-ref obj)
  (if (indirect-obj? obj)
      (build-dictionary `((,(indirect-obj-name obj) . ,(get-obj-ref obj))))
      (error 'get-font-ref "~s not an indirect-obj" obj)))

(define (gen-name prefix)
  (*next-var-number* (+ 1 (*next-var-number*)))
  (fmt #f "~a~d" prefix (*next-var-number*)))

(define (get-document-font-refs)
  (let ((fonts (doc-fonts (*document*)))
        (dict (build-dictionary '())))
    (for-each
     (lambda (x)
       (let ((font-ref (get-font-ref x)))
         (add-dictionary-item dict 
                              (caar (dictionary-values font-ref)) 
                              (cdar (dictionary-values font-ref)))))
     fonts)
    dict))

(define (font-name font)
  (indirect-obj-name font))

(define (get-next-obj-number)
  (*next-obj-number* (+ 1 (*next-obj-number*)))
  (*next-obj-number*))

(define (page-height)
  (*page-height*))

(define (page-width)
  (*page-width*))

(define (unit-size)
  *unit-size*)
