;;;
;;; pdf.scm -- Public interface for PDF package, including exported macros
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


(module pdf

  ( set-page-stream
    set-font
    move-to-next-line
    draw-text
    move-text
    draw-text-on-next-line
    set-text-rendering-mode
    set-char-spacing
    set-text-x-scale
    set-text-leading
    set-text-rise
    set-text-matrix
    draw-and-adjust-string
    escape
    rotate
    translate
    scale
    set-line-width
    set-line-cap
    set-line-join
    set-dash-pattern
    set-mitter-limit
    move-to
    line-to
    bezier-to
    bezier2-to
    bezier3-to
    close-path
    basic-rect
    stroke
    close-and-stroke
    fill-path
    close-and-fill
    even-odd-fill
    fill-and-stroke
    even-odd-fill-and-stroke
    close-fill-and-stroke
    close-even-odd-fill-and-stroke
    end-path-no-op
    clip-path
    even-odd-clip-path
    set-gray-stroke
    set-gray-fill
    set-rgb-stroke
    set-rgb-fill
    set-cymk-stroke
    set-cymk-fill
    +2pi+
    +pi/2+
    arc
    pie
    circle
    ellipse
    rectangle
    polyline
    regular-polygon
    star
    ;; defined in pdf-base
    build-font
    write-document
    font-name
    unit-size
    page-width
    page-height

    (with-document
     reset-parameters
     *document*
     build-doc)

    (with-document-to-file
     reset-parameters *document* build-doc write-document)

    (in-text-mode *page-stream*)

    (with-page
     *page-width* *page-height* *page* add-page
     build-pdf-stream build-indirect-obj build-page
     set-page-stream
     *default-width* *default-height*)
    )

(cond-expand
  (chicken-5 (import (chicken base) (chicken file posix) format regex scheme srfi-1))
  (else (begin (import scheme chicken)
               (use srfi-1 regex posix format))))


(define-syntax with-document
  (syntax-rules ()
    ((_ body ...)
     (begin
       (reset-parameters)
       (*document* (build-doc))
       body ...))))

(define-syntax with-document-to-file
  (syntax-rules ()
    ((_ filename body ...)
     (begin
       (reset-parameters)
       (*document* (build-doc))
       body ...
       (write-document filename)))))

(define-syntax pdf-with-output-to-string
  (syntax-rules ()
    ((_ body ...)
     (let ((s-port (open-output-string)))
       (set-page-stream s-port)
         body ...
       (get-output-string s-port)))))

(define-syntax with-page
  (syntax-rules ()
    ((_ (width height) body ...)
     (begin
       (*page-width* width)
       (*page-height* height)
       (let* ((pdf-stream
               (build-pdf-stream
                (pdf-with-output-to-string body ...)))
              (content (build-indirect-obj pdf-stream))
              (page (build-page width height content)))
         (*page* page)
         (add-page (*page*)))))
    ((_ body ...) ; default media box size
     (with-page (*default-width* *default-height*) body ...))))


;; was in pdf-graphics
(define-syntax in-text-mode
  (syntax-rules ()
    [(_ arg ...)
     (begin
       (format (*page-stream*) "BT~%")
       arg ...
       (format (*page-stream*) "ET~%"))]))

(include "pdf-base-impl.scm")
(include "pdf-graphics-impl.scm")

)
