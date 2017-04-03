;;; -*- mode: Lisp; Base: 10 ; Syntax: ANSI-Common-Lisp ; buffer-read-only: t; -*-
;;; This is ASDF 3.2.1: Another System Definition Facility.
;;;
;;; Feedback, bug reports, and patches are all welcome:
;;; please mail to <asdf-devel@common-lisp.net>.
;;; Note first that the canonical source for ASDF is presently
;;; <URL:http://common-lisp.net/project/asdf/>.
;;;
;;; If you obtained this copy from anywhere else, and you experience
;;; trouble using it, or find bugs, you may want to check at the
;;; location above for a more recent version (and for documentation
;;; and test files, if your copy came without them) before reporting
;;; bugs.  There are usually two "supported" revisions - the git master
;;; branch is the latest development version, whereas the git release
;;; branch may be slightly older but is considered `stable'

;;; -- LICENSE START
;;; (This is the MIT / X Consortium license as taken from
;;;  http://www.opensource.org/licenses/mit-license.html on or about
;;;  Monday; July 13, 2009)
;;;
;;; Copyright (c) 2001-2016 Daniel Barlow and contributors
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; -- LICENSE END

;;; The problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file.

