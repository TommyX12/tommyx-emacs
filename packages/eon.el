;;; eon.el --- An object notation helper for emacs -*- lexical-binding: t -*-

;; Author: TommyX
;; Maintainer: TommyX
;; Version: 0.0.1
;; Package-Requires: (TODO)
;; Homepage: TODO
;; Keywords: TODO


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
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; TODO

;;; Code:

(require 'ht)

(defun eon (&rest args)
  (ht<-plist args))

(defun eon-keys (obj)
  (ht-keys obj))

(defmacro eon-get (obj keys &optional default)
  `(let ((keys (list ,@keys))
         (default ,default)
         (result ,obj)
         (loop t))
     (while (and keys loop)
       (let ((key (car keys)))
         (setq keys (cdr keys))
         (if (ht-p result)
             (setq result
                   (ht-get result key (and (null keys) default)))
           (setq result default
                 loop nil))))
     result))

(defmacro eon-set (obj keys value &optional op default)
  `(let ((keys (list ,@keys))
         (value ,value)
         (op ,op)
         (default ,default)
         (result ,obj))
     (while keys
       (let ((key (car keys)))
         (setq keys (cdr keys))
         (if (ht-p result)
             (if keys
                 (setq result (ht-get result key))
               (ht-set result key
                       (if op
                           (funcall op
                                    (ht-get result (car keys) default)
                                    value)
                         value)))
           (error "Cannot access property %s of %s"
                  (prin1-to-string key)
                  (prin1-to-string result)))))))

(defmacro eon-from-each (item items key value)
  `(let ((result (eon)))
     (dolist (,item ,items)
       (eon-set result (prop) ,value))
     result))

(defun eon-map (obj func)
  (ht-map func obj))

(defun eon-each (obj func)
  (ht-each func obj))

(provide 'eon)

;;; eon.el ends here
