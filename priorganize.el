;;; priorganize.el --- Task priority queues

;; Author: Cameron Kingsbury <camsbury7@gmail.com>
;; URL: https://github.com/Camsbury/priorganize.el
;; Created: Augest 18, 2019
;; Keywords: org, todo
;; Package-Requires: ((dash "2.16.0") (dash-functional "1.2.0") (emacs "24") (uuid "0.0.3"))
;; Version: 0.1

;; Copyright (c) 2019 Cameron Kingsbury

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'uuid)
(require 'dash)
(require 'dash-functional)


;;; Init

;; the path for the SQLite database
(setq-default priorg-db-path "~/priorg.db")

;;; Helpers

(defun priorg-db-execute (db query)
    (shell-command
     (format "sqlite3 %s \"%s\"" priorg-db-path query)))


(defun priorg-db-fetch (db query)
  (let* ((raw-output (shell-command-to-string
                     (format "sqlite3 -header %s \"%s\"" priorg-db-path query)))
         (output (split-string raw-output "\n"))
         (column-names (split-string (car output) "|"))
         (rows (--map (split-string it "|") (cdr output))))
    `((column-names ,column-names) (rows ,rows))))

;;; Major Modes

;; Main Mode
(define-derived-mode priorg-mode tabulated-list-mode "Priorganize"
  "Priorganize Mode"
  (let ((columns [("Options" 80)])
        (rows (list '(:queues ["Queues"]))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defvar priorg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'priorg-queue-mode)))

;; Queue Mode
(defconst priorg-queue-list-sql
  "SELECT id, name, description FROM queues;")
(define-derived-mode priorg-queue-mode tabulated-list-mode "Priorganize Queues"
  "Priorganize Queue Mode"
  (let* ((table (priorg-db-fetch priorg-db-path priorg-queue-list-sql))
         (rows (--map (list (car it) (apply 'vector (cdr it))) ;grab id
                      (cadr (assoc 'rows table))))
         (tl-format [("Name" 20) ("Description" 60)])
         (tl-rows (mapcar (lambda (x) (list (car x) (cadr x))) rows)))
    (switch-to-buffer "*priorganize-queues*")
    (setq tabulated-list-format tl-format)
    (setq tabulated-list-entries tl-rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defvar priorg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'priorg-queue-add)))

;;; Entry Point

(defun priorganize ()
  (interactive)
  (switch-to-buffer "*priorganize-main*")
  (priorg-mode))

;;; Commands

(defun priorg-migrate ()
  "Creates the tables for priorganize.el in the provided db"
  (interactive)
  (shell-command
   (format "sqlite3 %s < %smigrate.sql"
           priorg-db-path
           (file-name-directory buffer-file-name))))

(defun priorg-queue-add (name desc)
  "Adds a new queue to prioritize items in"
  (interactive "sQueue name: \nsDescription: ")
  (let ((q-id (uuid-to-stringy (uuid-create))))
    (db-execute db (format
      "INSERT INTO queues \
       VALUES ('%s', '%s', '%s')"
      q-id name desc))))


(provide 'priorganize)




;;; Helpful snippets

(defmacro comment (&rest _body)
  "Comment out one or more s-expressions"
  nil)

(comment


  (apply 'vector (--map `(,it 50 t) (cadr (assoc 'column-names (db-fetch priorg-db-path "SELECT name, description FROM queues;")))))
  (mapcar (lambda (x) `(nil ,x)) (--map (apply 'vector it) (cadr (assoc 'rows (db-fetch priorg-db-path "SELECT name, description FROM queues;")))))

  (mapcar (lambda (x) `(nil [,x])) '("frontend-6f567b7966-6pgzs" "hello-node-7f5b6bd6b8-48kk4" "redis-64896b74dc-zrw7w" ""))
  )


