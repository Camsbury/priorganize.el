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

(defconst priorg-global-queue-id
  "d3f8e89c-8994-5d24-ff33-5610abfaf294")

(setq priorg-current-queue
  priorg-global-queue-id)

(defun priorg-db-execute (db query)
    (shell-command
     (format "sqlite3 %s \"%s\"" priorg-db-path query)))

(defun priorg-db-fetch (db query)
  (let* ((raw-output (shell-command-to-string
                     (format "sqlite3 -header %s \"%s\"" priorg-db-path query)))
         (output (split-string raw-output "\n"))
         (column-names (split-string (car output) "|"))
         (rows (delete '("") (--map (split-string it "|") (cdr output)))))
    `((column-names ,column-names) (rows ,rows))))

;;; Entry Point

(defun priorganize (db)
  "Start priorganizing"
  (interactive "sDatabase Name (default ~/priorg.db): ")
  (unless (eq db "")
      (setq priorg-db-path db)
    (setq priorg-db-path "~/priorg.db"))
  (unless (file-exists-p priorg-db-path)
    (priorg-migrate))
  (switch-to-buffer "*priorganize-main*")
  (priorg-mode))

;;; Major Modes

;; Main Mode
(define-derived-mode priorg-mode tabulated-list-mode "Priorganize"
  "Priorganize Mode"
  (let ((columns [("Options" 80)])
        (rows (list '(:queues ["Queues"])
                    '(:items  ["Items"]))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (read-only-mode)))

(defvar priorg-mode-map nil "Keymap for `priorg-mode'")
(progn
  (setq priorg-mode-map (make-sparse-keymap))
  (define-key priorg-mode-map (kbd "C-c RET") 'priorg-dispatch)
  (define-key priorg-mode-map (kbd "C-c q") 'kill-this-buffer))

(defun priorg-dispatch ()
  "dispatch from priorganize menu"
  (interactive)
  (let ((cmd (tabulated-list-get-id)))
    (cond ((eq :queues cmd) (priorg-queue-list))
          ((eq :items  cmd) (priorg-item-list)))))

;; Queue Mode
(defun priorg-queue-list ()
  "List queues"
  (interactive)
  (switch-to-buffer "*priorganize-queues*")
  (priorg-queue-mode))

(defconst priorg-queue-list-sql
  "SELECT id, name, description \
   FROM queues \
   ORDER BY name;")

(define-derived-mode priorg-queue-mode tabulated-list-mode "Priorganize Queues"
  "Priorganize Queue Mode"
  (let* ((table (priorg-db-fetch priorg-db-path priorg-queue-list-sql))
         (rows (--map (list (car it) (apply 'vector (cdr it))) ;grab id
                      (cadr (assoc 'rows table))))
         (tl-format [("Name" 20) ("Description" 60)])
         (tl-rows (mapcar (lambda (x) (list (car x) (cadr x))) rows)))
    (setq tabulated-list-format tl-format)
    (setq tabulated-list-entries tl-rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (read-only-mode)))

(defvar priorg-queue-mode-map nil "keymap for `priorg-queue-mode'")
(progn
  (setq priorg-queue-mode-map (make-sparse-keymap))
  (define-key priorg-queue-mode-map (kbd "C-c i") 'priorg-item-list)
  (define-key priorg-queue-mode-map (kbd "C-c n") 'priorg-queue-add)
  (define-key priorg-queue-mode-map (kbd "C-c q") 'kill-this-buffer))

;; Item Mode
(defun priorg-item-list ()
  "List items"
  (interactive)
  (let* ((table-id (tabulated-list-get-id))
         (queue-id (if (eq :items table-id)
                    priorg-global-queue-id
                  table-id)))
    (setq priorg-current-queue queue-id)
    (priorg--item-list)))

(defun priorg--item-list ()
  "Switch to list view without updating active queue"
  (switch-to-buffer "*priorganize-items*")
  (priorg-item-mode))

(defconst priorg-item-list-sql
  "SELECT i.id, i.name, i.description \
   FROM items i \
   INNER JOIN items_queues iq \
   ON i.id = iq.item_id
   WHERE iq.queue_id = '%s'
   ORDER BY name")

(define-derived-mode priorg-item-mode tabulated-list-mode "Priorganize Items"
  "Priorganize Item Mode"
  (let* ((table (priorg-db-fetch priorg-db-path
                                 (format priorg-item-list-sql
                                         priorg-current-queue)))
         (rows (--map (list (car it) (apply 'vector (cdr it))) ;grab id
                      (cadr (assoc 'rows table))))
         (tl-format [("Name" 20) ("Description" 60)])
         (tl-rows (mapcar (lambda (x) (list (car x) (cadr x))) rows)))
    (setq tabulated-list-format tl-format)
    (setq tabulated-list-entries tl-rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (read-only-mode)))

(defvar priorg-item-mode-map nil "keymap for `priorg-item-mode'")
(progn
  (setq priorg-item-mode-map (make-sparse-keymap))
  (define-key priorg-item-mode-map (kbd "C-c n") 'priorg-item-add)
  (define-key priorg-item-mode-map (kbd "C-c q") 'kill-this-buffer))

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
    (priorg-db-execute priorg-db-path (format
      "INSERT INTO queues \
       VALUES ('%s', '%s', '%s');"
      q-id name desc)))
  (priorg-queue-list))

(defun priorg-item-add (name desc)
  "Adds a new item to prioritize"
  (interactive "sItem name: \nsDescription: ")
  (let ((q-id (uuid-to-stringy (uuid-create))))
    (priorg-db-execute priorg-db-path (format
      "INSERT INTO items \
       VALUES ('%s', '%s', '%s');\
       INSERT INTO items_queues (item_id, queue_id) \
       VALUES ('%s', '%s');"
      q-id name desc q-id priorg-current-queue))
  (priorg--item-list)))


(provide 'priorganize)




;;; Helpful snippets

(defmacro comment (&rest _body)
  "Comment out one or more s-expressions"
  nil)

(comment

  (delete-file "~/priorg.db")
  (apply 'vector (--map `(,it 50 t) (cadr (assoc 'column-names (db-fetch priorg-db-path "SELECT name, description FROM queues;")))))
  (mapcar (lambda (x) `(nil ,x)) (--map (apply 'vector it) (cadr (assoc 'rows (db-fetch priorg-db-path "SELECT name, description FROM queues;")))))

  (mapcar (lambda (x) `(nil [,x])) '("frontend-6f567b7966-6pgzs" "hello-node-7f5b6bd6b8-48kk4" "redis-64896b74dc-zrw7w" ""))
  )


