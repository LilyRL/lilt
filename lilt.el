;;; lilt.el --- Emacs music player -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Lily Lopes
;;
;; Author: Lily Lopes <lily.r.lopes@gmail.com>
;; Maintainer: Lily Lopes <lily.r.lopes@gmail.com>
;; Created: October 27, 2026
;; Modified: October 27, 2026
;; Version: 0.0.1
;; Keywords: multimedia
;; Homepage: https://github.com/lily/lilt
;; Package-Requires: ((emacs "28.1"))
;; Package-Requires: ((libmpdel "20250922.938"))
;; Package-Requires: ((dash "2.20.0"))
;; Package-Requires: ((vui "20261028.1003"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; (progn (add-to-list 'load-path "/tmp/vui.el") (add-to-list 'load-path "/home/lily/repos/lilt/libmpdel/") (load-file "/home/lily/repos/lilt/libmpdel/libmpdel.el") (load-file "/tmp/vui.el/vui.el") (load-file "/home/lily/repos/lilt/lilt.el"))

(require 'libmpdel)
(require 'vui)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup lilt nil "Emacs music player." :group 'multimedia)

(defvar lilt--albums nil)
(defvar lilt--albums-changed-hook nil)
(defvar lilt--request-counter 0)

(defcustom lilt-artist-column-width 24
  "Width of the artist column in the album list."
  :type 'integer
  :group 'lilt)

(cl-defstruct (lilt--song (:constructor lilt--song-create))
  (entity nil :read-only t)
  (name "" :read-only t)
  (file "" :read-only t))

(cl-defstruct (lilt--album-info (:constructor lilt--album-info-create))
  (key "" :read-only t)
  (entity nil :read-only t)
  (name "" :read-only t)
  (artist "" :read-only t)
  (date nil :read-only t)
  (songs nil :read-only t)
  (last-listened 0)
  (expanded nil))

(defun lilt--connect (&optional host port)
  (setq libmpdel-hostname (or host "localhost")
        libmpdel-port (or port 6600))
  (when (libmpdel-connected-p)
    (libmpdel-disconnect))
  (libmpdel-ensure-connection)
  (lilt--load-history)
  (lilt--load-albums))

(defun lilt--load-albums (&optional done)
  (let ((idx (cl-incf lilt--request-counter)))
    (libmpdel-list
     'albums
     (lambda (albums)
       (libmpdel-list-songs
        albums
        (lambda (songs)
          (when (= idx lilt--request-counter) ;; check if this is a request from ages ago
            (lilt--set-albums (lilt--sort-albums (lilt--album-infos-from-songs songs)))
            (message "Loaded albums!")
            (message "%d" (length lilt--albums))
            (when done (funcall done lilt--albums)))))))))

(defun lilt--set-albums (albums)
  (setq lilt--albums albums)
  (run-hooks 'lilt--albums-changed-hook))

(defun lilt--song-from (song)
  (lilt--song-create
   :entity song
   :name (libmpdel-entity-name song)
   :file (libmpdel-song-file song)))

(defun lilt--album-infos-from-songs (songs)
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (song songs)
      (let* ((album (libmpdel-album song))
             (key (lilt--album-key album song)))
        (puthash key (cons song (gethash key groups)) groups)))
    (let (infos)
      (maphash
       (lambda (key songs)
         (let* ((songs (nreverse songs))
                (first (car songs))
                (album (libmpdel-album first)))
           (push (lilt--album-info-create
                  :key key
                  :entity album
                  :name (libmpdel-album-name album)
                  :artist (lilt--album-artist album first)
                  :date (libmpdel-entity-date album)
                  :songs (mapcar #'lilt--song-from songs)
                  :last-listened (gethash key lilt--last-listened 0))
                 infos)))
       groups)
      infos)))

(defun lilt--album-sort-string (album)
  "Formats album into a string that can be used for alphabetic sorting."
  (concat (lilt--album-info-artist album)
          "\0"
          (lilt--album-info-name album)))

(defun lilt--album-less-p (a b)
  (let ((ta (lilt--album-info-last-listened a))
        (tb (lilt--album-info-last-listened b)))
    (cond
     ((> ta tb) t)
     ((< ta tb) nil)
     (t (string-collate-lessp
         (lilt--album-sort-string a)
         (lilt--album-sort-string b)
         nil t)))))

(defun lilt--sort-albums (albums)
  (seq-sort #'lilt--album-less-p albums))

(defun lilt--album-info-update-last-listened (album timestamp)
  (setf (lilt--album-info-last-listened album) timestamp))

(defun lilt--album-info-set-expanded (album open)
  (setf (lilt--album-info-expanded album) open)
  (run-hooks 'lilt--albums-changed-hook))

(defun lilt--get-album-info-from-key (key)
  (seq-find (lambda (album)
              (string= key (lilt--album-info-key album)))
            lilt--albums))

(defun lilt--set-album-last-listened (key timestamp)
  (let ((album (seq-find (lambda (album)
                           (string= key (lilt--album-info-key album)))
                         lilt--albums)))
    (when album
      (lilt--album-info-update-last-listened album timestamp)
      (lilt--set-albums (lilt--sort-albums lilt--albums)))))

(defun lilt--record-listen ()
  (let ((song (libmpdel-current-song)))
    (when song
      (let* ((album (libmpdel-album song))
             (key (lilt--album-key album song))
             (timestamp (float-time)))
        (puthash key timestamp lilt--last-listened)
        (lilt--save-history)
        (lilt--set-album-last-listened key timestamp)))))

(defun lilt--record-current-song ()
  (when (libmpdel-playing-p)
    (lilt--record-listen)))

(add-hook 'libmpdel-current-song-changed-hook #'lilt--record-current-song)

(defun lilt--nonempty (string)
  (and string (not (string-empty-p string)) string))

(defun lilt--album-artist (album song)
  (or (lilt--nonempty (libmpdel-artists-name album))
      (lilt--nonempty (libmpdel-artists-name song))
      ""))

(defun lilt--album-key (album song)
  "Return a stable key for the album containing SONG."
  (let ((artist (lilt--album-artist album song))
        (name (or (libmpdel-album-name album) "")))
    (if (and (not (string-empty-p artist))
             (not (string-empty-p name)))
        (concat (downcase (string-trim artist))
                "\0"
                (downcase (string-trim name)))
      (or (file-name-directory (libmpdel-song-file song)) ""))))

(defun lilt--play-album (album-info)
  (libmpdel-playlist-replace (lilt--album-info-entity album-info) 'current-playlist)
  (libmpdel-play))

(defcustom lilt-history-file
  (locate-user-emacs-file "lilt-history.eld")
  "File where lilt stores album listening history."
  :type 'file)

(defvar lilt--last-listened (make-hash-table :test #'equal)
  "Map album key to the Unix time it was last played.")

(defun lilt--load-history ()
  (setq lilt--last-listened (make-hash-table :test #'equal))
  (when (file-readable-p lilt-history-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents lilt-history-file)
          (goto-char (point-min))
          (dolist (entry (read (current-buffer)))
            (puthash (car entry) (cdr entry) lilt--last-listened)))
      (error nil))))

(defun lilt--save-history ()
  (let (entries)
    (maphash (lambda (key timestamp)
               (push (cons key timestamp) entries))
             lilt--last-listened)
    (with-temp-file lilt-history-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 entries (current-buffer))))))

(defvar lilt--global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "x") #'libmpdel-playback-play-pause)
    map))

(defvar lilt--album-button-keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "o" #'lilt-expand-album-at-point)
    map))

(defun lilt-expand-album-at-point ()
  "Toggle the album at point."
  (interactive)
  (let* ((key (vui-key-at (point)))
         (album-info (and key (lilt--get-album-info-from-key key))))
    (unless album-info
      (user-error "No album at point"))
    (setf (lilt--album-info-expanded album-info)
          (not (lilt--album-info-expanded album-info)))
    (lilt--set-albums lilt--albums)))

(defun lilt--pad-right (string width)
  "Pads using width to correctly handle non-english characters."
  (concat string
          (make-string (max 0 (- width (string-width string))) ?\s)))

(defun lilt--album-label (album-info)
  (--> (lilt--album-info-artist album-info)
       (or (lilt--nonempty it) "Unknown artist")
       (truncate-string-to-width it lilt-artist-column-width nil nil "...")
       (lilt--pad-right it lilt-artist-column-width)
       (concat it "   " (lilt--album-info-name album-info))))

(vui-defcomponent lilt--song-view (song)
  :render
  (vui-text (lilt--song-name song)))

(vui-defcomponent lilt--album-info-view (album-info)
  :render
  (when album-info
    (let ((expanded (lilt--album-info-expanded album-info))
          (key (lilt--album-info-key album-info))
          (label (lilt--album-label album-info)))
      (vui-vstack
       (vui-button label
         :key key
         :no-decoration t
         :face 'default
         :help-echo nil
         :keymap lilt--album-button-keymap
         :on-click (lambda () (lilt--play-album album-info)))
       (when expanded
         (vui-list
          (lilt--album-info-songs album-info)
          (lambda (song)
            (vui-component 'lilt--song-view :song song))
          :vertical t
          :indent 2))))))

(vui-defcomponent lilt--album-list ()
  :state ((albums lilt--albums))
  :render
  (progn
    (vui-use-effect ()
      (let ((handler (vui-with-async-context
                      (vui-set-state :albums lilt--albums))))
        (add-hook 'lilt--albums-changed-hook handler)
        ;; cleanup
        (lambda ()
          (remove-hook 'lilt--albums-changed-hook handler))))
    (setq frame-title-format "Lilt music library")
    (vui-list albums
              (lambda (album-info)
                (vui-component 'lilt--album-info-view :album-info album-info))
              :keymap lilt--global-keymap)))

(defun lilt-run ()
  "Open Lilt, refreshing connection if present."
  (lilt--connect)
  (let ((buffer-name "*lilt*"))
    (vui-mount (vui-component 'lilt--album-list) buffer-name)
    (switch-to-buffer buffer-name)))


(provide 'lilt)
;;; lilt.el ends here
