;;; tommyx-music.el --- Extensions to EMMS for playing music -*- lexical-binding: t -*-

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

(require 'emms)
(require 'ivy)
(require 'counsel)
(require 'eon)

(defconst emms-queue-buffer-name " *EMMS Queue*")
(defconst emms-state-props '(emms-queue-active
                             emms-repeat-playlist
                             emms-repeat-track
                             emms-random-playlist
                             emms-single-track
                             emms-player-next-function))
(defvar emms-state-stack nil)
(defvar emms-queue-active nil)

(defun emms-state-push ()
  (interactive)
  (let* ((playlist-buffer-pos
          (save-excursion
            (with-current-emms-playlist
              (emms-playlist-mode-center-current)
              (point))))
         (state
          (eon :playlist-buffer emms-playlist-buffer
               :playlist-buffer-pos playlist-buffer-pos
               :properties
               (eon-from-each prop emms-state-props
                              prop (eval prop)))))
    (push state emms-state-stack)))

(defun emms-state-pop ()
  (interactive)
  (unless emms-state-stack
    (error "No saved EMMS state"))
  (when emms-queue-active
    (kill-buffer emms-playlist-buffer))
  (let ((state (pop emms-state-stack)))
    (emms-playlist-set-playlist-buffer
     (eon-get state (:playlist-buffer)))
    (save-excursion
      (with-current-emms-playlist
        (goto-char
         (eon-get state (:playlist-buffer-pos)))
        (emms-playlist-mode-play-current-track)))
    (eon-each (eon-get state (:properties))
              (lambda (prop value)
                (set prop value)))))

(defun emms-next-in-queue ()
  (if (condition-case nil
          (progn
            (emms-playlist-current-select-next)
            t)
        (error nil))
      (if (funcall emms-ok-track-function
                   (emms-playlist-current-selected-track))
          (emms-start)
        (emms-next-in-queue))
    (emms-state-pop)))

(defun emms-get-create-queue (&optional force-create)
  (if (and (not force-create) emms-queue-active)
      emms-playlist-buffer
    (let ((queue-buffer
           (emms-playlist-new emms-queue-buffer-name))
          (currently-playing emms-player-playing-p))
      (emms-state-push)
      (setq emms-player-next-function #'emms-next-in-queue
            emms-queue-active t
            emms-repeat-playlist nil
            emms-repeat-track nil
            emms-random-playlist nil
            emms-single-track nil)
      (when currently-playing
        (with-current-emms-playlist
          (counsel-emms-enqueue-item-at
           emms-playlist-buffer
           queue-buffer
           emms-playlist-selected-marker)))
      (emms-playlist-set-playlist-buffer queue-buffer)
      (when currently-playing
        (with-current-emms-playlist
          (emms-playlist-select (point-min))))
      queue-buffer)))

(defun emms-echo-no-error (&optional insertp)
  "Describe the current EMMS track in the minibuffer.
If INSERTP is non-nil, insert the description into the current buffer instead.
This function uses `emms-show-format' to format the current track."
  (interactive "P")
  (condition-case nil
      (progn
        (let ((string (format emms-show-format
                              (emms-track-description
                               (emms-playlist-current-selected-track)))))
          (message "%s" string))
        t)
    (error nil)))

(defun emms-next-and-echo ()
  (interactive)
  (emms-next)
  (emms-echo-no-error)
  (sit-for 2))

(defun emms-previous-and-echo ()
  (interactive)
  (emms-previous)
  (emms-echo-no-error)
  (sit-for 2))

(defun emms-seek-backward-more ()
  (interactive)
  (emms-seek -30))

(defun emms-seek-forward-more ()
  (interactive)
  (emms-seek 30))

(defun emms-restart ()
  (interactive)
  (emms-seek-to 0))

(defun emms-set-active-playlist ()
  (interactive)
  (let* ((buf-list (mapcar #'(lambda (buf)
                               (list (buffer-name buf)))
                           (emms-playlist-buffer-list)))
         (selected
          (completing-read "Select active playlist: "
                           buf-list nil t nil
                           'emms-set-active-playlist-history
                           (and emms-playlist-buffer
                                (buffer-name
                                 emms-playlist-buffer)))))
    (emms-playlist-set-playlist-buffer selected)))

(defun counsel-emms-playlist-get-counsel-item (pos)
  (save-excursion
    (when (markerp pos)
      (setq pos (marker-position pos)))
    (goto-char pos)
    (let ((name (file-name-base
                 (buffer-substring-no-properties
                  pos (line-end-position)))))
      (propertize name 'property pos))))

(defun counsel-emms-playlist-get-items ()
  (let (items)
    (save-excursion
      (beginning-of-buffer)
      (while (< (point) (point-max))
        (push (counsel-emms-playlist-get-counsel-item (point))
              items)
        (forward-line)))
    (nreverse items)))

(defun counsel-emms-get-playlist-items (&optional playlist-buffer)
  (if playlist-buffer
      (with-current-buffer playlist-buffer
        (counsel-emms-playlist-get-items))
    (with-current-emms-playlist
      (counsel-emms-playlist-get-items))))

(defun counsel-emms-play-item (item)
  (let ((pos (get-text-property 0 'property item)))
    (with-current-emms-playlist
      (save-excursion
        (goto-char pos)
        (emms-playlist-mode-play-current-track)))))

(defun counsel-emms-enqueue-item-at (from-buffer to-buffer pos)
  (let (track)
    (with-current-buffer from-buffer
      (save-excursion
        (goto-char pos)
        (setq track (buffer-substring
                     (line-beginning-position)
                     (1+ (line-end-position))))))
    (with-current-buffer to-buffer
      (emms-with-inhibit-read-only-t
       (save-excursion
         (goto-char (point-max))
         (insert track))))
    (unless emms-player-playing-p
      (with-current-emms-playlist
        (unless emms-playlist-selected-marker
          (emms-playlist-select (point-min)))
        (emms-start)))))

(defun counsel-emms-enqueue-item (from-buffer to-buffer item)
  (let ((pos (get-text-property 0 'property item)))
    (counsel-emms-enqueue-item-at
     from-buffer to-buffer pos)))

(defun counsel-emms-get-current-track ()
  (with-current-emms-playlist
    (when emms-playlist-selected-marker
      (counsel-emms-playlist-get-counsel-item
       emms-playlist-selected-marker))))

(defun counsel-emms-play ()
  (interactive)
  (ivy-read "Play track: "
            (counsel-emms-get-playlist-items)
            :history 'counsel-emms-play-history
            :action #'counsel-emms-play-item
            :preselect (counsel-emms-get-current-track)
            :require-match t))

(defun counsel-emms-enqueue ()
  (interactive)
  (unless emms-playlist-buffer
    (error "No active playlist buffer"))
  (let* ((tracks-buffer (if emms-queue-active
                            (eon-get (car emms-state-stack)
                                     (:playlist-buffer))
                          emms-playlist-buffer))
         (tracks (counsel-emms-get-playlist-items
                  tracks-buffer)))
    (ivy-read "Enqueue track: " tracks
              :history 'counsel-emms-enqueue-history
              :action (lambda (item)
                        (message "Enqueued: %s" item)
                        (counsel-emms-enqueue-item
                         tracks-buffer (emms-get-create-queue) item))
              :preselect (counsel-emms-get-current-track)
              :require-match t)))

(defun emms-show-progress (&rest _)
  (let* ((total-playing-time (emms-track-get
                              (emms-playlist-current-selected-track)
                              'info-playing-time))
         (playing-time emms-playing-time)
         (elapsed/total (/ (* 100 emms-playing-time) total-playing-time)))
    (with-temp-message (format "[%-100s] [%02d:%02d/%02d:%02d] %2d%%"
                               (make-string elapsed/total ?=)
                               (/ playing-time 60)
                               (% playing-time 60)
                               (/ total-playing-time 60)
                               (% total-playing-time 60)
                               elapsed/total)
      (sit-for 2))))

(defun emms-setup-show-progress-on-seek ()
  (add-hook 'emms-player-seeked-functions #'emms-show-progress 'append))

(provide 'tommyx-music)

;;; tommyx-music.el ends here
