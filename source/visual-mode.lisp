;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/visual-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:nyxt/web-mode #:query-hints #:get-nyxt-id)
  (:documentation "Visual mode."))
(in-package :nyxt/visual-mode)

(define-mode visual-mode ()
  "Visual mode. For documentation on commands and keybindings, see the manual."
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "visual"
      scheme:cua
      (list
       "up" 'backward-line
       "down" 'forward-line
       "left" 'backward-char
       "right" 'forward-char
       "escape" 'visual-mode
       "delete" 'clear-selection
       "keypadend" 'end-line
       "space" 'forward-char
       "backspace" 'backward-char
       "keypadhome" 'beginning-line
       "shift-down" 'forward-line-with-selection
       "shift-up" 'backward-line-with-selection
       "shift-left" 'backward-char-with-selection
       "shift-right" 'forward-char-with-selection
       "C-shift-left" 'backward-word-with-selection
       "C-shift-right" 'forward-word-with-selection
       "C-shift-up" 'beginning-line-with-selection
       "C-shift-down" 'end-line-with-selection
       "C-c" 'visual-mode)
      scheme:emacs
      (list
       "C-h" 'select-paragraph
       "shift-space" 'toggle-mark
       "C-space" 'toggle-mark
       "C-g" 'visual-mode
       "C-f" 'forward-char
       "C-b" 'backward-char
       "M-f" 'forward-word
       "M-b" 'backward-word
       "C-n" 'forward-line
       "C-p" 'backward-line
       "C-a" 'beginning-line
       "C-e" 'end-line
       "M-a" 'backward-sentence
       "M-e" 'forward-sentence)
      ;; vi keybindings only enable use of vim's plain "visual" mode for now
      scheme:vi-normal
      (list
       "h" 'backward-char
       "j" 'forward-line
       "k" 'backward-line
       "l" 'forward-char
       "w" 'forward-word
       "b" 'backward-word
       "$" 'end-line
       "0" 'beginning-line
       "v" 'toggle-mark
       "C-c" 'visual-mode)))
   (mark-set nil)
   (destructor
    (lambda (mode)
      (make-page-uneditable)
      (unlock-page-keypresses)
      (setf (mark-set mode) nil)))
   (constructor
    (lambda (mode)
      (make-page-editable)
      (block-page-keypresses)
      (select-paragraph)
      ;; imitating visual mode in vim
      (if (equal (keymap-scheme-name (buffer mode)) scheme:vi-normal)
          (setf (mark-set mode) t))))))

(defmethod prompter:object-attributes ((element nyxt/dom:text-element))
  `(("Hint" ,(plump:get-attribute element "nyxt-hint"))
    ("Text" ,(plump:text element))))

(define-parenscript set-caret-on-start (&key nyxt-identifier)
  (let ((el (nyxt/ps:qs-nyxt-id document nyxt-identifier))
        (range (ps:chain document (create-range)))
        (sel (ps:chain window (get-selection))))
    (ps:chain window (focus))
    (ps:chain range (set-start (ps:@ el child-nodes 0) 0))
    (ps:chain range (collapse true))
    (ps:chain sel (remove-all-ranges))
    (ps:chain sel (add-range range))))

(defmethod %follow-hint ((element nyxt/dom:text-element))
  (set-caret-on-start :nyxt-identifier (get-nyxt-id element)))

(define-parenscript block-page-keypresses ()
  (setf (ps:@ window block-keypresses)
        (lambda (event)
          (ps:chain event (prevent-default))))
  (ps:chain window
            (add-event-listener "keydown"
                                (ps:@ window block-keypresses)
                                false)))

(define-parenscript unlock-page-keypresses ()
  (ps:chain window
            (remove-event-listener "keydown"
                                   (ps:@ window block-keypresses)
                                   false)))

(define-parenscript make-page-editable ()
  (setf (ps:@ document body content-editable) "true"))

(define-parenscript make-page-uneditable ()
  (setf (ps:@ document body content-editable) "false"))

(define-command select-paragraph ()
  "Add hints to text elements on the page and query them."
  (query-hints "Set caret on element"
               (lambda (results) (%follow-hint (first results)))
               :selector "a, b, p, del, h1, h2, h3, h4, h5, h6, i, option,
strong, sub, sup, listing, xmp, plaintext, basefont, big, blink, center, font,
marquee, multicol, nobr, s, spacer, strike, tt, u, wbr, code, cite, pre"))

(define-parenscript is-collapsed ()
  ;; returns "true" if mark's start and end are the same value
  (defun is-collapsed ()
    (let ((sel (ps:chain window (get-selection))))
      (ps:@ sel is-collapsed)))
  (is-collapsed))

(define-parenscript collapse-to-focus ()
  "Collapse the selection"
  (let ((sel (ps:chain window (get-selection))))
    (ps:chain sel
              (collapse (ps:@ sel focus-node)
                        (ps:@ sel focus-offset)))))

(define-command toggle-mark ()
  "Toggle the mark."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (if (string= (is-collapsed) "true")
        (progn
          (setf (mark-set mode) (not (mark-set mode)))
          (if (mark-set mode)
              (echo "Mark set")
              (echo "Mark deactivated")))
        (progn
          (collapse-to-focus)
          (echo "Mark set")))))

(define-command clear-selection ()
  "Clear the selection and unset the mark."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (unless (is-collapsed) (collapse-to-focus))
    (setf (mark-set mode) nil)
    (echo "Mark deactivated")))

(define-parenscript caret-move (&key action direction scale (n 1))
  (let ((sel (ps:chain window (get-selection))))
    (dotimes (i (ps:lisp n))
      (ps:chain sel (modify (ps:lisp action)
                            (ps:lisp direction)
                            (ps:lisp scale))))))

(define-command forward-char ()
  "Move caret forward by a character."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "character")))

(define-command backward-char ()
  "Move caret backward by a character."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "character")))

(define-command forward-word ()
  "Move caret forward by a word."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "word")))

(define-command backward-word ()
  "Move caret backward by a word."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "word")))

(define-command forward-line ()
  "Move caret forward by a line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "line")))

(define-command backward-line ()
  "Move caret backward by a line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "line")))

(define-command beginning-line ()
  "Move caret to the beginning of the line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "lineboundary")))

(define-command end-line ()
  "Move caret to the end of the line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "lineboundary")))

(define-command forward-sentence ()
  "Move caret forward to next end of sentence."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "sentence")))

(define-command backward-sentence ()
  "Move caret backward to start of sentence."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "sentence")))

(define-command forward-line-with-selection ()
  "Set mark and move caret forward by a line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (forward-line)))

(define-command backward-line-with-selection ()
  "Set mark and move caret backward by a line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (backward-line)))

(define-command forward-char-with-selection ()
  "Set mark and move caret forward by a character."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (forward-char)))

(define-command backward-char-with-selection ()
  "Set mark and move caret backward by a character."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (backward-char)))

(define-command forward-word-with-selection ()
  "Set mark and move caret forward by a word."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (forward-word)))

(define-command backward-word-with-selection ()
  "Set mark and move caret backward by a word."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (backward-word)))

(define-command beginning-line-with-selection ()
  "Set mark and move caret to the beginning of the line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (beginning-line)))

(define-command end-line-with-selection ()
  "Set mark and move caret to the end of line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (end-line)))
