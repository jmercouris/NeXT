;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-command spell-check-word (&key (word nil word-supplied-p))
  "Spell check a word."
  (if word-supplied-p
      (enchant:with-dict (lang (spell-check-language *browser*))
        (enchant:dict-check lang word))
      (let ((word (first (prompt
                          :prompt "Spell check word"
                          :sources (make-instance 'prompter:raw-source)))))
        (if (enchant:with-dict (lang (spell-check-language *browser*))
              (enchant:dict-check lang word))
            (echo "~s spelled correctly." word)
            (echo "~s is incorrect." word)))))

(define-command spell-check-highlighted-word ()
  "Spell check a highlighted word. If a word is incorrectly spelled,
pull up a prompt of suggestions."
  (let ((word (%copy)))
    (spell-check-prompt word)))

(defun spell-check-prompt (word)
  "Spell check `word', if incorrectly spelled, prompt the user with
suggestions."
  (if (spell-check-word :word word)
        (echo "Highlighted word ~s spelled correctly." word)
        (progn (echo "Highlighted word ~s spelled incorrectly." word)
               (spell-check-suggest-word :word word))))

(define-command spell-check-word-at-cursor ()
  "Spell check the word at cursor."
  (let* ((contents (nyxt/input-edit-mode::active-input-area-content))
         (cursor-position (truncate (nyxt/input-edit-mode::active-input-area-cursor))))
    (let ((text-buffer (make-instance 'text-buffer:text-buffer))
          (cursor (make-instance 'text-buffer:cursor)))
      (cluffer:attach-cursor cursor text-buffer)
      (text-buffer::insert-string cursor contents)
      (setf (cluffer:cursor-position cursor)
            cursor-position)
      (spell-check-prompt (text-buffer::word-at-cursor cursor)))))

(define-command spell-check-suggest-word (&key word)
  "Suggest a spelling for a given word."
  (let ((selected-word (first (prompt
                               :input word
                               :prompt "Suggest spelling (3+ characters)"
                               :sources (make-instance 'enchant-source)))))
    (trivial-clipboard:text selected-word)
    (echo "Word copied to clipboard.")))

(define-class enchant-source (prompter:source)
  ((case-sensitive-p nil)
   (prompter:name "Enchant")
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions source))
      (when (> (length input) 2)
        (enchant:with-dict (lang (spell-check-language *browser*))
          (enchant:dict-suggest lang input)))))))
