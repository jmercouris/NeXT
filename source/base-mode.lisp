;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-mode base-mode ()
  "Bind general-purpose commands defined by `define-command'.
This mode is a good candidate to be passed to `make-buffer'."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (keymap-scheme (define-scheme "base"
                    scheme:cua
                    (list
                     "C-q" 'quit
                     "C-[" 'switch-buffer-previous
                     "C-]" 'switch-buffer-next
                     "M-down" 'switch-buffer
                     "C-w" 'delete-current-buffer
                     "C-shift-tab" 'switch-buffer-previous
                     "C-tab" 'switch-buffer-next
                     "C-`" 'switch-buffer-last
                     "C-pageup" 'switch-buffer-previous
                     "C-pagedown" 'switch-buffer-next
                     "C-l" 'set-url
                     "M-l" 'set-url-new-buffer
                     "C-u M-l" 'set-url-new-nosave-buffer
                     "f5" 'reload-current-buffer
                     "C-r" 'reload-current-buffer
                     "C-R" 'reload-buffers
                     "C-m o" 'set-url-from-bookmark
                     "C-m s" 'bookmark-current-url
                     "C-d" 'bookmark-current-url
                     "C-m C-s" 'bookmark-buffer-url
                     "C-m k" 'delete-bookmark
                     "C-t" 'make-buffer-focus
                     "C-m u" 'bookmark-url
                     "C-b" 'list-bookmarks
                     "M-c l" 'copy-url
                     "M-c t" 'copy-title
                     "f1 f1" 'help
                     "f1 t" 'tutorial
                     "f1 r" 'manual
                     "f1 v" 'describe-variable
                     "f1 f" 'describe-function
                     "f1 c" 'describe-command
                     "f1 C" 'describe-class
                     "f1 s" 'describe-slot
                     "f1 k" 'describe-key
                     "f1 b" 'describe-bindings
                     "f11" 'toggle-fullscreen
                     "C-O" 'load-file
                     "C-j" 'list-downloads
                     "C-space" 'execute-command
                     "C-n" 'make-window
                     "C-shift-W" 'delete-current-window
                     "C-W" 'delete-current-window
                     "M-w" 'delete-window
                     "C-/" 'reopen-buffer
                     "C-shift-t" 'reopen-buffer
                     "C-T" 'reopen-buffer
                     "C-p" 'print-buffer
                     "C-o" 'open-file)

                    scheme:emacs
                    (list
                     "C-x C-c" 'quit
                     "C-x k" 'delete-buffer
                     "C-x C-k" 'delete-current-buffer
                     "C-x left" 'switch-buffer-previous
                     "C-x right" 'switch-buffer-next
                     "C-x b" 'switch-buffer
                     "C-x C-b" 'nyxt/buffer-listing-mode::list-buffers
                     "C-M-l" 'copy-url
                     "C-M-i" 'copy-title
                     "C-h C-h" 'help
                     "C-h h" 'help
                     "C-h t" 'tutorial
                     "C-h r" 'manual
                     "C-h v" 'describe-variable
                     "C-h f" 'describe-function
                     "C-h c" 'describe-command
                     "C-h C" 'describe-class
                     "C-h s" 'describe-slot
                     "C-h k" 'describe-key
                     "C-h b" 'describe-bindings
                     "C-d" 'list-downloads
                     "M-x" 'execute-command
                     "C-x r j" 'set-url-from-bookmark
                     "C-x r M" 'bookmark-current-url
                     "C-x r m" 'bookmark-buffer-url
                     "C-x r k" 'delete-bookmark
                     "C-x r u" 'bookmark-url
                     "C-x 5 2" 'make-window
                     "C-x 5 0" 'delete-current-window
                     "C-x 5 1" 'delete-window
                     "C-x C-f" 'open-file)

                    scheme:vi-normal
                    (list
                     "Z Z" 'quit
                     "[" 'switch-buffer-previous
                     "]" 'switch-buffer-next
                     "g b" 'switch-buffer
                     "d" 'delete-buffer
                     "D" 'delete-current-buffer
                     "B" 'make-buffer-focus
                     "o" 'set-url
                     "O" 'set-url-new-buffer
                     "g o" 'set-url-new-nosave-buffer
                     "m u" 'bookmark-url
                     "m d" 'delete-bookmark
                     "R" 'reload-current-buffer
                     "r" 'reload-buffers
                     "m o" 'set-url-from-bookmark
                     "m m" 'bookmark-buffer-url
                     "m M" 'bookmark-current-url
                     "y u" 'copy-url
                     "y t" 'copy-title
                     ;; TODO: Use "f1 *" instead?
                     "C-h C-h" 'help
                     "C-h h" 'help
                     "C-h t" 'tutorial
                     "C-h r" 'manual
                     "C-h v" 'describe-variable
                     "C-h f" 'describe-function
                     "C-h c" 'describe-command
                     "C-h C" 'describe-class
                     "C-h s" 'describe-slot
                     "C-h k" 'describe-key
                     "C-h b" 'describe-bindings
                     ":" 'execute-command
                     "W" 'make-window
                     "C-w C-w" 'make-window
                     "C-w q" 'delete-current-window
                     "C-w C-q" 'delete-window
                     "u" 'reopen-buffer))
                  :type keymap:scheme)))
