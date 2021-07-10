;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun tutorial-content ()
  (markup:markup
   (:h2 "Core Concepts")
   (:h3 "Keybindings and Commands")
   (:p "Commands are invoked by pressing specific keys or from
the " (:code "execute-command") " menu (" (:code (binding-keys 'execute-command))
").")
   (:p "Keybindings are represented like this: 'C-x'. In this example, 'C' is a
shortcut for the modifier 'control', and 'x' represents the character 'x'. To
input the 'C-x' keybinding you would keep 'control' pressed and then hit 'x'.
Multiple key presses can be chained: in 'C-x C-s', you would have to press
'C-x', and then press 'C-s'.")
   (:p "Modifier keys legend:")
   (:ul
    (:li (:code "control") " (" (:code "C") "): Control key")
    (:li (:code "super") " (" (:code "S") "): Windows key, Command key")
    (:li (:code "meta") " (" (:code "M") "): Alt key, Option key")
    (:li (:code "shift") " (" (:code "s") "): Shift key"))
   (:p "Modifiers can be remapped, see the " (:code "modifier-translator")
       " slot of the " (:code "gtk-browser") " class.")

   (:h3 "Quickstart keys")
   (:ul
    (:li (:code (binding-keys 'set-url)) ": Load URL")
    (:li (:code (binding-keys 'reload-current-buffer)) ": Reload buffer")
    (:li (:code (binding-keys 'set-url-new-buffer)) ": Load URL in new buffer")
    (:li (:code (binding-keys 'switch-buffer-previous)) ", " (:code (binding-keys 'switch-buffer-next)) ": Switch buffer")
    (:li (:code (binding-keys 'nyxt/web-mode:history-backwards)) ": Backwards history")
    (:li (:code (binding-keys 'nyxt/web-mode:history-forwards)) ": Forwards history")
    (:li (:code (binding-keys 'nyxt/web-mode:follow-hint)) ": Follow link in current buffer")
    (:li (:code (binding-keys 'nyxt/web-mode:follow-hint-new-buffer)) ": Follow link in new buffer")
    (:li (:code (binding-keys 'quit)) ": Quit")
    (:li (:code (binding-keys 'execute-command)) ": Run a command by name")
    (:li (:code (binding-keys 'describe-bindings)) ": List all bindings for the current buffer"))

   (:h3 "Buffers")
   (:p "Nyxt uses the concept of buffers instead of tabs. Unlike tabs, buffers
are fully separated, each buffer having its own behavior and settings.")
   (:h3 "Modes")
   (:p "Each buffer has its own list of modes, ordered by priority.  A mode is a
set of functions, hooks, keybindings and other facilities that may modify the
behavior of a buffer.  For example, 'blocker-mode' can be used for domain-based
adblocking while 'noscript-mode' disables JavaScript.")
   (:p "Each buffer has separate instances of modes, which means that altering
the settings of a mode in a buffer does not impact other buffers.  Mode specific
functions/commands are only available when a mode is enabled for the current
buffer.")
   (:p "Each mode has an associated " (:i "mode toggler") " which is a command
of the same name that toggles the mode for the current buffer.")

   (:h3 "Prompt buffer")
   (:p "The prompt buffer is a menu that will appear when a command requests user
input. For example, when invoking the " (:code "set-url") " command, you must
supply the URL you would like to navigate to.  The prompt buffer can provide
suggestions.  The list of suggestions will automatically narrow down to those
matching your input as you type.")
   (:ul
    (:li  (command-markup 'nyxt/prompt-buffer-mode:return-selection
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Validate the selected suggestion(s) or the current input if there is
no suggestion.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:return-selection-over-action
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Query the user for an action to run over the selected suggestion(s)."))
   (:p " Some commands support multiple selections, for
instance " (:code "delete-buffer") " can delete all selected buffers at once.
When the input is changed and the suggestions are re-filtered, the selection is
not altered even if the marked elements don't show.")
   (:p "When at least one suggestion is marked, only the marked suggestions are processed
upon return.  The suggestion under the cursor is not processed if not marked.")
   (:ul
    (:li  (command-markup 'nyxt/prompt-buffer-mode:toggle-mark
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Select or deselect the current suggestion.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:mark-all
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Select all currently-displayed suggestions.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:unmark-all
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Deselect all currently-displayed suggestions.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:toggle-mark-all
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Toggle the mark of all currently-displayed suggestions.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:toggle-attributes-display
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Change which attributes are displayed in the suggestions list."))

   (:h3 "Message Area")
   (:p "The message area represents a space (typically at the bottom of a
window) where Nyxt outputs messages back to you. To view the history of all
messages, invoke the command " (:code "list-messages") ".")

   (:h3 "Status Area")
   (:p "The status area is where information about the state of that buffer is
printed (typically at the bottom of a window). By default, this includes the
active modes, the URL, and the title of the current buffer.")

   (:h2 "Basic controls")
   (:h3 "Moving within a buffer")
   (:p "To move within a buffer, several commands are provided:")
   (:ul
    (:li (command-markup 'nyxt/web-mode:scroll-down) ": Move down.")
    (:li (command-markup 'nyxt/web-mode:scroll-up) ": Move up.")
    (:li (command-markup 'nyxt/web-mode:scroll-to-bottom) ": Jump to bottom of page.")
    (:li (command-markup 'nyxt/web-mode:scroll-to-top) ": Jump to top of page."))
   (:h3 "Setting the URL")
   (:p "When ambiguous URLs are inputted, Nyxt will attempt the best guess it
can. If you do not supply a protocol in a URL, HTTPS will be assumed. To
visit a site supporting only the less secure HTTP, you must explicitly type the
full URL including the 'http://' prefix.")
   (:ul
    (:li (command-markup 'set-url) ": Set URL of current buffer.")
    (:li (command-markup 'set-url-new-buffer) ": Open a new buffer and set its URL.")
    (:li (command-markup 'make-buffer-focus) ": Make a new empty buffer."))
   (:h3 "Switching buffers")
   (:ul
    (:li (command-markup 'switch-buffer) ": Switch buffer using fuzzy completion
to quickly find whatever buffer you are looking for.")
    (:li (command-markup 'switch-buffer-next) ": Go to next buffer.")
    (:li (command-markup 'switch-buffer-previous) ": Go to previous buffer."))
   (:h3 "Copying and Pasting")
   (:p "Unlike other web browsers, Nyxt provides powerful ways of copying
   and pasting content via different commands. Starting from:")
   (:ul
    (:li (command-markup 'nyxt/web-mode:copy) ": " (command-docstring-first-sentence 'nyxt/web-mode:copy))
    (:li (command-markup 'nyxt/web-mode:paste) ": " (command-docstring-first-sentence 'nyxt/web-mode:paste)))
   (:p "Passing through webpage's data:")
   (:ul
    (:li (command-markup 'copy-url) ": " (command-docstring-first-sentence 'copy-url))
    (:li (command-markup 'copy-title) ": " (command-docstring-first-sentence 'copy-title))
    (:li (command-markup 'nyxt/web-mode:copy-placeholder) ": " (command-docstring-first-sentence 'nyxt/web-mode:copy-placeholder))
    (:li (command-markup 'nyxt/web-mode:copy-hint-url) ": " (command-docstring-first-sentence 'nyxt/web-mode:copy-hint-url)))
   (:p "Leveraging password managers: ")
   #+nil
   (:ul 
    (:li (command-markup 'copy-username) ": " (command-docstring-first-sentence 'copy-username))
    (:li (command-markup 'copy-password) ": " (command-docstring-first-sentence 'copy-password))
    (:li (command-markup 'copy-password-prompt-details) ": " (command-docstring-first-sentence 'copy-password-prompt-details)))
   (:p "And more: ")
   #+nil 
   (:ul
    (:li (command-markup 'nyxt/web-mode:paste-from-clipboard-ring) ": " (command-docstring-first-sentence 'nyxt/web-mode:paste-from-clipboard-ring))
    (:li (command-markup 'copy-system-information) ": " (command-docstring-first-sentence 'copy-system-information)))
   (:h3 "Link navigation")
   (:p "Link-hinting allows you to visit URLs on a page without using the mouse.
Invoke one of the commands below: several hints will appear on screen and all
links on the page will be listed in the prompt buffer.  You can select the hints
by matching against the hint, the URL or the title.")
   (:ul
    (:li (command-markup 'nyxt/web-mode:follow-hint) ": Go to link in current buffer.")
    (:li (command-markup 'nyxt/web-mode:follow-hint-new-buffer-focus) ": Create new buffer with link; focus on new buffer.")
    (:li (command-markup 'nyxt/web-mode:follow-hint-new-buffer) ": Create new buffer with link; keep focus on current buffer."))
   (:h3 "Using the buffer history")
   (:p "History is represented as a tree that you can traverse: when you go back
in history, then follow a new URL, it effectively creates a new branch without
deleting the old path. The tree makes sure you never lose track of where you've
been.")
   (:ul
    (:li (command-markup 'nyxt/web-mode:history-forwards) ": History forwards.")
    (:li (command-markup 'nyxt/web-mode:history-backwards) ": History backwards.")
    (:li (command-markup 'nyxt/web-mode:history-forwards-query) ": History forwards query to any following location on the branch.")
    (:li (command-markup 'nyxt/web-mode:history-backwards-query) ": History backwards query to any previous location.")
    (:li (command-markup 'nyxt/web-mode:history-forwards-all-query) ": History forwards query to any following location on all branches.")
    (:li (command-markup 'nyxt/web-mode:history-all-query) ": History all query, jump to any history entry."))
   (:p "You can also view a full tree of the history for a given buffer by
invoking the command 'buffer-history-tree'.")
   (:h3 "Searching")
   (:p "Nyxt can search a single buffer or multiple buffers at the same time.")
   (:p "You can view suggestions for search results in the prompt buffer in one
place rather than having to jump around in a buffer (or multiple buffers).")
   (:ul
    (:li (command-markup 'nyxt/web-mode:search-buffer) ": Search buffer.")
    (:li (command-markup 'nyxt/web-mode:search-buffers) ": Search multiple buffers.")
    (:li (command-markup 'nyxt/web-mode:remove-search-hints) ": Remove the highlighting around the search hits."))
   (:h3 "Bookmarks")
   (:p "The bookmark file "
       (:code (let ((buffer (make-instance 'buffer)))
                (expand-path (bookmarks-path buffer))))
       " is made to be human readable and editable.
Bookmarks can have the following settings:")
   (:ul
    (:li (:code ":url") ": The URL of the bookmark.")
    (:li (:code ":title") ": The title of the bookmark.")
    (:li (:code ":tags") ": A list of strings.  Useful to categorize and filter bookmarks."))
   (:p "Bookmark-related commands")
   (:ul
    (:li (command-markup 'bookmark-current-url) ": Bookmark current page.
Prompt for tags.  The input defaults to the existing tags: if some tags are
removed from the input, they are also removed from the existing bookmark.")
    (:li (command-markup 'bookmark-buffer-url) ": Same as above but prompt for a buffer first.")
    (:li (command-markup 'bookmark-url) ": Same as above but prompt for a URL first.")
    (:li (command-markup 'nyxt/web-mode:bookmark-hint) ": Same as above but prompt for a hinted URL first.")
    (:li (command-markup 'set-url-from-bookmark) ": Open bookmark in current buffer.")
    (:li (command-markup 'delete-bookmark) ": Delete queried bookmarks.")
    (:li (command-markup 'list-bookmarks) ": Display a new buffer containing the
list of all bookmarks."))
   (:h3 "Application mode")
   (:p "The command " (:code "application-mode") " forwards all keys to the
renderer. For instance, using the default binding of Nyxt (" (:code "web-cua-map") ") the
key-binding " (:code "C-i") " executes " (:code "autofill") ". Suppose
a user is using their email client which also uses " (:code "C-i") " for the italic command. Thus, after
executing " (:code "application-mode") " the " (:code "C-i") " binding is associated
with the webpage's italic command instead of " (:code "autofill") ". Finally, the
user can return to their configuration just by executing " (:code "application-mode") " again.")
   (:h3 "Enable and Disable modes")
   (:p "The command " (:code "enable-mode") " allows the user to apply a
mode (such as " (:code "nosound-mode") " or " (:code "dark-mode") ") to multiple
buffers at once. Conversely, it is possible to revert this action
executing "(:code "disable-mode") " while choosing exactly the same buffers and
the same mode previously selected.")
   (:h3 "Visual mode")
   (:p "Select text without a mouse. Nyxt's "
       (:code "visual-mode") " imitates Vim's visual mode (and comes with the
CUA and Emacs-like keybindings out of the box, too). Activate it with the "
       (command-markup 'nyxt/visual-mode:visual-mode) " command.")
   (:p "Visual mode provides the following commands: ")
   (:ul
    (:li (command-markup 'nyxt/visual-mode:visual-mode
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Quit visual mode.")
    (:li (command-markup 'nyxt/visual-mode:select-paragraph
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Open up a prompt buffer for selecting a paragraph you want to
set the caret on.")
    (:li (command-markup 'nyxt/visual-mode:toggle-mark
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Toggle text selection.")
    (:li (command-markup 'nyxt/visual-mode:forward-char
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret forward by a character (if any text selected, moves the
selection forward by a character).")
    (:li (command-markup 'nyxt/visual-mode:backward-char
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret backward by a character (if any text selected, moves the
selection backward by a character).")
    (:li (command-markup 'nyxt/visual-mode:forward-word
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret forward by a word (if any text selected, moves the
selection forward by a word).")
    (:li (command-markup 'nyxt/visual-mode:backward-word
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret backward by a word (if any text selected, moves the
selection backward by a word).")
    (:li (command-markup 'nyxt/visual-mode:forward-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret forward by a line (if any text selected, moves the
selection forward by a line).")
    (:li (command-markup 'nyxt/visual-mode:backward-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret backward by a line (if any text selected, moves the
selection backward by a line).")
    (:li (command-markup 'nyxt/visual-mode:beginning-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret to the beginning of the line.")
    (:li (command-markup 'nyxt/visual-mode:end-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret to the end of the line.")
    (:li (command-markup 'nyxt/visual-mode:forward-sentence
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret forward by a sentence (if any text selected, moves the
 selection forward by a sentence).")
    (:li (command-markup 'nyxt/visual-mode:backward-sentence
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move caret backward by a sentence (if any text selected, moves the
 selection backward by a sentence)."))
   (:p "Commands designed to ease the use for CUA users (but available to all users): ")
   (:ul
    (:li (command-markup 'nyxt/visual-mode:forward-char-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Turn on the text selection and move caret forward by a character.")
    (:li (command-markup 'nyxt/visual-mode:backward-char-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Turn on the text selection and move caret backward by a character.")
    (:li (command-markup 'nyxt/visual-mode:forward-line-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Turn on the text selection and move caret forward by a line.")
    (:li (command-markup 'nyxt/visual-mode:backward-line-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Turn on the text selection and move caret backward by a line."))
   (:p "A note for " (:code "emacs-mode") " users: unlike in Emacs, in Nyxt the command "
       (command-markup 'nyxt/visual-mode:toggle-mark
                       :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
       " is bound to Shift-space, as C-space is bound to 'execute-command,
overriding any mode keybinding. If you want to toggle mark with C-space,
you'll need to set your own override-map such that C-space is not bound.
An example:")
   (:pre (:code "
\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"override-map\")))
                   (define-key map
                     \"M-x\" 'execute-command)))))"))

   (:h3 "Miscellaneous")
   (:ul
    (:li (command-markup 'nyxt/web-mode:zoom-page)
         ", " (command-markup 'nyxt/web-mode:unzoom-page)
         ", " (command-markup 'nyxt/web-mode:reset-page-zoom)
         ": Control the page zoom.")
    (:li (command-markup 'nyxt/web-mode:jump-to-heading) ": Query a heading (a
section) of the current page and jump to it.")
    (:li (command-markup 'nyxt/web-mode::autofill) ": See the "
         (:code "autofills") " browser slot.")
    (:li (command-markup 'download-open-file)
         ": Open file in Nyxt or externally.  See " (:code "open-file-function") ".")
    (:li (command-markup 'edit-with-external-editor)
         ": Edit selected HTML input tag with an external editor.")
    (:li (command-markup 'quit) ": Close all Nyxt windows and quit."))

   (:h2 "The Nyxt Help System")
   (:p "Nyxt provides introspective and help capabilities.  All commands,
classes, slots, variables, functions and bindings can be inspected for
definition and documentation.")
   (:ul
    (:li (command-markup 'help) ": Open up a small help buffer.")
    (:li (command-markup 'tutorial) ": Open up this tutorial.")
    (:li (command-markup 'describe-key) ": Lets you to input a key binding and
see what command it is bound to.")
    (:li (command-markup 'describe-bindings) ": View all of your currently set
bindings in the current buffer.")
    (:li (command-markup 'describe-command) ": Find out about a particular
command (including showing its source).")
    (:li (command-markup 'describe-function) ": Find out about a particular
function.")
    (:li (command-markup 'describe-variable) ": View the value and documentation
of a variable.")
    (:li (command-markup 'describe-class) ": Lookup a class documentation and all its slots.")
    (:li (command-markup 'describe-slot) ": Lookup a class slot value and documentation.")
    (:li (command-markup 'describe-any) ": The conflation of all the 'describe' functions."))
   (:p "A good starting point is to study the documentation of the classes "
       (:code "browser") ", " (:code "window") ", " (:code "buffer") " and "
       (:code "prompt-buffer") ".")))
