;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Dotty package

(require 'dash)
(require 'evil)
(require 'projectile)

;;; Variables

(defvar sbt/compile-arguments "-color:never"
  "Arguments passed to compile command in SBT")

(defvar sbt/sbt-command-name '("sbt" "--no-colors")
  "Name of the command used to start SBT. ")

(defvar sbt/test-command "testCompilation"
  "Command sent to SBT when testing the project.")

(defvar sbt/compile-command "compile"
  "Command sent to SBT when compiling the project.")

(defvar-local sbt/inspect-input t
  "Prompt to save project files when sending a command to SBT?")

;;; Semi-internal variables

(defvar-local sbt/output/source-buffer nil
  "Buffer that was used to create the output in this buffer")

(defvar-local sbt/output/command nil
  "Command that was used to create the output in this buffer")

(defvar sbt/output/watched-buffers nil
  "Buffers with SBT output that are under watch")

(defvar sbt/last-operation nil
  "Last SBT command")

(defmacro sbt//invocation (command &rest arguments)
  "Used in interactive clause to both return arguments, and record that the command was invoked."
  `(-let [args (list ,@arguments)]
     (setq sbt/last-operation (cons ,command args))
     args))

(defun sbt//ensure-console-open ()
  (projectile-ensure-project default-directory)
  (let* ((name (sbt/buffer-name))
         (buf (get-buffer-create name))
         (project-root (projectile-project-root)))
    (with-current-buffer buf
      (cd project-root)
      (unless (eq major-mode 'sbt-mode)
        (sbt-mode))
      (apply #'make-comint-in-buffer name buf (car sbt/sbt-command-name) nil (cdr sbt/sbt-command-name))
      buf)))

(defun sbt/buffer-name (&optional suffix)
  (concat "*sbt<" (projectile-project-name) ">" suffix "*"))

(defun sbt/console ()
  (interactive)
  (select-window (display-buffer (sbt//ensure-console-open))))

;; TODO add a single buffer for locking

(defun sbt/lock ()
  "Lock last output buffer"
  (interactive)
  (let ((buffer-name (sbt/buffer-name "-output-locked")))
    (when (get-buffer buffer-name)
      (if (yes-or-no-p "Locked buffer already exists, delete it?")
          (kill-buffer buffer-name)
        (return)))

    (with-current-buffer (sbt/buffer-name "-output")
      (rename-buffer buffer-name)
      (sbt/watch)
      (display-buffer (current-buffer))
      )))

(defun sbt/locked ()
  "Display locked buffer"
  (interactive)
  (select-window (display-buffer (sbt/buffer-name "-output-locked"))))

(defun sbt/watch ()
  "Add current buffer to the list of watched buffers"
  (interactive)
  ;; TODO enable following once there is some form of queueing
  ;; (unless (member (current-buffer) sbt/output/watched-buffers)
  ;;   ;; TODO ensure that it is reasonable to have this buffer in the list (check if it has OK local vars?)
  ;;   (setq sbt/output/watched-buffers (cons (current-buffer) sbt/output/watched-buffers)))
  (setq sbt/output/watched-buffers (list (current-buffer))))

(defun sbt/output/refresh-watched ()
  "Refresh the output of watched buffers"
  (interactive (sbt//invocation
                'sbt/output/refresh-watched))
  (when sbt/output/watched-buffers
    ;; TODO watching multiple buffers is difficult, needs some sort of queueing
    (dotty//projectile/save-project-files)
    (with-current-buffer (car sbt/output/watched-buffers)
      (let ((source-buffer sbt/output/source-buffer)
            (source-cmd sbt/output/command))
        (with-current-buffer source-buffer
          (sbt/run source-cmd (car sbt/output/watched-buffers)))))))

(defun sbt/output/refresh (buffer)
  "Refresh the output of current buffer"
  (interactive (sbt//invocation
                'sbt/output/refresh
                (current-buffer)))
  (with-current-buffer buffer
    (dotty//projectile/save-project-files)
    (let ((source-buffer sbt/output/source-buffer)
          (source-cmd sbt/output/command)
          (target-buffer (current-buffer)))
      (with-current-buffer source-buffer
        (sbt/run source-cmd target-buffer)))))

(defun sbt/clean ()
  (ansi-color-filter-region (point-min) (point-max)))

(defun sbt/run (cmd &optional target-buffer)
  (when (and target-buffer (get-buffer target-buffer))
    (with-current-buffer target-buffer
      (erase-buffer)))
  (let ((resolved-target-buffer (if target-buffer
                                    (get-buffer-create target-buffer)
                                  (generate-new-buffer "sbt-output"))))
    (with-current-buffer resolved-target-buffer
      (unless (eq major-mode 'dotty-trace-mode)
        (dotty-trace-mode))
      ;; TODO set these post-factum? keep track of command success?
      (setq sbt/output/source-buffer (sbt/buffer-name)
            sbt/output/command cmd))
    (with-current-buffer (sbt/buffer-name)
      (comint-redirect-send-command cmd resolved-target-buffer nil))))

(defun sbt/compile-file (file)
  (interactive (sbt//invocation
                'sbt/compile-file
                (helm-read-file-name "SBT compile: "
                                     :name "Read file name (all marked files will be compiled)"
                                     :marked-candidates t)))
  (dotty//projectile/save-project-files)
  ;; TODO escape the name of the file
  (sbt/run (concat "dotc " sbt/compile-arguments " " file)
           (sbt/buffer-name "-output")))

(defun sbt/compile-this-file ()
  (interactive)
  (dotty//projectile/save-project-files)
  (setq sbt/last-operation `(sbt/compile-file ,buffer-file-name))
  (sbt/compile-file buffer-file-name))

(defun sbt/send-command (command)
  (interactive (sbt//invocation
                'sbt/send-command
                (read-string "Command: ")))
  ;; TODO read through comint-redirect-send-command to see how it works
  (with-current-buffer (sbt/buffer-name)
    (if (not (string-match-p comint-prompt-regexp
                             (buffer-substring (car comint-last-prompt)
                                               (cdr comint-last-prompt))))
        (error "Process is not ready for input")
      (dotty//projectile/save-project-files)
      (delete-region (cdr comint-last-prompt)
                     (point-max))
      (goto-char (point-max))
      (insert command)
      (comint-send-input))
    (display-buffer (current-buffer))))

(defun sbt/send-test-command ()
  (interactive (sbt//invocation 'sbt/send-test-command))
  (when (not dotty//prints-commented-out-p)
    (dotty/toggle-printing-global))
  (sbt/send-command sbt/test-command))

(defun sbt/send-compile-command ()
  (interactive (sbt//invocation 'sbt/send-compile-command))
  (sbt/send-command sbt/compile-command))

(defvar dotty//prints-commented-out-p nil)
(defmacro dotty//toggle-printing/replace (pattern replacement bound literal)
  ;; NOTE it mostly isn't a problem that bound isn't updated after replacements,
  ;; NOTE but only because it's ever only set to the end of the current line
  `(let ((cnt 0))
     (while (and (or (not ,bound) (> ,bound (point)))
             (re-search-forward (rx ,pattern) ,bound t))
       (replace-match ,replacement t ,literal)
       (setq cnt (1+ cnt)))
     cnt))

(defun dotty//uncomment-printing (&optional bound)
  (+
   (save-excursion
     (dotty//toggle-printing/replace "trace/*force*/(" "trace.force(" bound t))
   (save-excursion
     (dotty//toggle-printing/replace "/*newPrinter*/noPrinter" "new Printer" bound t))
  ))

(defun dotty//comment-printing (&optional bound)
  (+
   (save-excursion
     (dotty//toggle-printing/replace "trace.force(" "trace/*force*/(" bound t))
   (save-excursion
     (dotty//toggle-printing/replace "new Printer" "/*newPrinter*/noPrinter" bound t))
   ))

(defun dotty/toggle-printing-this-line ()
  (interactive)
  (if (save-excursion
            (beginning-of-line)
            (zerop (dotty//uncomment-printing (point-at-eol))))
      (save-excursion
        (beginning-of-line)
        (zerop (dotty//comment-printing (point-at-eol))))))

(defun dotty/toggle-printing-global ()
  (interactive)
  (save-excursion
    (if (not dotty//prints-commented-out-p)
        (mapc (lambda (buf)
                (with-current-buffer buf
                  (beginning-of-buffer)
                  (if (eq major-mode 'scala-mode) (dotty//comment-printing))))
              (projectile-project-buffers))
      (mapc (lambda (buf)
              (with-current-buffer buf
                (beginning-of-buffer)
                (if (eq major-mode 'scala-mode) (dotty//uncomment-printing))))
            (projectile-project-buffers))))
  (setf dotty//prints-commented-out-p (not dotty//prints-commented-out-p))
  (message "Dotty printing: %s" (if dotty//prints-commented-out-p "disabled" "enabled")))

(defun dotty/toggle-printing (prefix)
  (interactive "P")
  (if prefix (dotty/toggle-printing-this-line) (dotty/toggle-printing-global)))

(defun sbt/repeat-last-operation ()
  (interactive)
  (when (not sbt/last-operation)
    (error "No SBT command was yet run."))
  (apply 'funcall (car sbt/last-operation) (cdr sbt/last-operation)))

(defun sbt/repeat-last-command ()
  "Repeats last command in SBT buffer"
  (interactive (sbt//invocation
                'sbt/repeat-last-command))
  (display-buffer (sbt//ensure-console-open))
  (dotty//projectile/save-project-files)
  (with-current-buffer (sbt/buffer-name)
    (goto-char (point-max))
    (comint-previous-input 1)
    (comint-send-input)))

;;; Indirect

(defvar-local dotty/trace/indirect-buffer nil
  "Indirect buffer connected to the current buffer, if any")

(defmacro dotty//trace/in-indirect-buffer (&rest body)
  (declare (indent 0))
  `(progn
     (when (not (buffer-live-p dotty/trace/indirect-buffer))
       (setq dotty/trace/indirect-buffer nil))
     (if (not dotty/trace/indirect-buffer)
         (progn
           (setq dotty/trace/indirect-buffer
                 (make-indirect-buffer
                  (current-buffer)
                  (s-replace-regexp (rx "*" eol)
                                    (lambda (s) (concat "-focused" s))
                                    (buffer-name))))
           (with-current-buffer dotty/trace/indirect-buffer
             (dotty-trace-mode)))
       (with-current-buffer dotty/trace/indirect-buffer
         (when (buffer-narrowed-p)
           (widen))))
     (with-current-buffer dotty/trace/indirect-buffer
       ,@body)))

(defun dotty/trace/preview-header ()
  (interactive)
  (when (not (dotty-trace/current-line-trace-header))
    (error "Not looking at a trace header!"))
  (-let [p (point)]
    (dotty//trace/in-indirect-buffer
      (hs-hide-all)
      (goto-char p)
      (dotty/trace/narrow-header)
      (with-selected-window (display-buffer (current-buffer)
                                            '(nil . ((inhibit-same-window . t))))
        (goto-char (point-min))
        (beginning-of-line-text))
      )))

(defun dotty/trace/peek-header ()
  (interactive)
  (when (not (dotty-trace/current-line-trace-header))
    (error "Not looking at a trace header!"))
  (-let [p (point)]
    (dotty//trace/in-indirect-buffer
      (hs-hide-all)
      (select-window (display-buffer (current-buffer)
                                     '(nil . ((inhibit-same-window . t)))))
      (goto-char p)
      (save-excursion
       (dotty/trace/narrow-header))
      (dotty/jump-to-matching-trace-header)
      (beginning-of-line)
      (pcase (dotty-trace/current-line-trace-header)
        (`(opening . ,_)
         (evil-scroll-line-to-top 1)
         (forward-line)
         (save-excursion
           (dotty-trace/hs-forward-toggle-node))
         (forward-line)
         )
        (`(closing . ,_)
         (dotty-trace/hs-forward-toggle-node)
         (evil-scroll-line-to-top nil))))))

(defun dotty//trace/end-of-answer ()
  (interactive)
  (end-of-line)
  (backward-char)
  (assert (looking-at-p "{"))
  (evil-jump-item)
  (end-of-line)
  ;; (backward-char)
  )

(defun dotty/trace/narrow-header ()
  (interactive)
  (-if-let ((type . size) (dotty-trace/current-line-trace-header))
      (-let [(beg . end) (ecase type
                           ('opening
                            (cons (save-excursion
                                    (beginning-of-line)
                                    (point))
                                  (save-excursion
                                    (dotty/jump-to-matching-trace-header)
                                    (dotty//trace/end-of-answer)
                                    (point)
                                    )))
                           ('closing
                            (cons (save-excursion
                                    (dotty/jump-to-matching-trace-header)
                                    (beginning-of-line)
                                    (point))
                                  (save-excursion
                                    (dotty//trace/end-of-answer)
                                    (point)))))]
        (goto-char beg)
        (narrow-to-region beg end))
    (error "Not looking at a trace header!")))

;;; Motions

(defun dotty-trace/current-line-trace-header ()
  "Returns information about the trace header on current line, or nil if not looking at a trace header."
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((result (or
                   (and (looking-at " *==>") 'opening)
                   (and (looking-at " *<==") 'closing))))
      (and result (cons result (- (match-end 0) (match-beginning 0) 3))))))

(evil-define-motion dotty-trace/jump-to-opening-header ()
  :type line
  :jump t
  (-if-let ((type . size) (dotty-trace/current-line-trace-header))

      (re-search-backward
       (concat "^" (s-repeat (- size 2) " ") "==>"))

    (while (and (not (dotty-trace/current-line-trace-header))
                (zerop (forward-line -1)))))
  )

(evil-define-motion dotty-trace/jump-forwards-to-sibling-header ()
  :type line
  :jump t
  (while (and (not (dotty-trace/current-line-trace-header))
              (zerop (forward-line 1))))
  (end-of-line)
  (-if-let ((type . size) (dotty-trace/current-line-trace-header))
      (re-search-forward
       (concat "^" (s-repeat size " ") "==>"))))

(evil-define-motion dotty-trace/jump-backwards-to-sibling-header ()
  :type line
  :jump t
  (while (and (not (dotty-trace/current-line-trace-header))
              (zerop (forward-line -1))))
  (beginning-of-line)
  (-if-let ((type . size) (dotty-trace/current-line-trace-header))
      (re-search-backward
       (concat "^" (s-repeat size " ") "==>"))))

(evil-define-motion dotty/jump-to-matching-trace-header ()
  :type line
  (-if-let ((type . size) (dotty-trace/current-line-trace-header))
      (ecase type
        ('opening
         (re-search-forward
          (concat "^" (s-repeat size " ") "<=="))
         (beginning-of-line-text))

        ('closing
         (re-search-backward
          (concat "^" (s-repeat size " ") "==>"))
         (beginning-of-line-text)))
    (error "Not looking at a trace header!")))


;;; Utility functions

(defun dotty//projectile/save-project-files (&optional arg)
  "Save files in current project.
If ARG is non-nil, do not ask about saving (mimicks behaviour of `save-some-buffers')."
  (interactive "P")
  (-let [project-root (projectile-project-root)]
    (save-some-buffers arg
                       (lambda ()
                         (and buffer-file-name
                              (eq major-mode 'scala-mode)
                              (projectile-project-buffer-p (current-buffer)
                                                           project-root))))))

(defun dotty-trace/summarise-trace ()
  (interactive)
  (let ((atts 0)
        (errs 0)
        (res "???"))
    (save-excursion
      (goto-char (point-min))
      (while (and (prog1
                      t
                    ;; REAL BODY
                    (when (looking-at (rx bol "#!!!"))
                      (setq atts (1+ atts)))
                    (when (looking-at  (rx bol "-- Error"))
                      (setq errs (1+ errs)))
                    (when (looking-at (rx bol "[success] Total time:"))
                      (setq res "success"))
                    (when (looking-at (rx bol "[error] Total time:"))
                      (setq res "fail"))
                    ;; / REAL BODY
                    )
                  (zerop (forward-line))))
    (message "Command finished! Status: %s; errors: %s; attentions: %s" res errs atts))))

(defun sbt//post-redirect-cleanup ()
  (unless (get-buffer comint-redirect-output-buffer)
    (error "Could not retrieve buffer for post-redirect cleanup (%s)" comint-redirect-output-buffer))

  (with-current-buffer comint-redirect-output-buffer
    (hs-hide-all)
    (dotty-trace/summarise-trace)))

;;; Dotty minor mode

(define-minor-mode dotty-minor-mode
  "Minor mode containing configuration common to all Dotty modes"
  :keymap (make-sparse-keymap))

(add-hook 'scala-mode-hook 'dotty-minor-mode)

;;; SBT mode

(define-derived-mode sbt-mode comint-mode "SBT"
  "SBT mode"
  (dotty-minor-mode 1)

  ;; NOTE despite docs, this is meaningful on its own (changes redirection behaviour)
  ;; NOTE comint-use-prompt-regexp is unset b/c it screws up input editing
  (setq comint-prompt-regexp "^sbt:[-a-zA-Z]+> ")

  (add-hook 'comint-redirect-hook #'sbt//post-redirect-cleanup nil t))

;;; Dotty trace mode

(define-derived-mode dotty-trace-mode fundamental-mode "Dotty trace"
  (dotty-minor-mode 1)

  (font-lock-add-keywords
   nil
   `(,(concat "^ *" (regexp-opt (list "<==" "==>"))) . 'font-lock-keyword-face))
  (font-lock-mode t)

  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setf (alist-get 'dotty-trace-mode hs-special-modes-alist)
        `(,(rx "{{{")
          ,(rx "}}}")
          "//"
          nil nil))
  (hs-minor-mode 1)
  (hs-hide-all))

(defun dotty-trace/hs-forward-toggle-node ()
  (interactive)
  (save-excursion
    (end-of-line)
    (when (not (looking-back (rx "{{{")))
      (error "Current line doesn't start a block!"))
    (hs-toggle-hiding)))

;;; Keybindings

(defun dotty/set-keys ()
  (spacemacs/set-leader-keys-for-minor-mode 'dotty-minor-mode
    "$" #'sbt/console
    "@" #'sbt/repeat-last-command
    "." #'sbt/repeat-last-operation

    "!!" #'sbt/send-command
    "!c" #'sbt/send-compile-command
    "!t" #'sbt/send-test-command

    ";" #'dotty/toggle-printing

    "w" #'sbt/watch
    "l" #'sbt/locked
    "L" #'sbt/lock
    "r" #'sbt/output/refresh-watched
    "R" #'sbt/output/refresh
    "c" #'sbt/compile-file
    "C" #'sbt/compile-this-file)

  (evil-define-key 'normal dotty-trace-mode-map
    (kbd "RET") #'dotty-trace/hs-forward-toggle-node
    (kbd "<tab>") (lambda () (interactive) (re-search-forward "^#"))
    (kbd "<backtab>") (lambda () (interactive) (re-search-backward "^#"))
    (kbd "<C-tab>") (lambda () (interactive) (re-search-forward "^#!"))
    (kbd "<C-iso-lefttab>") (lambda () (interactive) (re-search-backward "^#!"))
    "g%" 'dotty/jump-to-matching-trace-header
    )

  (spacemacs/set-leader-keys-for-major-mode 'dotty-trace-mode
    "p" #'dotty/trace/peek-header
    "P" #'dotty/trace/preview-header
    "n" #'dotty/trace/narrow-header
    "<SPC>" #'dotty/jump-to-matching-trace-header
    "[" #'dotty-trace/jump-to-opening-header
    "{" #'dotty-trace/jump-backwards-to-sibling-header
    "}" #'dotty-trace/jump-forwards-to-sibling-header
    )
  )

;;; Internal functions

(defun dotty//start-dotty-lsp ()
  (-let [dotty-ide-artifact-file (concat (projectile-project-root) ".dotty-ide-artifact")]
    (when (not (file-exists-p dotty-ide-artifact-file))
      (error "Dotty LSP artifact file (%s) does not exist." dotty-ide-artifact-file))
    (-let [dotty-ide-artifact-ref (with-temp-buffer
                                    (insert-file-contents dotty-ide-artifact-file)
                                    (string-trim (buffer-string)))]
      (list "coursier" "launch" dotty-ide-artifact-ref "-M" "dotty.tools.languageserver.Main" "--" "-stdio"))))

(defun sbt//comint-send-input-advice (underlying &rest args)
  (interactive)
  (when (and (eq major-mode 'sbt-mode)
             sbt/inspect-input)
    (dotty//projectile/save-project-files))
  (apply underlying args))

(advice-add 'comint-send-input :around 'sbt//comint-send-input-advice)

(provide 'dotty)
