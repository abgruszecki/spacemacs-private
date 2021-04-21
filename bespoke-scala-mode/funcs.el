;; Automatically insert asterisk in a comment when enabled
(defun my-scala/newline-and-indent-with-asterisk ()
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(defun my-scala//handle-multiline-comment (&rest r)
  (when (eq major-mode 'scala-mode)
    (scala-indent:insert-asterisk-on-multiline-comment)))

(defun my-scala/set-indent (u)
  (interactive "p")
  (ecase u
    ((2 4) (setq-local scala-indent:step u))))

(defun my-scala/init ()
  (setq spacemacs-jump-handlers spacemacs-default-jump-handlers)
  (flycheck-mode 0)

  (bespoke-scala/init)
  )

(defun my-scala/scala-join-line ()
  "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
  (interactive)
  (let (join-pos)
    (save-excursion
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-line)
        (call-interactively 'scala-indent:join-line)
        (setq join-pos (point))))

    (when join-pos
      (goto-char join-pos))))

(defun my-sbt/sbt-do-reload ()
  "Execute the sbt `reload' command for the project."
  (interactive)
  (sbt:command "reload"))

;; (defun comint-redirect-results-list-from-process0 (process command regexp regexp-group)
;;   "Send COMMAND to PROCESS.
;; Return a list of expressions in the output which match REGEXP.
;; REGEXP-GROUP is the regular expression group in REGEXP to use."
;;   (let ((output-buffer " *Comint Redirect Work Buffer*")
;;         results)
;;     (with-current-buffer (get-buffer-create output-buffer)
;;       (erase-buffer)
;;       (comint-redirect-send-command-to-process command
;;                                                output-buffer process nil t)
;;       ;; Wait for the process to complete
;;       (set-buffer (process-buffer process))
;;       (sleep-for 1)
;;       ;; (while (not comint-redirect-completed)
;;       ;;   (accept-process-output process 0.1))
;;       (message "comint-redirect-completed: %s" comint-redirect-completed)
;;       ;; Collect the output
;;       (set-buffer output-buffer)
;;       (goto-char (point-min))
;;       ;; Skip past the command, if it was echoed
;;       (and (looking-at command)
;;            (forward-line))
;;       (while (and (not (eobp))
;;                   (re-search-forward regexp nil t))
;;         (push (buffer-substring-no-properties
;;                (match-beginning regexp-group)
;;                (match-end regexp-group))
;;               results))
;;       (nreverse results))))

;; (defun comint-redirect-results-list0 (command regexp regexp-group)
;;   "Send COMMAND to current process.
;; Return a list of expressions in the output which match REGEXP.
;; REGEXP-GROUP is the regular expression group in REGEXP to use."
;;   (comint-redirect-results-list-from-process0
;;    (get-buffer-process (current-buffer))
;;    command regexp regexp-group))

;; (defun sbt:get-completions (input)
;;    (sbt:require-buffer)
;;    (when (not (comint-check-proc (current-buffer)))
;;      (error "process not running in buffer %s" (current-buffer)))
;;    (when (or (null input) (string-match "^\\s *$" input))
;;      (setq input ""))
;;    (let ((submode
;;           (save-excursion
;;             (comint-goto-process-mark)
;;             (beginning-of-line)
;;             (cond ((looking-at sbt:sbt-prompt-regexp) 'sbt)
;;                   ((looking-at sbt:console-prompt-regexp) 'console)
;;                   ('t (error "process not ready (no prompt found)"))))))
;;      (message "Querying completions for %s..." input)
;;      (setq input
;;            (cond ((eq submode 'sbt) (concat "completions \""
;;                                             (sbt:scala-escape-string input)
;;                                             "\""))
;;                  ((eq submode 'console) (concat ":completions " input))))
;;      (message input)
;;      (prog1
;;          (comint-redirect-results-list0 input
;;                                         sbt:completions-regex
;;                                         1)
;;        (message nil))))
