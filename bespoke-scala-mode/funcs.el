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

(defvar my-scala//fix-brace-last-key nil)
(defun my-scala/fix-brace ()
  ;; (message "this key: %s" (this-command-keys-vector))
  (when (and (not (eql my-scala//fix-brace-last-key ?\{))
         (= 1 (length (this-command-keys-vector)))
             (eql (elt (this-command-keys-vector) 0)
                  ?\C-m)
             (save-excursion
               (beginning-of-line)
               (looking-at " *\\} *$")))
    (save-excursion
      (insert-char ?\n)
      (funcall indent-line-function)))
  (setq my-scala//fix-brace-last-key (elt (this-command-keys-vector) 0)))

(defun my-scala/init ()
  (add-hook 'post-self-insert-hook #'my-scala/fix-brace nil 'local)
  (electric-indent-local-mode 0))

(defun my-scala/compute-ws-indent ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (looking-at "^ *$"))
      (forward-line -1))
    (back-to-indentation)
    (let ((ci (current-column)))
      (end-of-line)
      (skip-chars-backward "\s")
      (let ((adjustment
             (if (looking-back (rx (or "[" "{" "("
                                       "=" "=>" "=>>" "<-"
                                       )))
                 +1
               (if (and (looking-back (rx word-boundary
                                          (or
                                           "if" "while" "for" "match" "try"
                                           "then" "else" "do" "finally" "yield" "case"
                                          )))
                        (progn (goto-char (match-beginning 0))
                               (skip-chars-backward "\s")
                               (not (looking-back "end"))))
                   +1
                 0))))
        (+ ci (* adjustment scala-indent:step)))
      )
    ))

(defvar my-scala//ws-indent-last-line nil)
(defvar my-scala//ws-indent-last-command nil)
(defun my-scala/ws-indent (&optional direction)
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (my-scala/compute-ws-indent))
        (d (if (< 0 (or direction 1)) 1 -1)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      ;; TODO match indent
      (when (looking-at (rx (or "]" "}" ")" "case" "end")))
        (setq need (- need scala-indent:step)))
      (if (and (eql (line-number-at-pos) my-scala//ws-indent-last-line)
               (eq last-command my-scala//ws-indent-last-command))
          (indent-to (+ ci (* d scala-indent:step)))
        (indent-to need)))
    (if (< (current-column) (current-indentation))
        (forward-to-indentation 0))
    (setq my-scala//ws-indent-last-command this-command
          my-scala//ws-indent-last-line (line-number-at-pos)
    )
    ))

(defun my-scala/ws-indent-backwards ()
  (interactive "*")
  (my-scala/ws-indent -1))

(defun my-scala/toggle-indent ()
  (interactive)
  (when (eq major-mode 'scala-mode)
    (if (eq indent-line-function #'scala-indent:indent-line)
        (progn
          (message "Toggle indent: relative to previous line")
          (setq indent-line-function #'my-scala/ws-indent)
          (remove-hook 'post-self-insert-hook #'scala-indent:indent-on-special-words 'local))

      (progn
        (message "Toggle indent: Scala indent")
        (setq indent-line-function #'scala-indent:indent-line)
        (add-hook 'post-self-insert-hook #'scala-indent:indent-on-special-words 'local)))))

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
