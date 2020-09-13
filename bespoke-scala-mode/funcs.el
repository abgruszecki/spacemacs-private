;; Automatically insert asterisk in a comment when enabled
(defun my-scala/newline-and-indent-with-asterisk ()
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

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

(defun my-scala/toggle-indent ()
  (interactive)
  (when (eq major-mode 'scala-mode)
    (if (eq indent-line-function #'indent-relative)
        (progn
          (message "Toggle indent: Scala indent")
          (setq indent-line-function #'scala-indent:indent-line))

      (message "Toggle indent: relative to previous line")
      (setq indent-line-function #'indent-relative))))

(defun my-sbt/sbt-do-reload ()
  "Execute the sbt `reload' command for the project."
  (interactive)
  (sbt:command "reload"))
