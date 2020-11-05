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
  (add-hook 'post-self-insert-hook #'bespoke-scala/fix-brace nil 'local)
  (electric-indent-local-mode 0))

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
