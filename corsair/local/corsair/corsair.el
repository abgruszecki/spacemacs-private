;; -*- mode: emacs-lisp; lexical-binding: t -*-

(require 'general)

(general-define-key
 :prefix "H-<backspace>"
 "a" #'spacemacs/jump-to-last-layout
 "s" #'eyebrowse-last-window-config
 "w" #'spacemacs/alternate-window
 "r" #'spacemacs/alternate-buffer
 )

(general-define-key
 :prefix "H-["
 "a" #'persp-prev
 "s" #'eyebrowse-prev-window-config
 "w" #'previous-window
 "r" #'previous-buffer

 "(" #'evil-previous-open-paren
 )

(general-define-key
 :prefix "H-]"
 "a" #'persp-next
 "s" #'eyebrowse-next-window-config
 "w" #'next-window
 "r" #'next-buffer
 )

(general-define-key
 :prefix "H-<return>"
 "r" #'lazy-helm/helm-mini

 "0" #'eyebrowse-switch-to-window-config-0
 "1" #'eyebrowse-switch-to-window-config-1
 "2" #'eyebrowse-switch-to-window-config-2
 "3" #'eyebrowse-switch-to-window-config-3
 "4" #'eyebrowse-switch-to-window-config-4
 "5" #'eyebrowse-switch-to-window-config-5
 "6" #'eyebrowse-switch-to-window-config-6
 "7" #'eyebrowse-switch-to-window-config-7
 "8" #'eyebrowse-switch-to-window-config-8
 "9" #'eyebrowse-switch-to-window-config-9

 "<f1>" #'spacemacs/persp-switch-to-1
 "<f2>" #'my-perspective/switch-to-para
 "<f3>" #'my-perspective/switch-to-bespoke
 "<f4>" #'my-perspective/switch-to-dotty

 "<f5>" #'my-perspective/switch-to-dynamic
 "<f6>" #'my-perspective/switch-to-dynamic
 "<f7>" #'my-perspective/switch-to-dynamic
 "<f8>" #'my-perspective/switch-to-dynamic
 )

(provide 'corsair)
