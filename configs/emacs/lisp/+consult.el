;;; +consult.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Additional functions for Consult

;;; Code:

(defvar consult-colors-history nil
  "History for `consult-colors-emacs' and `consult-colors-web'.")

;; No longer preloaded in Emacs 28.
(autoload 'list-colors-duplicates "facemenu")
;; No preloaded in consult.el
(autoload 'consult--read "consult")

(defun consult-colors-emacs (color)
  "Show a list of all supported colors for a particular frame.\

You can insert the name (default), or insert or kill the hexadecimal or RGB value of the
selected color."
  (interactive
   (list (consult--read (list-colors-duplicates (defined-colors))
                        :prompt "Emacs color: "
                        :require-match t
                        :category 'color
                        :history '(:input consult-colors-history)
                        )))
  (insert color))

;; Adapted from counsel.el to get web colors.
(defun counsel-colors--web-list nil
  "Return list of CSS colors for `counsult-colors-web'."
  (require 'shr-color)
  (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

(defun consult-colors-web (color)
  "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB value of the
selected color."
  (interactive
   (list (consult--read (counsel-colors--web-list)
                        :prompt "Color: "
                        :require-match t
                        :category 'color
                        :history '(:input consult-colors-history)
                        )))
  (insert color))

(provide '+consult)

;;; +consult.el ends here
