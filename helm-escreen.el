;;; helm-escreen -- Uses helm to extend escreen
;;; Commentary:
;;; Introduces support for an arbitrary number of
;;; named escreens as well as helm autocompletion
;;; for selecting escreens

;;; Code:

(require 'helm)
(require 'escreen)

(setq escreen-max-screens 100)

(defvar helm-escreen-name-alist '(("main" . 0))
  "Maps the names of the current escreens to their corresponding escreen numbers.")

(defun helm-escreen-create-screen (escreen-name)
  "Create a new escreen and prompt for a name (ESCREEN-NAME).  Also save it to the list of escreens."
  (interactive "Mescreen name: ")
  (setf helm-escreen-name-alist
        (cons `(,escreen-name . ,(escreen-first-unused-screen-number))
              helm-escreen-name-alist))
  (call-interactively 'escreen-create-screen))

(defun helm-escreen-select-escreen ()
  "Use helm to select an escreen."
  (interactive)
  (escreen-goto-screen
   (helm-comp-read "Select an escreen: "
                   helm-escreen-name-alist
                   :alistp t
                   :must-match 'confirm)))

(defun helm-escreen-prompt-rename (escreen-name)
  "Prompt for a new ESCREEN-NAME."
  (interactive "MNew escreen name: ")
  escreen-name)

(defun helm-escreen-rename-escreen ()
  "Rename an escreen.  Selection is done with helm."
  (interactive)
  (let ((escreen-num (helm-comp-read "Select an escreen to rename:"
                                     helm-escreen-name-alist
                                     :alistp t
                                     :must-match 'confirm)))
    (setcar (rassoc escreen-num helm-escreen-name-alist)
            (call-interactively 'helm-escreen-prompt-rename))))

(defun helm-escreen-kill-escreen ()
  "Kill a named escreen with helm completion."
  (interactive)
  (setq helm-escreen-name-alist
        (map-remove (lambda (name num) (= num escreen-current-screen-number))
                    helm-escreen-name-alist))
  (call-interactively 'escreen-kill-screen))


(provide 'helm-escreen)
;;; helm-escreen.el ends here
