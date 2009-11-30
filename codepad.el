;; codepad.el --- Emacs integration for codepad.org
;;
;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Website: http://github.com/ruediger/emacs-codepad
;; Keywords: codepad paste pastie pastebin
;;
;; This code is inspired by gist.el (written by Christian Neukirchen et.al.)
;; see http://github.com/defunkt/gist.el/blob/master/gist.el
;;
;; This file is NOT part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This code can be used to paste code to codepad.org.

;; codepad-paste-region pastes a region to codepad.org. The URL is printed
;; and if codepad-view is T opened in the browser.
;;
;; codepad-paste-buffer pastes the whole buffer.

;;; TODO:

;; * fetch code from codepad.org
;; * fetch Output from codepad.org (if run is True)
;; * support projects (http://project.codepad.org)
;; * support user accounts

;;; Idea:

;; add a local variable to each buffer with (a list?) of codepad ids so you new pastes
;; from this buffer are added as a reply to the original paste.

;;; Code:

(defconst +codepad-url+ "http://codepad.org")

(defconst +codepad-lang+ '((c-mode . "C")
                           (c++-mode . "C++")
                           (d-mode . "D")
                           (haskell-mode . "Haskell")
                           (lua-mode . "Lua")
                           (ocaml-mode . "OCaml")
                           (php-mode . "PHP")
                           (perl-mode . "Perl")
                           (python-mode . "Python")
                           (ruby-mode . "Ruby")
                           (scheme-mode . "Scheme")
                           (tcl-mode . "Tcl")))

(defconst +codepad-default-lang+ "Plain Text")

(defgroup codepad nil
  "Codepad paste support"
  :prefix "codepad-"
  :tag "Codepad"
  :group 'external
  :link '(url-link "http://github.com/ruediger/emacs-codepad"))

(defcustom codepad-private 'ask
  "Private pastes?"
  :group 'codepad
  :type '(radio
          (const :tag "Always ask" :value ask)
          (const :tag "Check prefix" :value prefix)
          (const :tag "No" :value no)
          (const :tag "Yes" :value yes)))

(defcustom codepad-run 'yes
  "Run pastes?"
  :group 'codepad
  :type '(radio
          (const :tag "Always ask" :value ask)
          (const :tag "Check prefix" :value prefix)
          (const :tag "No" :value no)
          (const :tag "Yes" :value yes)))

(defcustom codepad-view t
  "View paste in browser?"
  :group 'codepad
  :type 'boolean)

(defcustom codepad-async t
  "Async retreive"
  :group 'codepad
  :type 'boolean)

(defun codepad-read-p (prompt &optional default)
  "reads true (t,y,true,yes) or false (nil,false,no) from the minibuffer"
  (let ((val (downcase (read-string (concat prompt " [default '" (if default "Yes" "No") "']: ")))))
    (cond
      ((string= val "") default)
      ((member val '("t" "y" "true" "yes")) t)
      ((member val '("nil" "f" "n" "false" "no")) nil)
      (t (message (concat "Wrong input '" val "'! Please enter either Yes or No"))
         (codepad-read-p prompt default)))))

(defun codepad-interactive-option (var prompt)
  (case var
    ((ask) (codepad-read-p prompt))
    ((no) nil)
    ((yes) t)
    ((prefix) current-prefix-arg)
    (t var)))

(defun codepad-true-or-false (val)
  (if val
      "True"
      "False"))

(defun codepad-hexify-string (string)
  "like url-hexify-string but space is turned into +"
  (replace-regexp-in-string "%20" "+" (url-hexify-string string)))

;; copied from gist.el
(defun codepad-make-query-string (params)
  "Returns a query string constructed from PARAMS, which should be
a list with elements of the form (KEY . VALUE). KEY and VALUE
should both be strings."
  (mapconcat
   (lambda (param)
     (concat (codepad-hexify-string (car param)) "="
             (codepad-hexify-string (cdr param))))
   params "&"))

(defun codepad-paste-callback (&rest _)
  "callback called by url-retrieve or after a synced retreive"
  (goto-char (point-min))
  (re-search-forward "^[lL]ocation: \\(.*\\)$")
  (message "Paste created: %s" (match-string 1))
  (let ((url (concat +codepad-url+ (match-string 1))))
    (when codepad-view (browse-url url))
    (kill-new url)
    (kill-buffer (current-buffer))
    url))

;;;###autoload
(defun* codepad-paste-region (begin end &optional private (synchronously 'check-custom))
  "paste region to codepad.org"
  (interactive "r")
  (let* ((private (codepad-interactive-option (or private codepad-private) "Private Paste?"))
         (lang (or (cdr (assoc major-mode +codepad-lang+))
                   +codepad-default-lang+))
         (run (codepad-interactive-option codepad-run "Run Paste?"))
         (url-max-redirections 0)
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (codepad-make-query-string 
           `(("submit" . "Submit")
             ("private" . ,(codepad-true-or-false private))
             ("run" . ,(codepad-true-or-false run))
             ("lang" . ,lang)
             ("code" . ,(buffer-substring begin end))))))
    (when (eql synchronously 'check-custom)
        (setq synchronously (not codepad-async)))
    (if synchronously
        (with-current-buffer (url-retrieve-synchronously +codepad-url+)
          (codepad-paste-callback))
        (url-retrieve +codepad-url+ #'codepad-paste-callback))))

;;;###autoload
(defun* codepad-paste-buffer (&optional private (synchronously 'check-custom))
  "paste buffer to codepad.org"
  (interactive)
  (codepad-paste-region (point-min) (point-max) private synchronously))

;;;###autoload
(defun codepad-fetch (id))

(provide 'codepad)
;;; codepad.el ends here
