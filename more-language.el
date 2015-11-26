;;; Custom online request for grammar checking.

(defvar --lang-req "https://languagetool.org:8081/?language=en-US&text=%s")
(defvar --lang-req-de "https://languagetool.org:8081/?language=de-DE&text=%s")

(defun --my-lang-online-request (str)
  "Request for checking with given argument `str'.
Returns a string as parsed xml as a list. "
  (let* ((enc (url-encode-url (format --lang-req str)))
	 (buf1 (url-retrieve-synchronously enc)))
    (with-current-buffer buf1
      (goto-char url-http-end-of-headers)
      (xml-parse-region (point) (point-max)))))

(defun my-lang-online-check-errors (str)
  "Return a list of error information objects as
a list of lists. "
  (let* ((x (--my-lang-online-request str))
	 (ms (assoc 'matches x)))
    (if ms
	(let ((errs nil))
	  (dolist (e ms)
	    (when (and (listp e) (equal (car e) 'error))
	      (push (cadr e) errs)))
	  errs)
      (error "Failed request. "))))

(defface my-check-error-face
  '((t (:foreground "black" :background "pink")))
  "Face for errored words. ")

(defun --my-lang-online-check-overlays (str beg end)
  (save-excursion
    (let* ((errs (my-lang-online-check-errors str))
	   (ovls nil))
      (dolist (e errs ovls)
	;; Use `offset' other than `fromx', `tox' to refer positions. 
	(let* ((obeg (+ beg (string-to-int (cdr (assoc 'offset e)))))
	       (oend (+ obeg (string-to-int (cdr (assoc 'errorlength e)))))
	       (o (make-overlay obeg oend))
	       (msg (cdr (assoc 'msg e)))
	       (rps (replace-regexp-in-string "#" "\n* " (cdr (assoc 'replacements e))))
	       (hint (concat msg (if (zerop (length rps))
				     nil
				   (concat "\nReplacements: \n* " rps)))))
	  (overlay-put o 'help-echo hint)
	  (push o ovls))))))

(defun my-lang-online-check-region (begin end)
  (interactive "r")
  (let* ((str (buffer-substring-no-properties begin end))
	 (ovls (--my-lang-online-check-overlays str begin end))
	 (msgs nil))
    (dolist (o ovls)
      (overlay-put o 'face 'my-check-error-face))))

(defun my-lang-online-check-clear ()
  (interactive)
  (remove-overlays))
