(require 'url)
(require 'json)

(defconst OPENAI-DALLE-API-URL "https://api.openai.com/v1/images/generations")
(defconst OPENAI-DALLE-TOKEN "")
(defconst PEXELS-URL "https://api.pexels.com/v1/search")
(defconst PEXELS-TOKEN "")
(defconst IMAGE-COUNT 5)

(defun get-dalle-img-get-request-args-list (params)
  (concat "?"
	  (combine-and-quote-strings
	   (mapcar (lambda(x) (concat (substring (symbol-name (car x)) 1) "=" (cadr x))) (seq-partition params 2))
	   "&")))

(defun get-dalle-img-send-json-request (url method params token)
  "Send a JSON request to the specified URL with bearer token."
  (let* ((url-request-method method)
         (url-request-extra-headers `(("Authorization" . ,token)
                                      ("Content-Type" . "application/json")))
         (url-request-data (json-encode params))
	 (url (if (equal method "GET")
		  (concat url "/" (get-dalle-img-get-request-args-list params))
		url))
         (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun get-dalle-img-request-dalle-images (prompt n)
  (let* ((params (list
		  :prompt prompt
		  :n n))
	 
	 (json-response (get-dalle-img-send-json-request OPENAI-DALLE-API-URL "POST" params (concat "Bearer " OPENAI-DALLE-TOKEN)))
	 (url-list (cdr (assoc 'data json-response))))
    url-list))

(defun get-dalle-img-request-pexels-images (prompt n)
  (let* ((params (list
		   :query prompt
		   :per_page (number-to-string n)
		   :page "1"))
	 (json-response (get-dalle-img-send-json-request PEXELS-URL "GET" params PEXELS-TOKEN))
	 (url-list (cdr (assoc 'photos  json-response))))
    (vconcat (mapcar (lambda (photo)
	  (list (cons 'url (cdr (assoc 'large (cdr (assoc 'src photo)))))))
	  url-list))))

(defun get-dalle-img-insert-image-to-choose-image-menu (&optional url i)
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
         (let ((data (with-current-buffer buffer
                       (goto-char (point-min))
                       (search-forward "\n\n")
                       (buffer-substring (point) (point-max)))))
	   (switch-to-buffer "*dalle_output_buffer*")
	   (insert (number-to-string i))
	   (insert ".")
           (insert-image (create-image data nil t :scale 0.1))
	   (insert "\n"))
      (kill-buffer buffer))))

(defun get-dalle-img-choose-image-from-menu (image-url-list)
  (let* ((index 1))
  (switch-to-buffer "*dalle_output_buffer*")
  (erase-buffer)
  (mapcar (lambda (image-url)
	    (progn
	      (get-dalle-img-insert-image-to-choose-image-menu (cdr (assoc 'url image-url)) index)
	      (setq index (1+ index))))
	  image-url-list)
  (setq chosen-number (read-number "Choose which image to use (enter a number): "))
  (kill-buffer "*dalle_output_buffer*")
  (cdr (assoc 'url (aref image-url-list (1- chosen-number))))))


(defun generate-featured-blog-image-dalle-helper (prompt n)
  (let ((images (get-dalle-img-request-dalle-images prompt n)))
    (get-dalle-img-choose-image-from-menu images)))

(defun generate-featured-blog-image-dalle ()
  (interactive)
  (let ((prompt (read-string "Enter a prompt: ")))
    (generate-featured-blog-image-dalle-helper prompt IMAGE-COUNT)))

(defun generate-featured-blog-image-pexels-helper (prompt n)
  (let ((images (get-dalle-img-request-pexels-images prompt n)))
    (get-dalle-img-choose-image-from-menu images)))

(defun generate-featured-blog-image-pexels ()
  (interactive)
  (let ((prompt (read-string "Enter a prompt: ")))
    (generate-featured-blog-image-pexels-helper prompt IMAGE-COUNT)))
