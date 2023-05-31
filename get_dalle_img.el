(require 'url)
(require 'json)

(defconst OPENAI-DALLE-API-URL "https://api.openai.com/v1/images/generations")
(defconst OPENAI-DALLE-TOKEN "")

(defun send-json-request (url method params token)
  "Send a JSON request to the specified URL with bearer token."
  (let* ((url-request-method method)
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " token))
                                      ("Content-Type" . "application/json")))
         (url-request-data (json-encode params))
         (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (json-read))))

(defun request-dalle-images (prompt n)
  (let* ((params (list
		  :prompt prompt
		  :n n))
	 
	 (json-response (send-json-request OPENAI-DALLE-API-URL "POST" params OPENAI-DALLE-TOKEN))
	 (url-list (cdr (assoc 'data  json-response))))
    url-list))

(defun insert-image-to-choose-image-menu (&optional url i)
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
	   (insert (number-to-string index))
	   (insert ".")
           (insert-image (create-image data nil t :scale 0.1))
	   (insert "\n"))
      (kill-buffer buffer))))

(defun choose-image-from-menu (image-url-list)
  (let* ((index 1))
  (switch-to-buffer "*dalle_output_buffer*")
  (erase-buffer)
  (mapcar (lambda (image-url)
	    (progn
	      (insert-image-to-choose-image-menu (cdr (assoc 'url image-url)) index)
	      (setq index (1+ index))))
	  image-url-list)
  (setq chosen-number (read-number "Choose which image to use (enter a number): "))
  (kill-buffer "*dalle_output_buffer*")
  (cdr (assoc 'url (aref image-url-list (1- chosen-number))))))


(defun generate-featured-blog-image (prompt n)
  (interactive)
  (let ((images (request-dalle-images prompt n)))
    (choose-image-from-menu images)))

