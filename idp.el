;;; idp.el --- Login.gov Emacs Development Helpers -*- lexical-binding: t; -*-

;; Author: Eric Gade <eric.gade@gsa.gov>
;; Version: 1.0
;; Package-Requires ((projectile "2.7.0") (dash "2.19.1"))


;;; Commentary:

;; This package provides additional helpful
;; interactive functions and projectile settings
;; when developing the Login.gov identity-idp
;; application

;;; Code:

(defun idp-get-local-ip ()
  "Return the string representation of the local ip address."
  (let* ((get-ip-command "ipconfig getifaddr en0")
         (command-result (shell-command-to-string get-ip-command)))
    (string-trim command-result)))

(defun idp/get-default-application-yml ()
  (string-replace "<your-local-ip>" (idp-get-local-ip) "development:
  config_key:
  #domain_name: <your-local-ip>:3000
  #mailer_domain_name: <your-local-ip>:3000"))

;; directory local variables that are useful for idp
;; development, such as indentation levels.
;; Will be used to generate a .dir-locals.el
;; file in the root of the identity-idp repo
(defvar idp/dir-locals
  (prin1-to-string
   '((js2-mode . ((js2-basic-offset . 2)))
     (typescriptreact-mode . ((typescript-indent-level . 2))))))

(defvar idp/projectile-contents
      (string-join
       '("-/node_modules/"
         "-/nodeenv/"
         "-/public/acuant/"
         "-/public/packs/"
         "-/app/assets/builds/"
         "-/tmp/"
         "-/**/node_modules/"
         "-node_modules/*"
         "-/log/")
       "\n")
      "Directories and files that will be used to generate .projectile file.
This file helps projectile ignore irrelevant directories during greps and other
complex searches")

;;;###autoload;
(defun setup-idp ()
  "Setup the config/application.yml and projectile files.
To be used on a fresh clone of the idp repo"
  (interactive)
  (if (and (projectile-project-root) (string-equal (projectile-project-name) "identity-idp"))
      (let ((projectile-filename (concat (projectile-project-root) ".projectile"))
            (application-yml-filename (concat (projectile-project-root) "config/application.yml"))
            (dir-locals-filename (concat (projectile-project-root) ".dir-locals.el")))
        (with-temp-file
            projectile-filename
          (insert idp/projectile-contents))
        (with-temp-file application-yml-filename
          (insert (idp/get-default-application-yml)))
        (with-temp-file dir-locals-filename
          (insert idp/dir-locals)))))


(defun idp-enable-https ()
  "Set the application.yml file for local HTTPS development"
  (interactive)
  (let ((application-yml-filename (concat (projectile-project-root) "config/application.yml")))
    (with-temp-buffer
      (find-file application-yml-filename)
      (goto-char (point-min))
      (perform-replace "  #domain_name:" "  domain_name:" nil nil nil)
      (perform-replace "  #mailer_domain_name:" "  mailer_domain_name:" nil nil nil)
      (save-buffer)
      (kill-buffer))))

(defun idp-disable-https ()
  "Set the application.yml file for local HTTP development"
  (interactive)
  (let ((application-yml-filename (concat (projectile-project-root) "config/application.yml")))
    (with-temp-buffer
      (find-file application-yml-filename)
      (goto-char (point-min))
      (perform-replace "  domain_name:" "  #domain_name:" nil nil nil)
      (perform-replace "  mailer_domain_name:" "  #mailer_domain_name:" nil nil nil)
      (save-buffer)
      (kill-buffer))))

(defun idp-mocha-this-file ()
  "Run Mocha on the current test file.
and open in a compilation buffer"
  (interactive)
  (compile
   (concat "cd " (projectile-project-root) " && npx mocha " (idp-project-filename))
   t))

(defun idp-project-filename ()
  (if buffer-file-name
      (substring
       buffer-file-name
       (length (projectile-project-root))
       nil)
    ""))

(defun idp-get-matching-project-files-list (&rest terms)
  "Return a list of current Projectile files that contain TERMS.
Will search in the path or filename."
  (message (prin1-to-string terms))
  (let* ((project-root (projectile-acquire-root))
         (files (projectile-project-files project-root)))
    (seq-filter
     (lambda (filename)
       (-all-p (lambda (term) (string-match-p (regexp-quote term) filename)) terms))
     files)))

(defun idp-run-specs-matching (&rest terms)
  "Run all IDP specs whose filenames or paths contain the TERMS.
Will run the tests in a comint-enabled command-mode buffer"
  (interactive)
  (let* ((terms (if (interactive-p)
                   (split-string
                    (read-string "Terms: ") " ")
                 terms))
         (files (progn
                  (push "_spec" terms)
                  (apply 'idp-get-matching-project-files-list terms)))
         (cmd-str (concat "rspec " (string-join files " "))))
    
    (compile cmd-str t)))


;; Define a custom Projectile project type for the idp
(projectile-register-project-type
 'identity-idp
 '("Gemfile" "Procfile" "config" "db" "deploy" "certs" "certs.example" "pwned_passwords")
 :project-file "Makefile"
 :compile "make setup"
 :run "make run")

(defun idp-switched-to-project-hook ()
  "Function that is called when Projectile switches to an identity-idp project"
  (if (eq (projectile-project-type) 'identity-idp)
      (message "Switched to identity-idp project!")))

(add-hook 'projectile-after-switch-project-hook 'idp-switched-to-project-hook)
(add-hook 'projectile-mode-hook 'idp-switched-to-project-hook)


;;-------------------------------------------------------
;; General Helpers
;;-------------------------------------------------------
(defun idp-project-type-p ()
  "Responds true if the current projectile project type is identity-idp."
  (eq (projectile-project-type) 'identity-idp))

(defun idp-current-file-is-spec-p ()
  "Responds true if the current buffer file is a test file of some kind."
  (let ((filename (buffer-file-name))
        (term (regexp-quote "spec")))
    (string-match-p term filename)))

(defun idp--arglist-has-partial-arg (args name)
  (-any-p
   (lambda (arg)
     (string-match-p (regexp-quote name) arg))
   args))

(defun idp--transient-get-arg-value (args name)
  (let* ((matched-item (-reduce-from
                        (lambda
                          (prev-arg current-arg)
                          (if prev-arg
                              prev-arg
                            (if (string-match-p (regexp-quote name) current-arg)
                                current-arg)))
                        nil
                        args)))
    (if matched-item
        (car (last (string-split matched-item (regexp-quote "=")))))))
;;-------------------------------------------------------
;; Custom Transient Functions
;; ------------------------------------------------------
(defun idp--run-all-frontend-tests ()
  (interactive)
  (let ((test-cmd "npx mocha"))
    (compile test-cmd t)))

(transient-define-suffix idp--run-current-spec-file-suffix ()
  "Wrapper for running the current spec file, used with args
by Transient."
  (interactive)
  (if (idp-current-file-is-spec-p)
      (let* ((args (transient-args (oref transient-current-prefix command)))
         (show-browser
          (if (member "--show-browser" args)
              "SHOW_BROWSER=true "
            ""))
         (line-number (idp--transient-get-arg-value args "--line-number"))
         (filename (idp-project-filename))
         (suffix (file-name-extension filename))
         (is-javascript (if (member suffix '("js" "ts" "jsx" "tsx"))))
         (base-test-cmd (if is-javascript "npx mocha" "bundle exec rspec"))
         (test-cmd (if line-number (concat base-test-cmd ":" line-number) base-test-cmd))
         (default-directory (projectile-project-root)))

        (if is-javascript
            (compile test-cmd t)
          (compile (concat show-browser test-cmd t))))
    (error (concat "The file '" (idp-project-filename) "' does not appear to be a test file."))))

(transient-define-prefix idp-main-transient ()
  "Main transient menu for IDP actions."
  ["Login.gov IDP Actions\n"
   ("g" "Grep within this IDP project" projectile-grep)
   ("t" "Run test commands within this IDP project" idp-test-transient)])

(transient-define-prefix idp-test-transient ()
  "Main transiene menu for running IDP tests."
  ["Login.gov IDP Testing Actions\n"
   ["Commands"
    ("f" "Run this spec file" idp--run-current-spec-file-suffix)
    ("F" "Run frontend tests" idp--run-all-frontend-tests)
    ]
   ["Arguments"
    ("-s" "Run with SHOW_BROWSER as true" "--show-browser")
    ("-l" "Line number" "--line-number=")]
   ]
  :transient transient--do-exit)

(provide 'idp)
;;; idp.el ends here
