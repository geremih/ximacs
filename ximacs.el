;;; ximacs.el ---  basic functions to run Xi -*- lexical-binding: t -*-


;; Author: Mihir Rege
;; URL: http://github.com/geremih/ximacs
;; Package-Requires: ((cl-lib "0.5") (magit "1.2.2") )
;; Version: 0.1.0
;; Keywords: xi

;;TODO
;; Add agent to xi-init-agents
;; Make customizable variables defcustom
;;; CODE:


(eval-when-compile (require 'cl-lib))

(defgroup xi nil "Options for Xi-runner")

(defcustom xi-init-agents
  '("InputManager" "chrome-stt" "reminder" "speak")
  "Agents to be run at startup."
  :group 'xi
  :type '(list))


(defcustom xi-directory
  (file-name-as-directory "~/codes/Xi_2.0")
  "Xi directory.  The default Xi structure is assumed."
  :group 'xi
  :type '(directory))


(defcustom xi-delay 1
  "Delay between starting agents."
  :group 'xi
  :type '(number))

(defvar xi-running-agents (make-hash-table :test 'equal)  "Hashtable with keys as agents and values as the corresponding processes.")


(defun xi-reset ()
  "Reset to default variables."
  (interactive)
  (setq xi-running-agents (make-hash-table :test 'equal)))

;;;###autoload
(defun xi-restart-agent (agent)
  "Restart AGENT."
  (interactive
   (list (ido-completing-read "Agent: " (hash-table-keys xi-running-agents))))
  (xi-kill-agent agent)
  (xi-start-agent agent))

;;;###autoload
(defun xi-kill-agent (agent)
  "Kill process for AGENT if it exists."
  (interactive
   (list (ido-completing-read "Agent: " (hash-table-keys xi-running-agents))))
  (let ((agent-process (gethash agent xi-running-agents)))
    (when agent-process
      (delete-process agent-process))))

(defun get-agents-directory ()
  "Return agents directory."
  (concat xi-directory (file-name-as-directory "agents")))

(defun get-agent-directory (agent)
  "Return directory of AGENT."
  (concat (get-agents-directory) (file-name-as-directory agent)))

;;;###autoload
(defun xi-start-agent (agent)
  "Start AGENT if it is not currently running."
  (interactive
   (list (ido-completing-read "Agent: " (directory-files (get-agents-directory) nil nil t))))
  (if (not (gethash agent xi-running-agents))
      ;;TODO: Check if the agent exists
      (let* ((default-directory (get-agent-directory agent))
             (process-connection-type nil)
             (agent-process
              (start-file-process-shell-command
               agent nil (concat "node "  "index.js 2>&1 "  "> ../../logs/" agent ".log"))))
        (puthash agent agent-process xi-running-agents )
        (set-process-sentinel agent-process
                              (lambda (process event)
                                (message "Removing process from hash")
                                (remhash agent xi-running-agents))))))
;;;###autoload
(defun xi-start-core ()
  "Start xi-core."
  (interactive)
  (let* ((default-directory (concat xi-directory (file-name-as-directory "xi-core")))
         (process-connection-type nil)
         (agent-process (start-file-process-shell-command
                         "xi-core" nil (concat "grunt start 2>&1 " "> ../logs/" "xi-core" ".log"))))
    (puthash "xi-core" agent-process xi-running-agents)
    (set-process-sentinel agent-process
                          (lambda (process event)
                            (remhash "xi-core" xi-running-agents)))))

;;;###autoload
(defun xi-show-log (agent)
  "Show logs for AGENT in new buffer.  `auto-revert-tail-mode' is enabled for this buffer."
  (interactive
   (list (ido-completing-read "Agent: " (append ' ("xi-core") (directory-files (get-agents-directory) nil nil t)))))
  (shell-command (concat "tail -f " (concat xi-directory "/logs/" agent ".log") " | bunyan&") (concat agent "-log")))

;;;###autoload
(defun xi-start ()
  "Start all the agents specificied in `xi-init-agents'."
  (interactive)
  (xi-start-core)
  (mapc (lambda (ag)
          (xi-start-agent ag)
          (sleep-for xi-delay))
        xi-init-agents))

;;;###autoload
(defun xi-kill ()
  "Stop all Xi agents."
  (interactive)
  (let ((running-agent-list (hash-table-keys xi-running-agents)))
    (mapc 'xi-kill-agent running-agent-list)))

;;;###autoload
(defun xi-restart ()
  "Restart all the running agents."
  (interactive)
  (let ((running-agent-list (cl-remove-if
                             (lambda (x) (equal x "xi-core"))
                             (hash-table-keys xi-running-agents))))
    (xi-kill)
    (xi-start-core)
    (sleep-for xi-delay)
    (mapc 'xi-start-agent running-agent-list)))

;; agent-list-mode
(defun xi-agent-list-kill-agent ()
  "Kill the agent in `xi-agent-list-mode'."
  (interactive)
  (xi-kill-agent (tabulated-list-get-id))
  (revert-buffer))

(defun xi-agent-list-show-log ()
  "Show logs of the agent in `xi-agent-list-mode'."
  (interactive)
  (xi-show-log (tabulated-list-get-id))
  (revert-buffer))

(defun xi-agent-list-restart-agent ()
  "Restart the agent in `xi-agent-list-mode'."
  (interactive)
  (message "Restarting agent")
  (xi-restart-agent (tabulated-list-get-id))
  (revert-buffer))

(defvar xi-agent-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k" 'xi-agent-list-kill-agent)
    (define-key map "l" 'xi-agent-list-show-log)
    (define-key map "r" 'xi-agent-list-restart-agent)
    map))

(defvar tabulated-list-format)
(defvar tabulated-list-entries)
(defvar tabulated-list-format)
(define-derived-mode xi-agent-list-mode tabulated-list-mode "Xi Agents"
  "Major mode for listing agents in Xi"
  (setq tabulated-list-format [("Agent" 15 t)])
  (add-hook 'tabulated-list-revert-hook  'xi-list-agents--refresh nil t)
  (tabulated-list-init-header))

(defun xi-list-agents--refresh ()
  "Recompute list."
  (setq tabulated-list-entries nil)
  (dolist (a (hash-table-keys xi-running-agents))
    (push (list a (vector a)) tabulated-list-entries)))

;;;###autoload
(defun xi-list-running-agents ()
  "List all running agents."
  (interactive)
  (let ((buffer (get-buffer-create "*Running Agents*")))
    (with-current-buffer buffer
      (xi-agent-list-mode)
      (xi-list-agents--refresh)
      (tabulated-list-print))
    (display-buffer buffer)))


(defun xi-remote-in-syncp ()
  "Pull tracked branch and check if it in is in sync with remote, return t if in sync else nil."
  (let ((remote (magit-get-current-remote)))
    (message default-directory)
    (if remote
        (magit-run-git "fetch" remote)))
  (let ((tracked (magit-get-tracked-branch nil t)))
    (if (and tracked
             (not (equal (magit-git-string "rev-parse" "HEAD")
                         (magit-git-string "rev-parse" tracked))))
        nil
      t)))

;;;###autoload
(defun xi-check-sync ()
  "Check if xi-core, xal and agents are in sync with remotes.  If they are not, report defaulters."
  (interactive)
  (let ((directories (append (list (concat xi-directory "xi-core")
                                   (concat xi-directory "xal-javascript"))
                             (mapcar (lambda (d)
                                       (file-name-as-directory (concat (get-agents-directory) d)))
                                     (cl-remove-if (lambda (d)
                                                  (or (equal d "..")
                                                      (equal d ".")))
                                                (directory-files (get-agents-directory) nil nil t)))))
        (non-sync-dirs '()))
    (dolist (directory directories)
      (let ((default-directory directory))
        (unless (xi-remote-in-syncp)
          (push directory non-sync-dirs))))
    (if non-sync-dirs
        (let ((buffer (get-buffer-create "*Xi Sync*")))
          (with-current-buffer buffer
            (delete-region (point-min) (point-max))
            (insert "Unsynced repos exist:\n")
            (dolist (repo non-sync-dirs)
              (insert (concat repo "\n")))
            (display-buffer buffer)))
      (message "Xi is in sync"))))


(provide 'xiemacs)
;;; ximacs.el ends here
