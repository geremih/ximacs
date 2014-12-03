;;; ximacs.el ---  basic functions to run Xi   -*- lexical-binding: t -*-

;;; Commentary:

;;TODO
;; Autocomplete for agents
;; Add agent to xi-init-agents

;;; CODE:

(setq lexical-binding t)
(eval-when-compile (require 'cl-lib))
(defvar xi-init-agents '("InputManager" "chrome-stt" "reminder" "speak")
  "Agents to be run at startup.")

(defvar xi-directory (file-name-as-directory "~/codes/Xi_2.0")
  "Xi directory.  The default Xi structure is assumed.")

(defvar xi-running-agents (make-hash-table :test 'equal)
  "Hashtable with keys as agents and values as the corresponding processes.")

(defvar xi-delay 1
  "Delay between starting agents.")

(defun xi-reset ()
  "Reset to default variables."
  (interactive)
  (setq xi-running-agents (make-hash-table :test 'equal)))

(defun xi-restart-agent (agent)
  "Restart AGENT."
  (interactive "sAgent: ")
  (xi-kill-agent agent)
  (xi-start-agent agent))

(defun xi-kill-agent (agent)
  "Kill process for AGENT if it exists."
  (interactive "sAgent: ")
  (let ((agent-process (gethash agent xi-running-agents)))
    (when agent-process
      (delete-process agent-process))))

(defun get-agent-directory (agent)
  "Return directory of AGENT."
  (concat xi-directory (file-name-as-directory "agents") (file-name-as-directory agent)))

(defun xi-start-agent (agent)
  "Start AGENT if it is not currently running."
  (interactive "sAgent: ")
  (if (not (gethash agent xi-running-agents))
      ;;TODO: Check if the agent exists
      (let* ((default-directory (get-agent-directory agent))
             (agent-process
              (start-file-process-shell-command
               agent nil (concat "node "  "index.js"  "> ../../logs/" agent ".log"))))
        (puthash agent agent-process xi-running-agents )
        (set-process-sentinel agent-process
                              (lambda (process event)
                                (message "Removing process from hash")
                                (remhash agent xi-running-agents))))))

(defun xi-start-core ()
  "Start xi-core."
  (interactive)
  (let* ((default-directory (concat xi-directory (file-name-as-directory "xi-core")))
         (agent-process (start-file-process-shell-command 
                         "xi-core" nil (concat "grunt start " "> ../logs/" "xi-core" ".log"))))
    (puthash "xi-core" agent-process xi-running-agents )
    (set-process-sentinel agent-process
                          (lambda (process event)
                            (remhash "xi-core" xi-running-agents)))))

(defun xi-show-log (agent)
  "Show logs for AGENT in new buffer.  `auto-revert-tail-mode' is enabled for this buffer."
  (interactive "sAgent: ")
  (shell-command (concat "tail -f " (concat xi-directory "/logs/" agent ".log") " | bunyan&") (concat agent "-log")))

(defun xi-start ()
  "Start all the agents specificied in `xi-init-agents'."
  (interactive)
  (xi-start-core)
  (mapc (lambda (ag)
          (xi-start-agent ag)
          (sleep-for xi-delay))
        xi-init-agents))

(defun xi-stop ()
  "Stop all Xi agents."
  (interactive)
  (let ((running-agent-list (hash-table-keys xi-running-agents)))
    (mapc 'xi-kill-agent running-agent-list)))

(defun xi-restart ()
  "Restart all the running agents."
  (interactive)
  (let ((running-agent-list (remove-if
                             (lambda (x) (equal x "xi-core"))
                             (hash-table-keys xi-running-agents))))
    (xi-stop)
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

(defvar xi-agent-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k" 'xi-agent-list-kill-agent)
    (define-key map "l" 'xi-agent-list-show-log)
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

(defun xi-list-running-agents ()
  "List all running agents."
  (interactive)
  (let ((buffer (get-buffer-create "*Running Agents*")))
    (with-current-buffer buffer
      (xi-agent-list-mode)
      (xi-list-agents--refresh)
      (tabulated-list-print))
    (display-buffer buffer)))

(shell-command "tail -f ~/codes/Xi_2.0/logs/reminder.log | bunyan&" "tail")

(provide 'xiemacs)
;;; ximacs.el ends here
