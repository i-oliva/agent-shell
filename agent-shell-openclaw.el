;;; agent-shell-openclaw.el --- OpenClaw agent configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file includes OpenClaw-specific configuration.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'acp)

(declare-function agent-shell--dwim "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(defvar agent-shell-context-sources)

(defcustom agent-shell-openclaw-acp-command
  '("openclaw" "acp")
  "Command and parameters for the OpenClaw ACP client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-openclaw-environment
  nil
  "Environment variables for the OpenClaw ACP client.

This should be a list of environment variables to be used when
starting the OpenClaw ACP client process.  Use this for Gateway
authentication variables or proxy settings when needed.

Example usage to set custom environment variables:

  (setq agent-shell-openclaw-environment
        (agent-shell-make-environment-variables
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-openclaw-make-agent-config ()
  "Create an OpenClaw agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'openclaw
   :mode-line-name "OpenClaw"
   :buffer-name "OpenClaw"
   :shell-prompt "OpenClaw> "
   :shell-prompt-regexp "OpenClaw> "
   ;; OpenClaw exposes thinking levels through both legacy ACP session modes
   ;; and the richer thought_level config option.  Model changes refresh the
   ;; config option, while the legacy mode list can remain stale.
   :session-mode-config-category "thought_level"
   ;; OpenClaw ACP bridge sessions historically do not persist cwd metadata in
   ;; the Gateway session row.  If agent-shell sends a cwd filter, older
   ;; OpenClaw sessions disappear from the resume picker; list all OpenClaw
   ;; sessions instead and let the user choose by title/date/id.
   :session-list-filter-by-cwd nil
   :client-maker (lambda (buffer)
                   (agent-shell-openclaw-make-client :buffer buffer))
   :install-instructions "Install OpenClaw and configure an OpenClaw Gateway; see https://docs.openclaw.ai/start/wizard and https://docs.openclaw.ai/cli/acp."))

(defun agent-shell-openclaw-start-agent ()
  "Start an interactive OpenClaw agent shell."
  (interactive)
  ;; OpenClaw receives prompts through a Gateway and can faithfully answer any
  ;; text sent as part of the ACP prompt.  Avoid the most surprising DWIM source
  ;; here: the current line.  In Doom/which-key workflows, leader-key popups can
  ;; briefly make UI hint text look like the current line, which then pollutes
  ;; the first OpenClaw prompt.  Keep this override OpenClaw-specific:
  ;; the observed integration issue was OpenClaw's prompt echo/current-line
  ;; interaction, and any shared DWIM policy can be revisited separately.
  (let ((agent-shell-context-sources '(files region error)))
    (agent-shell--dwim :config (agent-shell-openclaw-make-agent-config)
                       :new-shell t)))

(cl-defun agent-shell-openclaw-make-client (&key buffer)
  "Create an OpenClaw ACP client using BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-openclaw-acp-command)
                                :command-params (cdr agent-shell-openclaw-acp-command)
                                :environment-variables agent-shell-openclaw-environment
                                :context-buffer buffer))

(provide 'agent-shell-openclaw)

;;; agent-shell-openclaw.el ends here
