;;; nostr-publish.el --- Publish Markdown to Nostr via NIP-23 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Rother

;; Author: Markus Rother <mail@markusrother.de>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: nostr, publishing, markdown
;; URL: https://github.com/941design/emacs-nostr-publish

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Thin Emacs wrapper around nostr-publish CLI tool.
;; Provides C-c C-p keybinding to publish current Markdown buffer to Nostr.

;;; Code:

(defgroup nostr-publish nil
  "Publish Markdown to Nostr via NIP-23."
  :group 'applications
  :prefix "nostr-publish-")

(defcustom nostr-publish-cli-command "nostr-publish"
  "Path to nostr-publish CLI executable."
  :type 'string
  :group 'nostr-publish)

(defcustom nostr-publish-bunker-uri nil
  "Default NIP-46 bunker URI for signing.
If nil, must be provided at publish time or via CLI config."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Bunker URI"))
  :group 'nostr-publish)

(defcustom nostr-publish-default-relays nil
  "Relay URLs for publishing (required).
List of wss:// or ws:// URLs. These serve as both the allowlist
and default relays per spec section 7. Frontmatter relays must
be a subset of this list."
  :type '(repeat string)
  :group 'nostr-publish)

(defcustom nostr-publish-timeout 30
  "Timeout in seconds for publish operation."
  :type 'integer
  :group 'nostr-publish)

(defun nostr-publish--check-cli ()
  "Check if nostr-publish CLI is available, warn if not."
  (unless (executable-find nostr-publish-cli-command)
    (display-warning 'nostr-publish
                     (format "CLI '%s' not found. Install with: pipx install nostr-publish"
                             nostr-publish-cli-command)
                     :warning)))

;; Check CLI availability on load
(nostr-publish--check-cli)

;;;###autoload
(defun nostr-publish-buffer ()
  "Publish current Markdown buffer to Nostr via NIP-23.

CONTRACT:
  Inputs:
    - Current buffer content (Markdown with frontmatter)
    - User customization variables (bunker URI, relays, timeout)

  Outputs:
    - Success message displayed in echo area with event ID and pubkey
    - Error message displayed in echo area on failure

  Invariants:
    - Buffer must be saved before publishing (auto-save if modified)
    - Buffer file must exist (not unsaved buffer)
    - Invokes CLI synchronously, blocks until completion
    - Timeout enforced per nostr-publish-timeout
    - Exit code 0 = success, non-zero = failure

  Properties:
    - Synchronous: blocks Emacs until publish completes or times out
    - User-visible: all results/errors shown in echo area
    - File-based: operates on saved file, not buffer content

  Algorithm:
    1. Check buffer has associated file:
       a. If buffer-file-name is nil, error: \"Buffer not associated with file\"
    2. Check relays are configured:
       a. If nostr-publish-default-relays is nil/empty,
          error: \"No relays configured\"
    3. Save buffer if modified:
       a. If buffer-modified-p, call save-buffer
    4. Build CLI command arguments:
       a. Start with nostr-publish-cli-command
       b. Add buffer-file-name
       c. If nostr-publish-bunker-uri set, add \"--bunker URI\"
       d. For each relay in nostr-publish-default-relays, add \"--relay URL\"
       e. Add \"--timeout SECONDS\"
    5. Invoke CLI synchronously:
       a. Call shell-command or make-process with arguments
       b. Capture stdout and stderr
       c. Wait for process completion with timeout
    6. Handle result:
       a. If exit code 0:
          - Parse stdout for event ID and pubkey
          - Display success message: \"Published: event ID <id>, pubkey <pubkey>\"
       b. If exit code non-zero:
          - Parse stderr for error message
          - Display error in echo area: \"Publish failed: <error>\"
       c. If timeout:
          - Display: \"Publish timed out after N seconds\"

  Raises:
    - error: If buffer not associated with file
    - error: If no relays configured
    - Does not raise for publish failures (displays message instead)"
  (interactive)
  (let ((file (buffer-file-name)))
    ;; Step 1: Validate buffer has associated file
    (unless file
      (error "Buffer not associated with file"))

    ;; Step 2: Validate relays are configured (required)
    (unless nostr-publish-default-relays
      (error "No relays configured. Set nostr-publish-default-relays"))
    (dolist (relay nostr-publish-default-relays)
      (unless (string-match-p "\\`wss?://" relay)
        (error "Invalid relay URL '%s': must use ws:// or wss://" relay)))

    ;; Step 3: Save buffer if modified
    (when (buffer-modified-p)
      (save-buffer))

    ;; Step 4: Build CLI command arguments
    (let ((args (list nostr-publish-cli-command file)))
      (when nostr-publish-bunker-uri
        (setq args (append args (list "--bunker" nostr-publish-bunker-uri))))
      (dolist (relay nostr-publish-default-relays)
        (setq args (append args (list "--relay" relay))))
      (setq args (append args (list "--timeout" (number-to-string nostr-publish-timeout))))

      ;; Step 4 & 5: Invoke CLI and handle result
      (nostr-publish--invoke-cli args))))

(defun nostr-publish--invoke-cli (args)
  "Invoke CLI with ARGS and handle result.
ARGS is a list where first element is command, rest are arguments.
Captures stdout/stderr and displays appropriate message."
  (let ((temp-stdout-file (make-temp-file "nostr-publish-stdout-"))
        (temp-stderr-file (make-temp-file "nostr-publish-stderr-"))
        (exit-code nil)
        (stdout nil)
        (stderr nil))
    (unwind-protect
        (progn
          ;; Invoke CLI synchronously
          (setq exit-code
                (apply #'call-process
                       (car args)
                       nil
                       (list (list :file temp-stdout-file) temp-stderr-file)
                       nil
                       (cdr args)))

          ;; Capture output
          (setq stdout (with-temp-buffer
                         (insert-file-contents temp-stdout-file)
                         (buffer-string)))
          (setq stderr (with-temp-buffer
                         (insert-file-contents temp-stderr-file)
                         (buffer-string)))

          ;; Handle result based on exit code
          (cond
           ((eq exit-code 0)
            ;; Success: parse event ID and pubkey from stdout
            (let ((event-id (nostr-publish--extract-event-id stdout))
                  (pubkey (nostr-publish--extract-pubkey stdout)))
              (cond
               ((and event-id pubkey)
                (message "Published: event ID %s, pubkey %s" event-id pubkey))
               (event-id
                (message "Published: event ID %s" event-id))
               (t
                (message "Published successfully (event ID not found in output)")))))
           (t
            ;; Failure: display error from stderr
            (let ((error-msg (string-trim (or stderr "Unknown error"))))
              (message "Publish failed: %s" error-msg)))))
      ;; Cleanup temp files
      (ignore-errors (delete-file temp-stdout-file))
      (ignore-errors (delete-file temp-stderr-file)))))

(defun nostr-publish--extract-event-id (output)
  "Extract event ID from CLI output.
Returns the event ID if found, nil otherwise.
Looks for patterns like \"Published: <hex>\", \"event ID: <hex>\", or \"ID: <hex>\"."
  (cond
   ;; Match "Published:" followed by hex event ID (primary CLI output format)
   ((string-match (rx "Published:" (+ space) (group (+ hex-digit)))
                  output)
    (match-string 1 output))
   ;; Match "event id:" or "event ID:" (case insensitive)
   ((string-match (rx "event" (+ space) (or "id" "ID" "Id" "iD")
                      ":" (+ space)
                      (group (+ hex-digit)))
                  output)
    (match-string 1 output))
   ;; Match standalone "ID:" or "id:" (case insensitive)
   ((string-match (rx (or "ID" "id" "Id" "iD") ":" (+ space)
                      (group (+ hex-digit)))
                  output)
    (match-string 1 output))
   (t nil)))

(defun nostr-publish--extract-pubkey (output)
  "Extract pubkey from CLI output.
Returns the pubkey if found, nil otherwise.
Looks for pattern \"Pubkey: <hex>\"."
  (when (string-match (rx "Pubkey:" (+ space) (group (+ hex-digit)))
                      output)
    (match-string 1 output)))

;;;###autoload
(define-minor-mode nostr-publish-mode
  "Minor mode for Markdown buffers with nostr-publish keybindings."
  :lighter " Nostr"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-p") #'nostr-publish-buffer)
            map))

(provide 'nostr-publish)
;;; nostr-publish.el ends here
