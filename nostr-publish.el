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

(defcustom nostr-publish-blossom-url nil
  "Blossom server HTTP base URL for cover image uploads.
If nil and frontmatter has image.file, an error will occur."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Blossom URL"))
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
      (when nostr-publish-blossom-url
        (setq args (append args (list "--blossom" nostr-publish-blossom-url))))

      ;; Step 4 & 5: Invoke CLI and handle result
      (nostr-publish--invoke-cli args))))

(defun nostr-publish--invoke-cli (args)
  "Invoke CLI with ARGS and handle result.
ARGS is a list where first element is command, rest are arguments.
Captures stdout/stderr and displays appropriate message."
  (let ((temp-stdout-file (make-temp-file "nostr-publish-stdout-"))
        (temp-stderr-file (make-temp-file "nostr-publish-stderr-"))
        (target-buffer (current-buffer))  ;; Capture buffer at invocation
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
            ;; Parse JSON output to extract all fields
            (condition-case _
                (let* ((result (json-parse-string stdout :object-type 'alist))
                       (event-id (alist-get 'event_id result))
                       (pubkey (alist-get 'pubkey result))
                       (naddr (alist-get 'naddr result))
                       (image (alist-get 'image result)))

                  ;; Update image metadata if present
                  (when image
                    (unless (eq (current-buffer) target-buffer)
                      (error "Buffer changed during publish operation"))
                    (with-current-buffer target-buffer
                      (nostr-publish--update-cover-frontmatter image)))

                  ;; Update naddr if present
                  (when naddr
                    (unless (eq (current-buffer) target-buffer)
                      (error "Buffer changed during publish operation"))
                    (with-current-buffer target-buffer
                      (nostr-publish--update-naddr-frontmatter naddr)))

                  ;; Display success message
                  (message "Published: event ID %s, pubkey %s%s"
                           event-id pubkey
                           (if naddr (format ", naddr %s" naddr) "")))

              ;; If JSON parsing fails, display message without metadata
              (error
               (message "Published successfully (could not parse JSON output)"))))
           (t
            ;; Failure: display error from stderr
            (let ((error-msg (string-trim (or stderr "Unknown error"))))
              (message "Publish failed: %s" error-msg)))))
      ;; Cleanup temp files
      (ignore-errors (delete-file temp-stdout-file))
      (ignore-errors (delete-file temp-stderr-file)))))

(defun nostr-publish--sanitize-yaml-value (value)
  "Sanitize VALUE for safe insertion into YAML frontmatter.
Removes newlines and YAML structural characters to prevent injection.
Defense-in-depth: Python validation already prevents these, but this
provides additional safety layer."
  (when value
    (let ((sanitized value))
      ;; Remove newlines (both Unix and Windows style)
      (setq sanitized (replace-regexp-in-string "[\n\r]" "" sanitized))
      ;; Remove YAML structural characters that could break parsing
      (setq sanitized (replace-regexp-in-string "[][{}]" "" sanitized))
      sanitized)))

(defun nostr-publish--update-naddr-frontmatter (naddr)
  "Update current buffer's YAML frontmatter with naddr field.
NADDR is NIP-19 encoded string (e.g., \"naddr1qqxnzd3cxsmnjv3hx56rjwf3...\").

Updates or inserts \"naddr:\" field in frontmatter at top-level.
Preserves all other frontmatter fields.
Marks buffer as modified and saves it.

Signals error if frontmatter delimiters not found.

Atomic guarantee: Either all changes are applied and saved, or all
changes are rolled back on error (via undo mechanism).

CONTRACT:
  Inputs:
    - naddr: string, NIP-19 encoded naddr value

  Outputs:
    - None (updates buffer in-place)

  Invariants:
    - Updates current buffer's YAML frontmatter
    - Adds or updates \"naddr\" field at frontmatter top-level
    - Preserves all other frontmatter fields
    - Saves buffer after update
    - Atomic: either all changes applied and saved, or all rolled back

  Properties:
    - Idempotent: updating with same naddr multiple times produces same result
    - Fail-safe: errors during update trigger rollback via undo mechanism

  Algorithm:
    1. Locate frontmatter delimiters (--- ... ---)
    2. Check if \"naddr:\" field already exists:
       a. If exists: update value in place
       b. If not exists: insert new \"naddr: <value>\" line after slug field
    3. Mark buffer as modified
    4. Save buffer"
  (let ((undo-start buffer-undo-list)
        (changes-made 0))
    (condition-case err
        (progn
          (save-excursion
            (goto-char (point-min))

            ;; Step 1: Locate frontmatter region
            (unless (re-search-forward "^---$" nil t)
              (error "YAML frontmatter start delimiter not found"))
            ;; Move to the beginning of the next line (after the newline)
            (forward-line 1)
            (let ((fm-start (point)))
              (unless (re-search-forward "^---$" nil t)
                (error "YAML frontmatter end delimiter not found"))
              (let ((fm-end (match-beginning 0)))
                ;; Sanitize naddr value before insertion
                (let ((naddr-value (nostr-publish--sanitize-yaml-value naddr)))

                  ;; Step 2: Check if naddr field exists and update or insert
                  (goto-char fm-start)
                  (if (re-search-forward "^naddr:" fm-end t)
                      ;; Update existing naddr line
                      (progn
                        (beginning-of-line)
                        (delete-region (point) (line-end-position))
                        (insert (format "naddr: %s" naddr-value))
                        (setq changes-made (1+ changes-made)))
                    ;; Insert new naddr line after slug field
                    (goto-char fm-start)
                    (if (re-search-forward "^slug:" fm-end t)
                        ;; Found slug field, insert after it
                        (progn
                          (end-of-line)
                          (insert (format "\nnaddr: %s" naddr-value))
                          (setq changes-made (1+ changes-made)))
                      ;; No slug field, insert after opening delimiter
                      ;; fm-start is after the opening ---, need to go to beginning of next line
                      (goto-char fm-start)
                      (insert (format "naddr: %s\n" naddr-value))
                      (setq changes-made (1+ changes-made))))))))

          ;; Step 3 & 4: Mark buffer as modified and save
          (set-buffer-modified-p t)
          (save-buffer))

      ;; Error handler: rollback all changes
      (error
       (when (> changes-made 0)
         ;; Calculate how many undo entries to roll back
         (let ((undo-count (- (length buffer-undo-list) (length undo-start))))
           (when (> undo-count 0)
             (primitive-undo undo-count buffer-undo-list))))
       ;; Re-signal the original error
       (signal (car err) (cdr err))))))

(defun nostr-publish--update-cover-frontmatter (cover-metadata)
  "Update current buffer's YAML frontmatter with image metadata.
COVER-METADATA is an alist with keys (hash url dim mime).
Only hash and url are written to frontmatter; dim and mime are ignored.

Updates or inserts image.hash and image.url within existing image block.
Preserves all other frontmatter fields and image fields (file, alt, etc.).
Marks buffer as modified and saves it.

Signals error if frontmatter delimiters or image block not found.

Atomic guarantee: Either all changes are applied and saved, or all
changes are rolled back on error (via undo mechanism)."
  ;; Capture undo state before any modifications for atomic rollback
  (let ((undo-start buffer-undo-list)
        (changes-made 0))
    (condition-case err
        (progn
          (save-excursion
            (goto-char (point-min))

            ;; Step 1: Locate frontmatter region
            (unless (re-search-forward "^---$" nil t)
              (error "YAML frontmatter start delimiter not found"))
            (let ((fm-start (point)))
              (unless (re-search-forward "^---$" nil t)
                (error "YAML frontmatter end delimiter not found"))
              (let ((fm-end (match-beginning 0)))

                ;; Step 2: Check if image block exists
                (goto-char fm-start)
                (unless (re-search-forward "^image:$" fm-end t)
                  (error "Image block not found in frontmatter"))
                (let ((cover-line-end (point))
                      ;; Sanitize values before insertion (defense-in-depth)
                      (hash-value (nostr-publish--sanitize-yaml-value
                                   (alist-get 'hash cover-metadata)))
                      (url-value (nostr-publish--sanitize-yaml-value
                                  (alist-get 'url cover-metadata))))

                  ;; Step 3: Update hash field
                  (goto-char cover-line-end)
                  (if (re-search-forward "^  hash:" fm-end t)
                      ;; Replace existing hash line
                      (progn
                        (beginning-of-line)
                        (delete-region (point) (line-end-position))
                        (insert (format "  hash: %s" hash-value))
                        (setq changes-made (1+ changes-made)))
                    ;; Insert new hash line after image:
                    (goto-char cover-line-end)
                    (insert (format "\n  hash: %s" hash-value))
                    (setq changes-made (1+ changes-made)))

                  ;; Step 4: Update url field
                  (goto-char cover-line-end)
                  (if (re-search-forward "^  url:" fm-end t)
                      ;; Replace existing url line
                      (progn
                        (beginning-of-line)
                        (delete-region (point) (line-end-position))
                        (insert (format "  url: %s" url-value))
                        (setq changes-made (1+ changes-made)))
                    ;; Insert new url line
                    ;; Find the hash line we just created/updated
                    (goto-char cover-line-end)
                    (re-search-forward "^  hash:" fm-end t)
                    (end-of-line)
                    (insert (format "\n  url: %s" url-value))
                    (setq changes-made (1+ changes-made)))))))

          ;; Step 5 & 6: Mark buffer as modified and save
          (set-buffer-modified-p t)
          (save-buffer))

      ;; Error handler: rollback all changes
      (error
       (when (> changes-made 0)
         ;; Calculate how many undo entries to roll back
         (let ((undo-count (- (length buffer-undo-list) (length undo-start))))
           (when (> undo-count 0)
             (primitive-undo undo-count buffer-undo-list))))
       ;; Re-signal the original error
       (signal (car err) (cdr err))))))

;;;###autoload
(define-minor-mode nostr-publish-mode
  "Minor mode for Markdown buffers with nostr-publish keybindings."
  :lighter " Nostr"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-p") #'nostr-publish-buffer)
            map))

(provide 'nostr-publish)
;;; nostr-publish.el ends here
