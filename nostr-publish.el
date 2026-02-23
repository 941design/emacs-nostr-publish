;;; nostr-publish.el --- Publish Markdown to Nostr -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Rother

;; Author: Markus Rother <mail@markusrother.de>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, text
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
;; Provides C-c C-b keybinding to preview in browser before publishing.

;;; Code:

(require 'subr-x)

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
List of wss:// or ws:// URLs.  These serve as both the allowlist
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

;; Preview mode configuration
(defcustom nostr-publish-preview-relay nil
  "Preview relay URL for preview mode publishing.
Should point to preview/staging relay infrastructure."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Preview Relay URL"))
  :group 'nostr-publish)

(defcustom nostr-publish-preview-bunker nil
  "Preview bunker URI for preview mode signing.
Uses same signing identity as production."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Preview Bunker URI"))
  :group 'nostr-publish)

(defcustom nostr-publish-preview-blossom nil
  "Preview Blossom server URL for preview mode uploads.
Should point to preview/staging Blossom infrastructure."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Preview Blossom URL"))
  :group 'nostr-publish)

(defcustom nostr-publish-preview-reader nil
  "Preview reader base URL for opening preview articles.
Example: https://preview.example.com"
  :type '(choice (const :tag "None" nil)
                 (string :tag "Preview Reader URL"))
  :group 'nostr-publish)

(defcustom nostr-publish-preview-open-browser t
  "Whether to open browser with preview reader after preview publish."
  :type 'boolean
  :group 'nostr-publish)

(defun nostr-publish--resolve-cli-command ()
  "Return the CLI command after verifying it is installed.
Signals `user-error' if the executable cannot be found."
  (unless (executable-find nostr-publish-cli-command)
    (user-error "CLI '%s' not found; install with: pipx install nostr-publish"
                nostr-publish-cli-command))
  nostr-publish-cli-command)

;;;###autoload
(defun nostr-publish-buffer ()
  "Publish current Markdown buffer to Nostr.

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
       a. If variable `buffer-file-name' is nil, signal user-error
    2. Check relays are configured:
       a. If nostr-publish-default-relays is nil/empty,
          user-error: \"No relays configured\"
    3. Save buffer if modified:
       a. If `buffer-modified-p', call `save-buffer'
    4. Build CLI command arguments:
       a. Start with nostr-publish-cli-command
       b. Add variable `buffer-file-name'
       c. If nostr-publish-bunker-uri set, add \"--bunker URI\"
       d. For each relay in nostr-publish-default-relays, add \"--relay URL\"
       e. Add \"--timeout SECONDS\"
    5. Invoke CLI synchronously:
       a. Call `call-process' with arguments
       b. Capture stdout and stderr
       c. Wait for process completion with timeout
    6. Handle result:
       a. If exit code 0:
          - Parse stdout for event ID and pubkey
          - Display success message with event ID and pubkey
       b. If exit code non-zero:
          - Parse stderr for error message
          - Display error in echo area: \"Publish failed: <error>\"
       c. If timeout:
          - Display: \"Publish timed out after N seconds\"

  Raises:
    - user-error: If buffer not associated with file
    - user-error: If no relays configured
    - Does not raise for publish failures (displays message instead)"
  (interactive)
  (let ((file (buffer-file-name)))
    ;; Step 1: Validate buffer has associated file
    (unless file
      (user-error "Buffer not associated with file"))

    ;; Step 2: Validate relays are configured (required)
    (unless nostr-publish-default-relays
      (user-error "No relays configured.  Set nostr-publish-default-relays"))
    (dolist (relay nostr-publish-default-relays)
      (unless (string-match-p "\\`wss?://" relay)
        (user-error "Invalid relay URL '%s': must use ws:// or wss://" relay)))

    ;; Step 3: Save buffer if modified
    (when (buffer-modified-p)
      (save-buffer))

    ;; Step 4: Build CLI command arguments
    (let ((args (list (nostr-publish--resolve-cli-command) file)))
      (when nostr-publish-bunker-uri
        (setq args (append args (list "--bunker" nostr-publish-bunker-uri))))
      (dolist (relay nostr-publish-default-relays)
        (setq args (append args (list "--relay" relay))))
      (setq args (append args (list "--timeout" (number-to-string nostr-publish-timeout))))
      (when nostr-publish-blossom-url
        (setq args (append args (list "--blossom" nostr-publish-blossom-url))))

      ;; Step 4 & 5: Invoke CLI and handle result
      (nostr-publish--invoke-cli args))))

(defun nostr-publish--invoke-cli-common (args success-handler error-prefix)
  "Invoke CLI with ARGS and handle result via SUCCESS-HANDLER.
ARGS is a list where first element is command, rest are arguments.
SUCCESS-HANDLER is called with (result target-buffer) on success.
ERROR-PREFIX is used in error messages.
Captures stdout/stderr and displays appropriate message."
  (let ((temp-stdout-file (make-temp-file "nostr-publish-stdout-"))
        (temp-stderr-file (make-temp-file "nostr-publish-stderr-"))
        (target-buffer (current-buffer))
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
            ;; Parse JSON output and call success handler
            (condition-case _
                (let ((result (json-parse-string stdout :object-type 'alist)))
                  (funcall success-handler result target-buffer))
              (error
               (message "%s successfully (could not parse JSON output)" error-prefix))))
           (t
            ;; Failure: display error from stderr
            (let ((error-msg (string-trim (or stderr "Unknown error"))))
              (message "%s failed: %s" error-prefix error-msg)))))
      ;; Cleanup temp files
      (ignore-errors (delete-file temp-stdout-file))
      (ignore-errors (delete-file temp-stderr-file)))))

(defun nostr-publish--invoke-cli (args)
  "Invoke CLI with ARGS and handle result for production publish.
Updates frontmatter with image and naddr if present."
  (nostr-publish--invoke-cli-common
   args
   (lambda (result target-buffer)
     (let ((event-id (alist-get 'event_id result))
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
                (if naddr (format ", naddr %s" naddr) ""))))
   "Publish"))

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

Signals `user-error' if frontmatter delimiters not found.

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
              (user-error "YAML frontmatter start delimiter not found"))
            ;; Move to the beginning of the next line (after the newline)
            (forward-line 1)
            (let ((fm-start (point)))
              (unless (re-search-forward "^---$" nil t)
                (user-error "YAML frontmatter end delimiter not found"))
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

Signals `user-error' if frontmatter delimiters or image block not found.

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
              (user-error "YAML frontmatter start delimiter not found"))
            (let ((fm-start (point)))
              (unless (re-search-forward "^---$" nil t)
                (user-error "YAML frontmatter end delimiter not found"))
              (let ((fm-end (match-beginning 0)))

                ;; Step 2: Check if image block exists
                (goto-char fm-start)
                (unless (re-search-forward "^image:$" fm-end t)
                  (user-error "Image block not found in frontmatter"))
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
(defun nostr-publish-preview-buffer ()
  "Preview current Markdown buffer using preview infrastructure.

Uses the exact same publishing pipeline as production but targets
preview infrastructure and suppresses frontmatter write-back.

CONTRACT:
  Inputs:
    - Current buffer content (Markdown with frontmatter)
    - Preview configuration variables (relay, bunker, blossom, reader)

  Outputs:
    - Success message with naddr
    - Opens preview reader in browser (if configured)
    - Does NOT update buffer frontmatter

  Invariants:
    - Buffer must be saved before previewing
    - Preview relay, bunker required
    - Uses same NIP-46 signing as production
    - Source file remains unmodified

  Properties:
    - Same validation as production publish
    - Same image processing and upload
    - Same event construction (with preview tag)

  Algorithm:
    1. Validate preview configuration (relay, bunker, reader required)
    2. Validate relays (preview relay must use wss:// or ws://)
    3. Save buffer if modified
    4. Build CLI arguments with preview configuration:
       a. Use preview relay (single relay)
       b. Use preview bunker
       c. Use preview blossom (if configured)
       d. Add --tag x-emacs-nostr-publish preview
    5. Invoke CLI via nostr-publish--invoke-cli-preview
    6. On success:
       a. Do NOT update frontmatter
       b. Open preview reader with naddr (if configured)
       c. Display success message"
  (interactive)
  ;; Step 1: Validate preview configuration
  (unless nostr-publish-preview-relay
    (user-error "Preview relay not configured.  Set nostr-publish-preview-relay"))
  (unless nostr-publish-preview-bunker
    (user-error "Preview bunker not configured.  Set nostr-publish-preview-bunker"))
  (unless nostr-publish-preview-reader
    (user-error "Preview reader not configured.  Set nostr-publish-preview-reader"))

  ;; Step 2: Validate buffer has file
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer has no file.  Save buffer before previewing"))

    ;; Validate preview relay URL
    (unless (string-match-p "\\`wss?://" nostr-publish-preview-relay)
      (user-error "Invalid preview relay URL '%s': must use ws:// or wss://" nostr-publish-preview-relay))

    ;; Validate preview reader URL (must be http:// or https://)
    (unless (string-match-p "\\`https?://" nostr-publish-preview-reader)
      (user-error "Invalid preview reader URL '%s': must use http:// or https://" nostr-publish-preview-reader))

    ;; Step 3: Save buffer if modified
    (when (buffer-modified-p)
      (save-buffer))

    ;; Step 4: Build CLI command arguments for preview
    (let ((args (list (nostr-publish--resolve-cli-command) file)))
      (setq args (append args (list "--bunker" nostr-publish-preview-bunker)))
      (setq args (append args (list "--relay" nostr-publish-preview-relay)))
      (setq args (append args (list "--timeout" (number-to-string nostr-publish-timeout))))
      (when nostr-publish-preview-blossom
        (setq args (append args (list "--blossom" nostr-publish-preview-blossom))))
      ;; Add preview tag
      (setq args (append args (list "--tag" "x-emacs-nostr-publish" "preview")))

      ;; Step 5: Invoke CLI and handle result (no frontmatter updates)
      (nostr-publish--invoke-cli-preview args))))

(defun nostr-publish--invoke-cli-preview (args)
  "Invoke CLI for preview with ARGS without updating frontmatter.
Open preview reader with naddr on success."
  (nostr-publish--invoke-cli-common
   args
   (lambda (result _target-buffer)
     (let ((event-id (alist-get 'event_id result))
           (pubkey (alist-get 'pubkey result))
           (naddr (alist-get 'naddr result)))

       ;; NO frontmatter updates in preview mode

       ;; Open preview reader if configured
       (when (and naddr nostr-publish-preview-open-browser)
         (let ((preview-url (concat nostr-publish-preview-reader "/" naddr)))
           (browse-url preview-url)))

       ;; Display success message
       (message "Preview published: event ID %s, pubkey %s%s"
                event-id pubkey
                (if naddr (format ", naddr %s" naddr) ""))))
   "Preview published"))

;;;###autoload
(define-minor-mode nostr-publish-mode
  "Minor mode for Markdown buffers with nostr-publish keybindings."
  :lighter " Nostr"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-p") #'nostr-publish-buffer)
            (define-key map (kbd "C-c C-b") #'nostr-publish-preview-buffer)
            map))

(provide 'nostr-publish)
;;; nostr-publish.el ends here
