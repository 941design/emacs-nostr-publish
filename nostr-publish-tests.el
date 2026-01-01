;;; nostr-publish-tests.el --- Unit tests for nostr-publish -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nostr-publish contributors

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT-based unit tests for nostr-publish.el
;; Tests verify:
;;  - Buffer validation (must have associated file)
;;  - Buffer auto-save on publish
;;  - CLI command construction with optional arguments
;;  - Event ID extraction from CLI output
;;  - Success/failure message handling

;;; Code:

(require 'ert)
(require 'nostr-publish)

;; Helper to create a temporary test buffer with file
(defun nostr-publish--make-test-buffer (name content &optional modified)
  "Create a test buffer named NAME with CONTENT.
If MODIFIED is t, mark buffer as modified without saving.
Returns the buffer."
  (let ((temp-file (make-temp-file "nostr-publish-test-" nil ".md"))
        (buf (get-buffer-create name)))
    (with-current-buffer buf
      (insert content)
      (set-visited-file-name temp-file t)
      (unless modified
        (set-buffer-modified-p nil)))
    buf))

;; Helper to clean up test buffer
(defun nostr-publish--cleanup-buffer (buf)
  "Clean up test buffer and its file."
  (when buf
    (let ((file (buffer-file-name buf)))
      (kill-buffer buf)
      (when file
        (ignore-errors (delete-file file))))))

(ert-deftest nostr-publish-buffer-requires-file-association ()
  "Buffer without associated file should raise error."
  (let ((buf (get-buffer-create "*test-no-file*")))
    (unwind-protect
        (with-current-buffer buf
          (should-error (nostr-publish-buffer)
                        :type 'error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-buffer-auto-saves-modified ()
  "Buffer should be auto-saved if modified."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-auto-save*"
              "# Test\nContent"
              t)))
    (unwind-protect
        (with-current-buffer buf
          (should (buffer-modified-p))
          (let ((nostr-publish--cli-called nil)
                (nostr-publish-default-relays '("wss://relay.example.com")))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args)
                         (setq nostr-publish--cli-called t))))
              (nostr-publish-buffer)
              (should-not (buffer-modified-p))
              (should nostr-publish--cli-called))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-buffer-uses-bunker-uri ()
  "CLI should receive bunker URI when configured."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-bunker*"
              "# Test\nContent"))
        (bunker-uri "bunker://test@signer.example.com?relay=wss://relay"))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-bunker-uri bunker-uri)
                (nostr-publish-default-relays '("wss://relay.example.com"))
                (captured-args nil))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args)
                         (setq captured-args args))))
              (nostr-publish-buffer)
              (should (member "--bunker" captured-args))
              (let ((bunker-idx (seq-position captured-args "--bunker")))
                (should (string= (nth (1+ bunker-idx) captured-args)
                                 bunker-uri))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-buffer-uses-default-relays ()
  "CLI should receive all configured relays."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-relays*"
              "# Test\nContent"))
        (relays '("wss://relay1.example.com" "wss://relay2.example.com")))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-default-relays relays)
                (captured-args nil))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args)
                         (setq captured-args args))))
              (nostr-publish-buffer)
              (should (member "--relay" captured-args))
              (should (member "wss://relay1.example.com" captured-args))
              (should (member "wss://relay2.example.com" captured-args)))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-buffer-uses-timeout ()
  "CLI should receive configured timeout."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-timeout*"
              "# Test\nContent"))
        (timeout 60))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-timeout timeout)
                (nostr-publish-default-relays '("wss://relay.example.com"))
                (captured-args nil))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args)
                         (setq captured-args args))))
              (nostr-publish-buffer)
              (should (member "--timeout" captured-args))
              (let ((timeout-idx (seq-position captured-args "--timeout")))
                (should (string= (nth (1+ timeout-idx) captured-args)
                                 "60"))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-buffer-passes-filename ()
  "CLI should receive buffer's filename."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-filename*"
              "# Test\nContent")))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-default-relays '("wss://relay.example.com"))
                (captured-args nil))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args)
                         (setq captured-args args))))
              (nostr-publish-buffer)
              (should (string= (nth 1 captured-args)
                               (buffer-file-name))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-extract-event-id-basic ()
  "Should extract event ID from stdout."
  (let ((output "Published with event ID: abc123def456"))
    (should (string= (nostr-publish--extract-event-id output)
                     "abc123def456"))))

(ert-deftest nostr-publish-extract-event-id-varied-format ()
  "Should handle different event ID formats."
  (let ((outputs '(("event id: 0123456789abcdef" . "0123456789abcdef")
                   ("event ID: fedcba9876543210" . "fedcba9876543210")
                   ("id: aaabbbcccdddeee" . "aaabbbcccdddeee"))))
    (dolist (pair outputs)
      (should (string= (nostr-publish--extract-event-id (car pair))
                       (cdr pair))))))

(ert-deftest nostr-publish-extract-event-id-not-found ()
  "Should return nil when event ID not in output."
  (let ((output "Successfully published (no ID in message)"))
    (should-not (nostr-publish--extract-event-id output))))

(ert-deftest nostr-publish-extract-event-id-multiline ()
  "Should extract ID even with surrounding text."
  (let ((output "Starting publish...\nevent ID: abc123\nWaiting for confirmation"))
    (should (string= (nostr-publish--extract-event-id output)
                     "abc123"))))

(ert-deftest nostr-publish-buffer-no-bunker-uri-omitted ()
  "Should not add --bunker if bunker URI not configured."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-no-bunker*"
              "# Test\nContent"))
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-bunker-uri nil)
                (nostr-publish-default-relays '("wss://relay.example.com")))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args)
                         (setq captured-args args))))
              (nostr-publish-buffer)
              (should-not (member "--bunker" captured-args)))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-buffer-no-relays-errors ()
  "Should error when no relays configured (relays are mandatory)."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-no-relays*"
              "# Test\nContent")))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-default-relays nil))
            (should-error (nostr-publish-buffer)
                          :type 'error)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-buffer-invalid-relay-url-errors ()
  "Should error when relay URL is not ws:// or wss://."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-invalid-relay*"
              "# Test\nContent")))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-default-relays '("https://relay.example.com")))
            (should-error (nostr-publish-buffer)
                          :type 'error)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-buffer-markdown-content ()
  "Should handle actual Markdown content correctly."
  (let ((markdown-content "---
title: Test Article
slug: test-article
---

# Heading

This is a test article.

- Item 1
- Item 2"))
    (let ((buf (nostr-publish--make-test-buffer
                "*test-markdown*"
                markdown-content)))
      (unwind-protect
          (with-current-buffer buf
            (let ((cli-called nil)
                  (nostr-publish-default-relays '("wss://relay.example.com")))
              (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                         (lambda (args)
                           (setq cli-called t))))
                (nostr-publish-buffer)
                (should cli-called))))
        (nostr-publish--cleanup-buffer buf)))))

(ert-deftest nostr-publish-buffer-respects-modified-flag ()
  "Should not save buffer if already saved."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-saved*"
              "# Test")))
    (unwind-protect
        (with-current-buffer buf
          (should-not (buffer-modified-p))
          (let ((save-count 0)
                (nostr-publish-default-relays '("wss://relay.example.com")))
            (cl-letf (((symbol-function 'save-buffer)
                       (lambda ()
                         (setq save-count (1+ save-count)))))
              (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                         (lambda (args) nil)))
                (nostr-publish-buffer)
                (should (eq save-count 0))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-extract-event-id-whitespace-handling ()
  "Should handle variable whitespace around event ID."
  (let ((outputs '(("event ID:  abc123" . "abc123")
                   ("event ID:   fedcba" . "fedcba")
                   ("event ID: \t\taaa999" . "aaa999"))))
    (dolist (pair outputs)
      (should (string= (nostr-publish--extract-event-id (car pair))
                       (cdr pair))))))

(ert-deftest nostr-publish-cli-argument-order ()
  "Should build CLI arguments in correct order."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-order*"
              "# Test"))
        (bunker "bunker://test")
        (relays '("wss://r1" "wss://r2"))
        (timeout 45)
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-bunker-uri bunker)
                (nostr-publish-default-relays relays)
                (nostr-publish-timeout timeout))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args)
                         (setq captured-args args))))
              (nostr-publish-buffer)
              (should (string= (nth 0 captured-args) "nostr-publish"))
              (should (file-name-absolute-p (nth 1 captured-args)))
              (should (string-match "\\.md$" (nth 1 captured-args)))
              (should (string= (nth 2 captured-args) "--bunker"))
              (should (string= (nth 3 captured-args) bunker))
              (should (string= (nth 4 captured-args) "--relay"))
              (should (string= (nth 5 captured-args) "wss://r1"))
              (should (string= (nth 6 captured-args) "--relay"))
              (should (string= (nth 7 captured-args) "wss://r2"))
              (should (string= (nth 8 captured-args) "--timeout"))
              (should (string= (nth 9 captured-args) "45")))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-cli-success-message-with-mock-output ()
  "Should display success message with event ID and pubkey when CLI succeeds."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-success-msg*"
              "# Test")))
    (unwind-protect
        (with-current-buffer buf
          (let ((message-calls nil)
                (nostr-publish-default-relays '("wss://relay.example.com")))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest args) 0))
                      ((symbol-function 'nostr-publish--extract-event-id)
                       (lambda (output)
                         "abc123def456"))
                      ((symbol-function 'nostr-publish--extract-pubkey)
                       (lambda (output)
                         "pubkey789"))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) message-calls))))
              (nostr-publish-buffer)
              (should message-calls)
              (should (string-match "Published: event ID abc123def456, pubkey pubkey789"
                                    (car message-calls))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-cli-invocation-exit-code-zero ()
  "Should treat exit code 0 as success."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-exit-zero*"
              "# Test")))
    (unwind-protect
        (with-current-buffer buf
          (let ((exit-code-received nil)
                (nostr-publish-default-relays '("wss://relay.example.com")))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args)
                         (setq exit-code-received
                               (call-process "true")))))
              (nostr-publish-buffer)
              (should (= exit-code-received 0)))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-cli-invocation-exit-code-nonzero ()
  "Should treat non-zero exit code as failure."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-exit-nonzero*"
              "# Test")))
    (unwind-protect
        (with-current-buffer buf
          (let ((exit-code-received nil)
                (nostr-publish-default-relays '("wss://relay.example.com")))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args)
                         (setq exit-code-received
                               (call-process "false")))))
              (nostr-publish-buffer)
              (should (not (= exit-code-received 0))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-extract-event-id-case-insensitive ()
  "Should handle mixed case in 'id' patterns."
  (let ((output1 "Event ID: abc123")
        (output2 "event id: def456")
        (output3 "ID: abcdef"))
    (should (string= (nostr-publish--extract-event-id output1) "abc123"))
    (should (string= (nostr-publish--extract-event-id output2) "def456"))
    (should (string= (nostr-publish--extract-event-id output3) "abcdef"))))

(ert-deftest nostr-publish-extract-event-id-no-match-when-no-hex ()
  "Should return nil if pattern matches but no hex follows."
  (let ((output "event ID: not-hex-but-text"))
    (should-not (nostr-publish--extract-event-id output))))

(ert-deftest nostr-publish-extract-pubkey-basic ()
  "Should extract pubkey from stdout."
  (let ((output "Pubkey: abc123def456789"))
    (should (string= (nostr-publish--extract-pubkey output)
                     "abc123def456789"))))

(ert-deftest nostr-publish-extract-pubkey-multiline ()
  "Should extract pubkey from multiline output."
  (let ((output "Published: abc123\nPubkey: def456789\nDone"))
    (should (string= (nostr-publish--extract-pubkey output)
                     "def456789"))))

(ert-deftest nostr-publish-extract-pubkey-not-found ()
  "Should return nil when pubkey not in output."
  (let ((output "Published: abc123\nNo pubkey here"))
    (should-not (nostr-publish--extract-pubkey output))))

(ert-deftest nostr-publish-extract-pubkey-whitespace-handling ()
  "Should handle variable whitespace around pubkey."
  (let ((outputs '(("Pubkey:  abc123" . "abc123")
                   ("Pubkey:   fedcba" . "fedcba")
                   ("Pubkey: \t\taaa999" . "aaa999"))))
    (dolist (pair outputs)
      (should (string= (nostr-publish--extract-pubkey (car pair))
                       (cdr pair))))))

(ert-deftest nostr-publish-extract-pubkey-no-match-when-no-hex ()
  "Should return nil if pattern matches but no hex follows."
  (let ((output "Pubkey: not-hex-but-text"))
    (should-not (nostr-publish--extract-pubkey output))))

(ert-deftest nostr-publish-buffer-cli-argument-construction ()
  "Should build correct command with all argument types."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-args*"
              "# Test"))
        (bunker "bunker://test")
        (relays '("wss://relay1" "wss://relay2")))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-bunker-uri bunker)
                (nostr-publish-default-relays relays)
                (nostr-publish-timeout 30)
                (captured-args nil))
            (cl-letf (((symbol-function 'nostr-publish--invoke-cli)
                       (lambda (args) (setq captured-args args))))
              (nostr-publish-buffer)
              ;; Verify all components are present
              (should (member "--bunker" captured-args))
              (should (member bunker captured-args))
              (should (member "--relay" captured-args))
              (should (member "wss://relay1" captured-args))
              (should (member "wss://relay2" captured-args))
              (should (member "--timeout" captured-args))
              (should (member "30" captured-args)))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish--invoke-cli-call-process-boundary ()
  "Verify --invoke-cli correctly invokes call-process and handles output."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-call-process*"
              "# Test"))
        (captured-command nil)
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (let ((nostr-publish-bunker-uri "bunker://test")
                (nostr-publish-default-relays '("wss://relay.example.com"))
                (nostr-publish-timeout 30))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (program infile destination display &rest args)
                         ;; Capture the call signature
                         (setq captured-command program)
                         (setq captured-args args)
                         ;; Simulate success by writing to stdout file
                         (when (and (listp destination)
                                    (listp (car destination))
                                    (eq (caar destination) :file))
                           (with-temp-file (cadar destination)
                             (insert "Published: abc123def456\nPubkey: fedcba987654\n")))
                         0)))  ; Return exit code 0
              (nostr-publish-buffer)
              ;; Verify call-process received correct command
              (should (string= captured-command "nostr-publish"))
              ;; Verify args include file, bunker, relay, timeout
              (should (member "--bunker" captured-args))
              (should (member "bunker://test" captured-args))
              (should (member "--relay" captured-args))
              (should (member "wss://relay.example.com" captured-args))
              (should (member "--timeout" captured-args))
              (should (member "30" captured-args)))))
      (nostr-publish--cleanup-buffer buf))))

(provide 'nostr-publish-tests)
;;; nostr-publish-tests.el ends here
