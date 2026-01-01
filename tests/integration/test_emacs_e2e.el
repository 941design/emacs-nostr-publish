;;; test_emacs_e2e.el --- End-to-end Emacs integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 nostr-publish contributors

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; End-to-end integration tests for nostr-publish.el.
;; These tests run against real Docker infrastructure (relay + nak bunker).
;;
;; Usage:
;;   emacs --batch -l nostr-publish.el -l test_emacs_e2e.el -f test-emacs-e2e-run
;;
;; Environment variables (must be set by caller):
;;   NOSTR_CLIENT_KEY - client secret key for bunker authentication
;;   TEST_BUNKER_URI  - bunker URI (e.g., bunker://pubkey?relay=...)
;;   TEST_RELAY_URL   - relay URL (e.g., ws://localhost:8081)
;;   TEST_FIXTURE_DIR - path to test fixtures directory
;;
;; Exit codes:
;;   0 - all tests passed
;;   1 - one or more tests failed

;;; Code:

(require 'cl-lib)
;; nostr-publish is loaded via -l flag before this file

(defvar test-emacs-e2e--results nil
  "List of test results as (name . passed-p).")

(defvar test-emacs-e2e--temp-files nil
  "List of temporary files to clean up.")

(defun test-emacs-e2e--log (format-string &rest args)
  "Log a message to stdout with FORMAT-STRING and ARGS."
  (princ (apply #'format (concat format-string "\n") args)))

(defun test-emacs-e2e--record (name passed-p &optional message)
  "Record test result for NAME as PASSED-P with optional MESSAGE."
  (push (cons name passed-p) test-emacs-e2e--results)
  (if passed-p
      (test-emacs-e2e--log "[PASS] %s" name)
    (test-emacs-e2e--log "[FAIL] %s: %s" name (or message "unknown error"))))

(defun test-emacs-e2e--create-test-file (content)
  "Create a temporary test file with CONTENT.
Returns the file path. File is registered for cleanup."
  (let ((temp-file (make-temp-file "nostr-publish-e2e-" nil ".md")))
    (push temp-file test-emacs-e2e--temp-files)
    (with-temp-file temp-file
      (insert content))
    temp-file))

(defun test-emacs-e2e--cleanup ()
  "Clean up temporary files."
  (dolist (file test-emacs-e2e--temp-files)
    (ignore-errors (delete-file file)))
  (setq test-emacs-e2e--temp-files nil))

(defun test-emacs-e2e--get-env (name)
  "Get required environment variable NAME or signal error."
  (let ((value (getenv name)))
    (unless value
      (error "Required environment variable %s not set" name))
    value))

;;; Test Cases

(defun test-emacs-e2e--test-basic-publish ()
  "Test basic article publish workflow.
Creates a test article, publishes it, and verifies success message."
  (let* ((bunker-uri (test-emacs-e2e--get-env "TEST_BUNKER_URI"))
         (relay-url (test-emacs-e2e--get-env "TEST_RELAY_URL"))
         (test-content "---
title: Emacs Integration Test
slug: emacs-integration-test
summary: Testing Emacs to CLI to bunker flow
published_at: 1700000000
tags:
  - emacs
  - integration
---

# Emacs Integration Test

This article was published from Emacs batch mode.
")
         (test-file (test-emacs-e2e--create-test-file test-content))
         (nostr-publish-bunker-uri bunker-uri)
         (nostr-publish-default-relays (list relay-url))
         (nostr-publish-timeout 60)
         (captured-message nil))

    ;; Capture the message output
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured-message (apply #'format fmt args))
                 (test-emacs-e2e--log "  message: %s" captured-message))))

      ;; Open file and publish
      (with-current-buffer (find-file-noselect test-file)
        (unwind-protect
            (progn
              (nostr-publish-buffer)

              ;; Verify success
              (cond
               ((and captured-message
                     (string-match "Published:" captured-message))
                (test-emacs-e2e--record "basic-publish" t))
               ((and captured-message
                     (string-match "failed" captured-message))
                (test-emacs-e2e--record "basic-publish" nil captured-message))
               (t
                (test-emacs-e2e--record "basic-publish" nil
                                        (or captured-message "no message captured")))))
          (kill-buffer))))))

(defun test-emacs-e2e--test-minimal-frontmatter ()
  "Test article with minimal frontmatter (title + slug only)."
  (let* ((bunker-uri (test-emacs-e2e--get-env "TEST_BUNKER_URI"))
         (relay-url (test-emacs-e2e--get-env "TEST_RELAY_URL"))
         (test-content "---
title: Minimal Emacs Test
slug: minimal-emacs-test
---

# Minimal Test

Just title and slug.
")
         (test-file (test-emacs-e2e--create-test-file test-content))
         (nostr-publish-bunker-uri bunker-uri)
         (nostr-publish-default-relays (list relay-url))
         (nostr-publish-timeout 60)
         (captured-message nil))

    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured-message (apply #'format fmt args))
                 (test-emacs-e2e--log "  message: %s" captured-message))))

      (with-current-buffer (find-file-noselect test-file)
        (unwind-protect
            (progn
              (nostr-publish-buffer)

              (cond
               ((and captured-message
                     (string-match "Published:" captured-message))
                (test-emacs-e2e--record "minimal-frontmatter" t))
               (t
                (test-emacs-e2e--record "minimal-frontmatter" nil
                                        (or captured-message "no message captured")))))
          (kill-buffer))))))

(defun test-emacs-e2e--test-no-bunker-fails ()
  "Test that publish fails without bunker URI."
  (let* ((relay-url (test-emacs-e2e--get-env "TEST_RELAY_URL"))
         (test-content "---
title: No Bunker Test
slug: no-bunker-test
---

Content.
")
         (test-file (test-emacs-e2e--create-test-file test-content))
         (nostr-publish-bunker-uri nil)  ; No bunker
         (nostr-publish-default-relays (list relay-url))
         (nostr-publish-timeout 10)
         (captured-message nil))

    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured-message (apply #'format fmt args))
                 (test-emacs-e2e--log "  message: %s" captured-message))))

      (with-current-buffer (find-file-noselect test-file)
        (unwind-protect
            (progn
              (nostr-publish-buffer)

              ;; Should fail because no bunker URI
              (cond
               ((and captured-message
                     (string-match "failed" captured-message))
                (test-emacs-e2e--record "no-bunker-fails" t))
               ((and captured-message
                     (string-match "Published:" captured-message))
                (test-emacs-e2e--record "no-bunker-fails" nil
                                        "Should have failed without bunker"))
               (t
                (test-emacs-e2e--record "no-bunker-fails" nil
                                        (or captured-message "no message captured")))))
          (kill-buffer))))))

;;; Test Runner

(defun test-emacs-e2e-run ()
  "Run all end-to-end tests and exit with appropriate code."
  (test-emacs-e2e--log "")
  (test-emacs-e2e--log "=== Emacs End-to-End Integration Tests ===")
  (test-emacs-e2e--log "")

  ;; Verify environment
  (condition-case err
      (progn
        (test-emacs-e2e--log "Environment:")
        (test-emacs-e2e--log "  NOSTR_CLIENT_KEY: %s"
                            (if (getenv "NOSTR_CLIENT_KEY") "[set]" "[NOT SET]"))
        (test-emacs-e2e--log "  TEST_BUNKER_URI: %s"
                            (or (getenv "TEST_BUNKER_URI") "[NOT SET]"))
        (test-emacs-e2e--log "  TEST_RELAY_URL: %s"
                            (or (getenv "TEST_RELAY_URL") "[NOT SET]"))
        (test-emacs-e2e--log ""))
    (error
     (test-emacs-e2e--log "ERROR: %s" (error-message-string err))
     (kill-emacs 1)))

  ;; Run tests
  (condition-case err
      (progn
        (test-emacs-e2e--log "Running tests...")
        (test-emacs-e2e--log "")

        (test-emacs-e2e--test-basic-publish)
        (test-emacs-e2e--test-minimal-frontmatter)
        (test-emacs-e2e--test-no-bunker-fails))
    (error
     (test-emacs-e2e--log "ERROR during tests: %s" (error-message-string err))
     (test-emacs-e2e--cleanup)
     (kill-emacs 1)))

  ;; Cleanup
  (test-emacs-e2e--cleanup)

  ;; Summary
  (test-emacs-e2e--log "")
  (test-emacs-e2e--log "=== Summary ===")
  (let ((passed (cl-count-if #'cdr test-emacs-e2e--results))
        (total (length test-emacs-e2e--results)))
    (test-emacs-e2e--log "%d/%d tests passed" passed total)
    (test-emacs-e2e--log "")

    ;; Exit with appropriate code
    (kill-emacs (if (= passed total) 0 1))))

(provide 'test_emacs_e2e)
;;; test_emacs_e2e.el ends here
