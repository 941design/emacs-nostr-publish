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
;; Environment variables:
;;   Required:
;;     TEST_BUNKER_URI   - bunker URI (e.g., bunker://pubkey?relay=...)
;;                         May include &secret=... for secret-based auth
;;     TEST_RELAY_URL    - relay URL (e.g., ws://localhost:8080)
;;     TEST_FIXTURE_DIR  - path to test fixtures directory
;;   Optional:
;;     TEST_BLOSSOM_URL  - Blossom server URL (enables image upload tests)
;;     NOSTR_CLIENT_KEY  - NIP-46 client secret key for bunker authorization
;;                         (alternative to &secret= in bunker URI)
;;
;; Authentication:
;;   Two auth methods are supported (one required):
;;   1. NOSTR_CLIENT_KEY env var - pubkey-based auth via --authorized-keys
;;   2. &secret= in TEST_BUNKER_URI - secret-based auth via --authorized-secrets
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
  "Get required environment variable NAME or signal user-error."
  (let ((value (getenv name)))
    (unless value
      (user-error "Required environment variable %s not set" name))
    value))

;;; Test Cases

(defun test-emacs-e2e--test-image-writeback ()
  "Test image upload with buffer write-back workflow.
Creates a test article with image.file, publishes it, and verifies
that image.hash and image.url are written back to the buffer."
  (let* ((bunker-uri (test-emacs-e2e--get-env "TEST_BUNKER_URI"))
         (relay-url (test-emacs-e2e--get-env "TEST_RELAY_URL"))
         (blossom-url (test-emacs-e2e--get-env "TEST_BLOSSOM_URL"))
         (fixture-dir (test-emacs-e2e--get-env "TEST_FIXTURE_DIR"))
         (cover-image (expand-file-name "test-cover-with-exif.jpg" fixture-dir))
         (test-content (format "---
title: Image Writeback Test
slug: image-writeback-test
summary: Testing image upload and buffer write-back
published_at: 1700000000
image:
  file: %s
  alt: Test image
tags:
  - image
  - integration
---

# Image Writeback Test

This article tests the image upload and buffer write-back functionality.
" cover-image))
         (test-file (test-emacs-e2e--create-test-file test-content))
         (nostr-publish-bunker-uri bunker-uri)
         (nostr-publish-default-relays (list relay-url))
         (nostr-publish-blossom-url blossom-url)
         (nostr-publish-timeout 60)
         (captured-message nil))

    ;; Verify image file exists
    (unless (file-exists-p cover-image)
      (test-emacs-e2e--record "image-writeback" nil
                              (format "Image file not found: %s" cover-image))
      (cl-return-from test-emacs-e2e--test-image-writeback nil))

    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured-message (apply #'format fmt args))
                 (test-emacs-e2e--log "  message: %s" captured-message))))

      (with-current-buffer (find-file-noselect test-file)
        (unwind-protect
            (progn
              (nostr-publish-buffer)

              ;; Check for success message
              (unless (and captured-message
                           (string-match "Published:" captured-message))
                (test-emacs-e2e--record "image-writeback" nil
                                        (or captured-message "no message captured"))
                (cl-return-from test-emacs-e2e--test-image-writeback nil))

              ;; Re-read buffer content to verify write-back
              (revert-buffer t t t)
              (let ((buffer-content (buffer-string)))
                ;; Verify image block is still an object (not string)
                (unless (string-match "^image:$" buffer-content)
                  (test-emacs-e2e--record "image-writeback" nil
                                          "image block not found or not an object")
                  (cl-return-from test-emacs-e2e--test-image-writeback nil))

                ;; Verify image.hash was written
                (unless (string-match "^  hash: [a-f0-9]+" buffer-content)
                  (test-emacs-e2e--record "image-writeback" nil
                                          "image.hash not found in buffer")
                  (cl-return-from test-emacs-e2e--test-image-writeback nil))

                ;; Verify image.url was written
                (unless (string-match "^  url: https?://" buffer-content)
                  (test-emacs-e2e--record "image-writeback" nil
                                          "image.url not found in buffer")
                  (cl-return-from test-emacs-e2e--test-image-writeback nil))

                ;; Verify image.file was preserved
                (unless (string-match "^  file: " buffer-content)
                  (test-emacs-e2e--record "image-writeback" nil
                                          "image.file was removed from buffer")
                  (cl-return-from test-emacs-e2e--test-image-writeback nil))

                ;; Verify image.alt was preserved
                (unless (string-match "^  alt: " buffer-content)
                  (test-emacs-e2e--record "image-writeback" nil
                                          "image.alt was removed from buffer")
                  (cl-return-from test-emacs-e2e--test-image-writeback nil))

                ;; All checks passed
                (test-emacs-e2e--record "image-writeback" t)))

          (kill-buffer))))))

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

;;; Preview Mode Tests

(defun test-emacs-e2e--test-preview-basic-publish ()
  "Test basic preview publish workflow.
Creates a test article, publishes it in preview mode, and verifies:
- Success message is displayed
- Source file is NOT modified (no naddr written back)"
  (let* ((bunker-uri (test-emacs-e2e--get-env "TEST_BUNKER_URI"))
         (relay-url (test-emacs-e2e--get-env "TEST_RELAY_URL"))
         (test-content "---
title: Preview Test Article
slug: preview-test-article
summary: Testing preview mode
published_at: 1700000000
tags:
  - preview
  - test
---

# Preview Test Article

This article tests the preview mode workflow.
")
         (test-file (test-emacs-e2e--create-test-file test-content))
         ;; Use same infrastructure for preview (in real usage, these would be separate)
         (nostr-publish-preview-bunker bunker-uri)
         (nostr-publish-preview-relay relay-url)
         (nostr-publish-preview-reader "https://preview.example.com")
         (nostr-publish-preview-blossom nil)
         (nostr-publish-preview-open-browser nil)  ; Don't open browser in tests
         (nostr-publish-timeout 60)
         (captured-message nil)
         (original-content test-content))

    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured-message (apply #'format fmt args))
                 (test-emacs-e2e--log "  message: %s" captured-message)))
              ((symbol-function 'browse-url)
               (lambda (_url) nil)))  ; Suppress browser

      (with-current-buffer (find-file-noselect test-file)
        (unwind-protect
            (progn
              (nostr-publish-preview-buffer)

              ;; Verify success message
              (unless (and captured-message
                           (string-match "Preview published:" captured-message))
                (test-emacs-e2e--record "preview-basic" nil
                                        (or captured-message "no message captured"))
                (cl-return-from test-emacs-e2e--test-preview-basic-publish nil))

              ;; Re-read file and verify NO naddr was written
              (revert-buffer t t t)
              (let ((buffer-content (buffer-string)))
                (if (string-match "^naddr:" buffer-content)
                    (progn
                      (test-emacs-e2e--record "preview-basic" nil
                                              "naddr was written to file (should not happen in preview)")
                      (cl-return-from test-emacs-e2e--test-preview-basic-publish nil))
                  (test-emacs-e2e--record "preview-basic" t))))

          (kill-buffer))))))

(defun test-emacs-e2e--test-preview-with-image ()
  "Test preview publish with image upload.
Creates a test article with image.file, publishes in preview mode, and verifies:
- Success message is displayed
- Source file is NOT modified (no hash/url written back)"
  (let* ((bunker-uri (test-emacs-e2e--get-env "TEST_BUNKER_URI"))
         (relay-url (test-emacs-e2e--get-env "TEST_RELAY_URL"))
         (blossom-url (test-emacs-e2e--get-env "TEST_BLOSSOM_URL"))
         (fixture-dir (test-emacs-e2e--get-env "TEST_FIXTURE_DIR"))
         (cover-image (expand-file-name "test-cover-with-exif.jpg" fixture-dir))
         (test-content (format "---
title: Preview with Image
slug: preview-image-test
summary: Testing preview mode with image upload
published_at: 1700000000
image:
  file: %s
  alt: Test preview image
tags:
  - preview
  - image
---

# Preview with Image

Testing preview mode with Blossom image upload.
" cover-image))
         (test-file (test-emacs-e2e--create-test-file test-content))
         (nostr-publish-preview-bunker bunker-uri)
         (nostr-publish-preview-relay relay-url)
         (nostr-publish-preview-reader "https://preview.example.com")
         (nostr-publish-preview-blossom blossom-url)
         (nostr-publish-preview-open-browser nil)
         (nostr-publish-timeout 60)
         (captured-message nil))

    ;; Verify image file exists
    (unless (file-exists-p cover-image)
      (test-emacs-e2e--record "preview-image" nil
                              (format "Image file not found: %s" cover-image))
      (cl-return-from test-emacs-e2e--test-preview-with-image nil))

    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured-message (apply #'format fmt args))
                 (test-emacs-e2e--log "  message: %s" captured-message)))
              ((symbol-function 'browse-url)
               (lambda (_url) nil)))

      (with-current-buffer (find-file-noselect test-file)
        (unwind-protect
            (progn
              (nostr-publish-preview-buffer)

              ;; Verify success
              (unless (and captured-message
                           (string-match "Preview published:" captured-message))
                (test-emacs-e2e--record "preview-image" nil
                                        (or captured-message "no message captured"))
                (cl-return-from test-emacs-e2e--test-preview-with-image nil))

              ;; Re-read file and verify NO hash/url was written
              (revert-buffer t t t)
              (let ((buffer-content (buffer-string)))
                (cond
                 ((string-match "^  hash:" buffer-content)
                  (test-emacs-e2e--record "preview-image" nil
                                          "hash was written to file (should not happen in preview)"))
                 ((string-match "^  url:" buffer-content)
                  (test-emacs-e2e--record "preview-image" nil
                                          "url was written to file (should not happen in preview)"))
                 ((string-match "^naddr:" buffer-content)
                  (test-emacs-e2e--record "preview-image" nil
                                          "naddr was written to file (should not happen in preview)"))
                 (t
                  (test-emacs-e2e--record "preview-image" t)))))

          (kill-buffer))))))

(defun test-emacs-e2e--test-preview-minimal-frontmatter ()
  "Test preview mode with minimal frontmatter (title + slug only)."
  (let* ((bunker-uri (test-emacs-e2e--get-env "TEST_BUNKER_URI"))
         (relay-url (test-emacs-e2e--get-env "TEST_RELAY_URL"))
         (test-content "---
title: Minimal Preview
slug: minimal-preview
---

# Minimal Preview

Just title and slug in preview mode.
")
         (test-file (test-emacs-e2e--create-test-file test-content))
         (nostr-publish-preview-bunker bunker-uri)
         (nostr-publish-preview-relay relay-url)
         (nostr-publish-preview-reader "https://preview.example.com")
         (nostr-publish-preview-blossom nil)
         (nostr-publish-preview-open-browser nil)
         (nostr-publish-timeout 60)
         (captured-message nil))

    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured-message (apply #'format fmt args))
                 (test-emacs-e2e--log "  message: %s" captured-message)))
              ((symbol-function 'browse-url)
               (lambda (_url) nil)))

      (with-current-buffer (find-file-noselect test-file)
        (unwind-protect
            (progn
              (nostr-publish-preview-buffer)

              (cond
               ((and captured-message
                     (string-match "Preview published:" captured-message))
                (test-emacs-e2e--record "preview-minimal" t))
               (t
                (test-emacs-e2e--record "preview-minimal" nil
                                        (or captured-message "no message captured")))))
          (kill-buffer))))))

(defun test-emacs-e2e--test-preview-no-config-fails ()
  "Test that preview fails without preview configuration."
  (let* ((test-content "---
title: Preview No Config
slug: preview-no-config
---

Content.
")
         (test-file (test-emacs-e2e--create-test-file test-content))
         (nostr-publish-preview-bunker nil)  ; No bunker
         (nostr-publish-preview-relay nil)   ; No relay
         (nostr-publish-preview-reader nil)  ; No reader
         (nostr-publish-timeout 10)
         (error-occurred nil))

    (with-current-buffer (find-file-noselect test-file)
      (unwind-protect
          (progn
            (condition-case _
                (nostr-publish-preview-buffer)
              (error (setq error-occurred t)))

            (if error-occurred
                (test-emacs-e2e--record "preview-no-config-fails" t)
              (test-emacs-e2e--record "preview-no-config-fails" nil
                                      "Should have failed without preview config")))
        (kill-buffer)))))

(defun test-emacs-e2e--test-preview-file-unchanged ()
  "Test that preview mode preserves original file exactly.
Compares file content before and after preview to ensure no changes."
  (let* ((bunker-uri (test-emacs-e2e--get-env "TEST_BUNKER_URI"))
         (relay-url (test-emacs-e2e--get-env "TEST_RELAY_URL"))
         (test-content "---
title: File Unchanged Test
slug: file-unchanged-test
summary: Verifying file is not modified
published_at: 1700000000
tags:
  - test
---

# File Unchanged Test

This file should remain identical after preview.
")
         (test-file (test-emacs-e2e--create-test-file test-content))
         (nostr-publish-preview-bunker bunker-uri)
         (nostr-publish-preview-relay relay-url)
         (nostr-publish-preview-reader "https://preview.example.com")
         (nostr-publish-preview-blossom nil)
         (nostr-publish-preview-open-browser nil)
         (nostr-publish-timeout 60)
         (captured-message nil)
         ;; Read original file content from disk
         (original-disk-content (with-temp-buffer
                                  (insert-file-contents test-file)
                                  (buffer-string))))

    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured-message (apply #'format fmt args))
                 (test-emacs-e2e--log "  message: %s" captured-message)))
              ((symbol-function 'browse-url)
               (lambda (_url) nil)))

      (with-current-buffer (find-file-noselect test-file)
        (unwind-protect
            (progn
              (nostr-publish-preview-buffer)

              ;; Verify success first
              (unless (and captured-message
                           (string-match "Preview published:" captured-message))
                (test-emacs-e2e--record "preview-file-unchanged" nil
                                        (or captured-message "no message captured"))
                (cl-return-from test-emacs-e2e--test-preview-file-unchanged nil))

              ;; Read file content from disk again and compare
              (let ((final-disk-content (with-temp-buffer
                                          (insert-file-contents test-file)
                                          (buffer-string))))
                (if (string= original-disk-content final-disk-content)
                    (test-emacs-e2e--record "preview-file-unchanged" t)
                  (test-emacs-e2e--record "preview-file-unchanged" nil
                                          "File content was modified during preview"))))

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
        (test-emacs-e2e--log "  TEST_BUNKER_URI: %s"
                            (or (getenv "TEST_BUNKER_URI") "[NOT SET]"))
        (test-emacs-e2e--log "  TEST_RELAY_URL: %s"
                            (or (getenv "TEST_RELAY_URL") "[NOT SET]"))
        (test-emacs-e2e--log "  TEST_BLOSSOM_URL: %s"
                            (or (getenv "TEST_BLOSSOM_URL") "[NOT SET]"))
        (test-emacs-e2e--log "  TEST_FIXTURE_DIR: %s"
                            (or (getenv "TEST_FIXTURE_DIR") "[NOT SET]"))
        (test-emacs-e2e--log ""))
    (error
     (test-emacs-e2e--log "ERROR: %s" (error-message-string err))
     (kill-emacs 1)))

  ;; Run tests
  (condition-case err
      (progn
        (test-emacs-e2e--log "Running tests...")
        (test-emacs-e2e--log "")

        ;; Production publish tests
        (test-emacs-e2e--test-basic-publish)
        (test-emacs-e2e--test-minimal-frontmatter)
        (test-emacs-e2e--test-no-bunker-fails)
        ;; Image writeback test requires TEST_BLOSSOM_URL and TEST_FIXTURE_DIR
        (when (and (getenv "TEST_BLOSSOM_URL") (getenv "TEST_FIXTURE_DIR"))
          (test-emacs-e2e--test-image-writeback))

        ;; Preview mode tests
        (test-emacs-e2e--log "")
        (test-emacs-e2e--log "Running preview mode tests...")
        (test-emacs-e2e--log "")
        (test-emacs-e2e--test-preview-no-config-fails)
        (test-emacs-e2e--test-preview-basic-publish)
        (test-emacs-e2e--test-preview-minimal-frontmatter)
        (test-emacs-e2e--test-preview-file-unchanged)
        ;; Preview with image test requires TEST_BLOSSOM_URL and TEST_FIXTURE_DIR
        (when (and (getenv "TEST_BLOSSOM_URL") (getenv "TEST_FIXTURE_DIR"))
          (test-emacs-e2e--test-preview-with-image)))
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
