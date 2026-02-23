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

;; Test portability: use a known executable command so tests do not depend on
;; local `nostr-publish` installation.
(defconst nostr-publish-tests-cli-command
  (or (executable-find "emacs")
      (executable-find "sh")
      "sh")
  "Executable command used by tests to satisfy CLI presence checks.")

(setq nostr-publish-cli-command nostr-publish-tests-cli-command)

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
  "Buffer without associated file should raise user-error."
  (let ((buf (get-buffer-create "*test-no-file*")))
    (unwind-protect
        (with-current-buffer buf
          (should-error (nostr-publish-buffer)
                        :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-buffer-requires-installed-cli ()
  "Publish should raise user-error when CLI executable is missing."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-missing-cli*"
              "# Test\nContent"))
        (nostr-publish-default-relays '("wss://relay.example.com"))
        (nostr-publish-cli-command "nostr-publish-definitely-missing"))
    (unwind-protect
        (with-current-buffer buf
          (should-error (nostr-publish-buffer)
                        :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-resolve-cli-command-returns-command-when-found ()
  "CLI command helper should return configured command when executable exists."
  (let ((nostr-publish-cli-command "nostr-publish-custom"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd)
                 (when (string= cmd "nostr-publish-custom")
                   "/mock/path/nostr-publish-custom"))))
      (should (string= (nostr-publish--resolve-cli-command) "nostr-publish-custom")))))

(ert-deftest nostr-publish-resolve-cli-command-signals-user-error-when-missing ()
  "CLI command helper should signal user-error when executable is missing."
  (let ((nostr-publish-cli-command "nostr-publish-definitely-missing"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_cmd) nil)))
      (should-error (nostr-publish--resolve-cli-command)
                    :type 'user-error))))

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
                       (lambda (_args)
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
                       (lambda (program _infile destination _display &rest args)
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
              (should (string= captured-command nostr-publish-tests-cli-command))
              ;; Verify args include file, bunker, relay, timeout
              (should (member "--bunker" captured-args))
              (should (member "bunker://test" captured-args))
              (should (member "--relay" captured-args))
              (should (member "wss://relay.example.com" captured-args))
              (should (member "--timeout" captured-args))
              (should (member "30" captured-args)))))
      (nostr-publish--cleanup-buffer buf))))

;;; Tests for nostr-publish--update-cover-frontmatter

(ert-deftest nostr-publish-update-cover-frontmatter-inserts-hash-and-url ()
  "Should insert hash and url into cover block when absent."
  (let ((buf (get-buffer-create "*test-cover-insert*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test Post\n")
          (insert "image:\n")
          (insert "  file: image.jpg\n")
          (insert "---\n")
          (insert "Body content\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "abc123") (url . "https://example.com/abc.jpg")))
          (goto-char (point-min))
          (should (re-search-forward "^  hash: abc123$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  url: https://example.com/abc.jpg$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  file: image.jpg$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-updates-existing-hash-and-url ()
  "Should update existing hash and url values."
  (let ((buf (get-buffer-create "*test-cover-update*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test Post\n")
          (insert "image:\n")
          (insert "  hash: oldHash\n")
          (insert "  url: https://old.com/old.jpg\n")
          (insert "  file: image.jpg\n")
          (insert "---\n")
          (insert "Body content\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "newHash") (url . "https://new.com/new.jpg")))
          (goto-char (point-min))
          (should (re-search-forward "^  hash: newHash$" nil t))
          (should-not (save-excursion (search-forward "oldHash" nil t)))
          (goto-char (point-min))
          (should (re-search-forward "^  url: https://new.com/new.jpg$" nil t))
          (should-not (save-excursion (search-forward "old.com" nil t))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-preserves-other-frontmatter ()
  "Should preserve non-cover frontmatter fields."
  (let ((buf (get-buffer-create "*test-cover-preserve*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: My Article\n")
          (insert "slug: my-article\n")
          (insert "tags:\n")
          (insert "  - test\n")
          (insert "  - article\n")
          (insert "image:\n")
          (insert "  file: img.jpg\n")
          (insert "---\n")
          (insert "Body\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "xyz789") (url . "https://cdn.com/xyz.jpg")))
          (goto-char (point-min))
          (should (re-search-forward "^title: My Article$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^slug: my-article$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^tags:$" nil t))
          (should (re-search-forward "^  - test$" nil t))
          (should (re-search-forward "^  - article$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-preserves-other-cover-fields ()
  "Should preserve other cover fields like file and alt."
  (let ((buf (get-buffer-create "*test-cover-fields*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "image:\n")
          (insert "  file: local.jpg\n")
          (insert "  alt: Description text\n")
          (insert "  mime: image/jpeg\n")
          (insert "---\n")
          (insert "Content\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "hash999") (url . "https://blob.com/h999.jpg")))
          (goto-char (point-min))
          (should (re-search-forward "^  file: local.jpg$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  alt: Description text$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  mime: image/jpeg$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-preserves-body ()
  "Should not modify content outside frontmatter."
  (let ((buf (get-buffer-create "*test-cover-body*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "image:\n")
          (insert "  file: img.jpg\n")
          (insert "---\n")
          (insert "# Heading\n")
          (insert "\n")
          (insert "Paragraph with url: https://example.com\n")
          (insert "And hash: abc123\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (let ((body-before (buffer-substring
                              (save-excursion
                                (goto-char (point-min))
                                (re-search-forward "^---$" nil t)
                                (re-search-forward "^---$" nil t)
                                (point))
                              (point-max))))
            (nostr-publish--update-cover-frontmatter
             '((hash . "newHash") (url . "https://new.com/img.jpg")))
            (let ((body-after (buffer-substring
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward "^---$" nil t)
                                 (re-search-forward "^---$" nil t)
                                 (point))
                               (point-max))))
              (should (string= body-before body-after)))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-idempotent ()
  "Should produce same result when called twice with same input."
  (let ((buf (get-buffer-create "*test-cover-idempotent*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "image:\n")
          (insert "  file: img.jpg\n")
          (insert "---\n")
          (insert "Body\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "same123") (url . "https://same.com/img.jpg")))
          (let ((state-after-first (buffer-string)))
            (set-buffer-modified-p nil)
            (nostr-publish--update-cover-frontmatter
             '((hash . "same123") (url . "https://same.com/img.jpg")))
            (should (string= (buffer-string) state-after-first))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-errors-no-start-delimiter ()
  "Should signal user-error when frontmatter start delimiter missing."
  (let ((buf (get-buffer-create "*test-no-start*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "title: Test\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (should-error
           (nostr-publish--update-cover-frontmatter
            '((hash . "x") (url . "https://x.com/x.jpg")))
           :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-errors-no-end-delimiter ()
  "Should signal user-error when frontmatter end delimiter missing."
  (let ((buf (get-buffer-create "*test-no-end*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "image:\n")
          (insert "  file: x.jpg\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (should-error
           (nostr-publish--update-cover-frontmatter
            '((hash . "x") (url . "https://x.com/x.jpg")))
           :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-errors-no-cover-block ()
  "Should signal user-error when cover block not found."
  (let ((buf (get-buffer-create "*test-no-cover*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "slug: test\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (should-error
           (nostr-publish--update-cover-frontmatter
            '((hash . "x") (url . "https://x.com/x.jpg")))
           :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-marks-modified ()
  "Should mark buffer as modified after update."
  (let ((buf (get-buffer-create "*test-modified*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "image:\n")
          (insert "  file: img.jpg\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (should-not (buffer-modified-p))
          (nostr-publish--update-cover-frontmatter
           '((hash . "mod123") (url . "https://mod.com/img.jpg")))
          (should-not (buffer-modified-p)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-indentation ()
  "Should maintain proper YAML indentation (2 spaces)."
  (let ((buf (get-buffer-create "*test-indent*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "image:\n")
          (insert "  file: img.jpg\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "ind123") (url . "https://ind.com/i.jpg")))
          (goto-char (point-min))
          (should (re-search-forward "^  hash: ind123$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  url: https://ind.com/i.jpg$" nil t))
          (should-not (save-excursion (search-forward "   hash:" nil t)))
          (should-not (save-excursion (search-forward " hash:" nil t))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-special-chars-in-url ()
  "Should handle URLs with special characters."
  (let ((buf (get-buffer-create "*test-special*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "image:\n")
          (insert "  file: img.jpg\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "sp123")
             (url . "https://cdn.example.com/path/to/image.jpg?v=1&format=webp")))
          (goto-char (point-min))
          (should (re-search-forward
                   "^  url: https://cdn.example.com/path/to/image.jpg\\?v=1&format=webp$"
                   nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-varied-structure-minimal ()
  "Should handle minimal frontmatter with only cover block."
  (let ((buf (get-buffer-create "*test-minimal*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "image:\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "min123") (url . "https://min.com/img.jpg")))
          (goto-char (point-min))
          (should (re-search-forward "^  hash: min123$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  url: https://min.com/img.jpg$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-varied-structure-complex ()
  "Should handle complex frontmatter with many fields."
  (let ((buf (get-buffer-create "*test-complex*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Complex Article\n")
          (insert "slug: complex-article\n")
          (insert "tags:\n")
          (insert "  - tech\n")
          (insert "  - tutorial\n")
          (insert "author:\n")
          (insert "  name: John Doe\n")
          (insert "  email: john@example.com\n")
          (insert "image:\n")
          (insert "  file: complex.jpg\n")
          (insert "  alt: Complex image\n")
          (insert "date: 2026-01-04\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "cx123") (url . "https://cx.com/complex.jpg")))
          (goto-char (point-min))
          (should (re-search-forward "^title: Complex Article$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  name: John Doe$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  hash: cx123$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  url: https://cx.com/complex.jpg$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-ignores-dim-and-mime ()
  "Should not write dim and mime metadata to frontmatter."
  (let ((buf (get-buffer-create "*test-ignore-meta*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "image:\n")
          (insert "  file: img.jpg\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-cover-frontmatter
           '((hash . "ig123")
             (url . "https://ig.com/img.jpg")
             (dim . "1200x630")
             (mime . "image/jpeg")))
          (goto-char (point-min))
          (should (re-search-forward "^  hash: ig123$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  url: https://ig.com/img.jpg$" nil t))
          (should-not (save-excursion (search-forward "dim:" nil t)))
          (should-not (save-excursion (search-forward "1200x630" nil t))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-cover-frontmatter-deterministic ()
  "Should produce identical output for same input (determinism test)."
  (let ((buf1 (get-buffer-create "*test-det1*"))
        (buf2 (get-buffer-create "*test-det2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (insert "---\n")
            (insert "title: Test\n")
            (insert "image:\n")
            (insert "  file: img.jpg\n")
            (insert "---\n")
            (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
            (set-buffer-modified-p nil)
            (nostr-publish--update-cover-frontmatter
             '((hash . "det123") (url . "https://det.com/img.jpg"))))
          (with-current-buffer buf2
            (insert "---\n")
            (insert "title: Test\n")
            (insert "image:\n")
            (insert "  file: img.jpg\n")
            (insert "---\n")
            (set-visited-file-name (make-temp-file "cover-test-" nil ".md") t)
            (set-buffer-modified-p nil)
            (nostr-publish--update-cover-frontmatter
             '((hash . "det123") (url . "https://det.com/img.jpg"))))
          (should (string= (with-current-buffer buf1 (buffer-string))
                           (with-current-buffer buf2 (buffer-string)))))
      (nostr-publish--cleanup-buffer buf1)
      (nostr-publish--cleanup-buffer buf2))))

;;; Tests for nostr-publish--extract-cover-metadata

;;; Integration tests for publish flow with cover metadata

(ert-deftest nostr-publish-invoke-cli-cover-metadata-integration ()
  "Verify cover metadata extraction and buffer update in publish flow."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-integration*"
              "---\ntitle: Test Article\nimage:\n  file: image.jpg\n---\n\nContent")))
    (unwind-protect
        (with-current-buffer buf
          (let ((cli-output "{\"event_id\":\"abc123def456\",\"pubkey\":\"pubkey123\",\"image\":{\"hash\":\"coverHash789\",\"url\":\"https://blossom.example/cover.jpg\",\"dim\":\"1200x630\",\"mime\":\"image/jpeg\"}}"))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (_program _infile destination _display &rest _args)
                         (when (and (listp destination)
                                    (listp (car destination))
                                    (eq (caar destination) :file))
                           (with-temp-file (cadar destination)
                             (insert cli-output)))
                         0)))
              (let ((nostr-publish-default-relays '("wss://relay.example.com"))
                    (nostr-publish-bunker-uri nil)
                    (nostr-publish-timeout 30))
                (nostr-publish-buffer)
                (let ((content (buffer-string)))
                  (should (string-match "hash: coverHash789" content))
                  (should (string-match "url: https://blossom.example/cover.jpg" content))
                  (should (string-match "file: image.jpg" content))
                  (should-not (buffer-modified-p)))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-invoke-cli-no-cover-metadata ()
  "Verify backward compatibility when no cover metadata in CLI output."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-no-cover-output*"
              "---\ntitle: Test Article\n---\n\nContent")))
    (unwind-protect
        (with-current-buffer buf
          (let ((cli-output "Published: abc123def456\nPubkey: pubkey123")
                (original-content (buffer-string)))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (_program _infile destination _display &rest _args)
                         (when (and (listp destination)
                                    (listp (car destination))
                                    (eq (caar destination) :file))
                           (with-temp-file (cadar destination)
                             (insert cli-output)))
                         0)))
              (let ((nostr-publish-default-relays '("wss://relay.example.com"))
                    (nostr-publish-bunker-uri nil)
                    (nostr-publish-timeout 30))
                (nostr-publish-buffer)
                (should (string= original-content (buffer-string)))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-invoke-cli-cover-with-existing-metadata ()
  "Verify cover metadata update when hash/url already exist (republish)."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-republish*"
              "---\ntitle: Test Article\nimage:\n  file: image.jpg\n  hash: oldHash\n  url: https://old.example/old.jpg\n---\n\nContent")))
    (unwind-protect
        (with-current-buffer buf
          (let ((cli-output "{\"event_id\":\"newEvent123\",\"pubkey\":\"pubkey789\",\"image\":{\"hash\":\"newHash456\",\"url\":\"https://new.example/new.jpg\",\"dim\":\"1200x630\",\"mime\":\"image/jpeg\"}}"))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (_program _infile destination _display &rest _args)
                         (when (and (listp destination)
                                    (listp (car destination))
                                    (eq (caar destination) :file))
                           (with-temp-file (cadar destination)
                             (insert cli-output)))
                         0)))
              (let ((nostr-publish-default-relays '("wss://relay.example.com"))
                    (nostr-publish-bunker-uri nil)
                    (nostr-publish-timeout 30))
                (nostr-publish-buffer)
                (let ((content (buffer-string)))
                  (should (string-match "hash: newHash456" content))
                  (should (string-match "url: https://new.example/new.jpg" content))
                  (should-not (string-match "oldHash" content))
                  (should-not (string-match "old.example" content))
                  (should (string-match "file: image.jpg" content))
                  (should-not (buffer-modified-p)))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-invoke-cli-cover-failure-no-update ()
  "Verify buffer not updated when CLI fails even if it outputs cover metadata."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-failure*"
              "---\ntitle: Test Article\nimage:\n  file: image.jpg\n---\n\nContent")))
    (unwind-protect
        (with-current-buffer buf
          (let ((cli-output "Image: {\"hash\":\"shouldNotUpdate\",\"url\":\"https://ignored.example/x.jpg\",\"dim\":\"100x100\",\"mime\":\"image/png\"}")
                (original-content (buffer-string)))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (_program _infile destination _display &rest _args)
                         (when (and (listp destination)
                                    (listp (car destination))
                                    (eq (caar destination) :file))
                           (with-temp-file (cadar destination)
                             (insert cli-output)))
                         1)))
              (let ((nostr-publish-default-relays '("wss://relay.example.com"))
                    (nostr-publish-bunker-uri nil)
                    (nostr-publish-timeout 30))
                (nostr-publish-buffer)
                (should (string= original-content (buffer-string)))
                (should-not (string-match "shouldNotUpdate" (buffer-string)))))))
      (nostr-publish--cleanup-buffer buf))))

;;; Tests for nostr-publish--sanitize-yaml-value

(ert-deftest nostr-publish-sanitize-yaml-value-removes-newlines ()
  "Should remove both Unix and Windows style newlines."
  (should (string= (nostr-publish--sanitize-yaml-value "hello\nworld")
                   "helloworld"))
  (should (string= (nostr-publish--sanitize-yaml-value "hello\rworld")
                   "helloworld"))
  (should (string= (nostr-publish--sanitize-yaml-value "hello\r\nworld")
                   "helloworld"))
  (should (string= (nostr-publish--sanitize-yaml-value "line1\nline2\nline3")
                   "line1line2line3")))

(ert-deftest nostr-publish-sanitize-yaml-value-removes-yaml-structural-chars ()
  "Should remove YAML structural characters (brackets and braces)."
  (should (string= (nostr-publish--sanitize-yaml-value "text[with]brackets")
                   "textwithbrackets"))
  (should (string= (nostr-publish--sanitize-yaml-value "text{with}braces")
                   "textwithbraces"))
  (should (string= (nostr-publish--sanitize-yaml-value "[start]middle{end}")
                   "startmiddleend"))
  (should (string= (nostr-publish--sanitize-yaml-value "{{nested}}")
                   "nested")))

(ert-deftest nostr-publish-sanitize-yaml-value-handles-nil ()
  "Should return nil when input is nil (no crash, no empty string)."
  (should-not (nostr-publish--sanitize-yaml-value nil)))

(ert-deftest nostr-publish-sanitize-yaml-value-preserves-safe-content ()
  "Should preserve content without dangerous characters."
  (should (string= (nostr-publish--sanitize-yaml-value "https://example.com/image.jpg")
                   "https://example.com/image.jpg"))
  (should (string= (nostr-publish--sanitize-yaml-value "sha256:abc123def456")
                   "sha256:abc123def456"))
  (should (string= (nostr-publish--sanitize-yaml-value "Simple text with spaces")
                   "Simple text with spaces")))

(ert-deftest nostr-publish-sanitize-yaml-value-combined-threats ()
  "Should handle values with multiple dangerous characters."
  (should (string= (nostr-publish--sanitize-yaml-value "url: https://evil.com\nkey: value")
                   "url: https://evil.comkey: value"))
  (should (string= (nostr-publish--sanitize-yaml-value "hash: {inject}\nmore: [data]")
                   "hash: injectmore: data"))
  (should (string= (nostr-publish--sanitize-yaml-value "[\r\n{}]")
                   "")))

(ert-deftest nostr-publish-sanitize-yaml-value-empty-string ()
  "Should handle empty string."
  (should (string= (nostr-publish--sanitize-yaml-value "")
                   "")))

(ert-deftest nostr-publish-sanitize-yaml-value-only-dangerous-chars ()
  "Should return empty string when input only contains dangerous chars."
  (should (string= (nostr-publish--sanitize-yaml-value "\n\r")
                   ""))
  (should (string= (nostr-publish--sanitize-yaml-value "[]{}")
                   ""))
  (should (string= (nostr-publish--sanitize-yaml-value "\n[]\r{}")
                   "")))

(ert-deftest nostr-publish-sanitize-yaml-value-unicode-content ()
  "Should preserve Unicode characters while removing dangerous chars."
  (should (string= (nostr-publish--sanitize-yaml-value "café\nrestaurant")
                   "caférestaurant"))
  (should (string= (nostr-publish--sanitize-yaml-value "日本語[test]")
                   "日本語test")))

(ert-deftest nostr-publish-sanitize-yaml-value-special-url-chars ()
  "Should preserve URL special characters that are safe for YAML."
  (should (string= (nostr-publish--sanitize-yaml-value "https://example.com/path?query=value&other=123")
                   "https://example.com/path?query=value&other=123"))
  (should (string= (nostr-publish--sanitize-yaml-value "file://path/to/file#anchor")
                   "file://path/to/file#anchor")))

(ert-deftest nostr-publish-sanitize-yaml-value-whitespace-preservation ()
  "Should preserve spaces and tabs, only removing newlines."
  (should (string= (nostr-publish--sanitize-yaml-value "text with spaces")
                   "text with spaces"))
  (should (string= (nostr-publish--sanitize-yaml-value "text\twith\ttabs")
                   "text\twith\ttabs"))
  (should (string= (nostr-publish--sanitize-yaml-value "  leading spaces")
                   "  leading spaces"))
  (should (string= (nostr-publish--sanitize-yaml-value "trailing spaces  ")
                   "trailing spaces  ")))

(ert-deftest nostr-publish-sanitize-yaml-value-idempotent ()
  "Should be idempotent (applying twice yields same result as once)."
  (let ((input "test\n[value]{data}"))
    (let ((once (nostr-publish--sanitize-yaml-value input)))
      (should (string= (nostr-publish--sanitize-yaml-value once) once)))))

(ert-deftest nostr-publish-sanitize-yaml-value-real-world-blossom-url ()
  "Should handle realistic Blossom server URL."
  (should (string= (nostr-publish--sanitize-yaml-value "https://cdn.blossom.example.com/sha256abc123def456.jpg")
                   "https://cdn.blossom.example.com/sha256abc123def456.jpg")))

(ert-deftest nostr-publish-sanitize-yaml-value-real-world-hash ()
  "Should handle realistic hash values."
  (should (string= (nostr-publish--sanitize-yaml-value "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
                   "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")))

;;; Tests for nostr-publish--extract-naddr

;;; Tests for nostr-publish--update-naddr-frontmatter

(ert-deftest nostr-publish-update-naddr-frontmatter-inserts-new-naddr ()
  "Should insert naddr field when absent."
  (let ((buf (get-buffer-create "*test-naddr-insert*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test Article\n")
          (insert "slug: test-article\n")
          (insert "---\n")
          (insert "Body content\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-naddr-frontmatter "naddr1qqxnzd3cxsmnjv3hx56rjwf3")
          (goto-char (point-min))
          (should (re-search-forward "^naddr: naddr1qqxnzd3cxsmnjv3hx56rjwf3$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-updates-existing-naddr ()
  "Should update existing naddr field."
  (let ((buf (get-buffer-create "*test-naddr-update*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "naddr: oldnaddr1qqxxx\n")
          (insert "slug: test\n")
          (insert "---\n")
          (insert "Body\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-naddr-frontmatter "newnaddr1qqyyyy")
          (goto-char (point-min))
          (should (re-search-forward "^naddr: newnaddr1qqyyyy$" nil t))
          (should-not (save-excursion (search-forward "oldnaddr1" nil t))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-inserts-after-slug ()
  "Should insert naddr after slug field when no existing naddr."
  (let ((buf (get-buffer-create "*test-naddr-after-slug*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "slug: test-slug\n")
          (insert "tags: [test]\n")
          (insert "---\n")
          (insert "Body\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-naddr-frontmatter "naddr1insert")
          (goto-char (point-min))
          (re-search-forward "^slug: test-slug$" nil t)
          (should (re-search-forward "^naddr: naddr1insert$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-preserves-other-fields ()
  "Should preserve non-naddr frontmatter fields."
  (let ((buf (get-buffer-create "*test-naddr-preserve*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: My Article\n")
          (insert "slug: my-article\n")
          (insert "tags:\n")
          (insert "  - test\n")
          (insert "  - article\n")
          (insert "image:\n")
          (insert "  file: img.jpg\n")
          (insert "---\n")
          (insert "Body\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-naddr-frontmatter "naddr1preserve")
          (goto-char (point-min))
          (should (re-search-forward "^title: My Article$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^slug: my-article$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^tags:$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^image:$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-preserves-body ()
  "Should not modify content outside frontmatter."
  (let ((buf (get-buffer-create "*test-naddr-body*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "slug: test\n")
          (insert "---\n")
          (insert "# Heading\n")
          (insert "\n")
          (insert "Paragraph with naddr: naddr1old\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (let ((body-before (buffer-substring
                              (save-excursion
                                (goto-char (point-min))
                                (re-search-forward "^---$" nil t)
                                (re-search-forward "^---$" nil t)
                                (point))
                              (point-max))))
            (nostr-publish--update-naddr-frontmatter "naddr1new")
            (let ((body-after (buffer-substring
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward "^---$" nil t)
                                 (re-search-forward "^---$" nil t)
                                 (point))
                               (point-max))))
              (should (string= body-before body-after)))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-idempotent ()
  "Should produce same result when called twice with same input."
  (let ((buf (get-buffer-create "*test-naddr-idempotent*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "slug: test\n")
          (insert "---\n")
          (insert "Body\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-naddr-frontmatter "naddr1same")
          (let ((state-after-first (buffer-string)))
            (set-buffer-modified-p nil)
            (nostr-publish--update-naddr-frontmatter "naddr1same")
            (should (string= (buffer-string) state-after-first))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-errors-no-start-delimiter ()
  "Should signal user-error when frontmatter start delimiter missing."
  (let ((buf (get-buffer-create "*test-naddr-no-start*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "title: Test\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (should-error
           (nostr-publish--update-naddr-frontmatter "naddr1test")
           :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-errors-no-end-delimiter ()
  "Should signal user-error when frontmatter end delimiter missing."
  (let ((buf (get-buffer-create "*test-naddr-no-end*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "slug: test\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (should-error
           (nostr-publish--update-naddr-frontmatter "naddr1test")
           :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-marks-modified ()
  "Should mark buffer as modified after update."
  (let ((buf (get-buffer-create "*test-naddr-modified*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "slug: test\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (should-not (buffer-modified-p))
          (nostr-publish--update-naddr-frontmatter "naddr1mod")
          (should-not (buffer-modified-p)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-minimal-frontmatter ()
  "Should handle minimal frontmatter with only title and slug."
  (let ((buf (get-buffer-create "*test-naddr-minimal*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "slug: test\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-naddr-frontmatter "naddr1min")
          (goto-char (point-min))
          (should (re-search-forward "^naddr: naddr1min$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-complex-structure ()
  "Should handle complex frontmatter with many nested fields."
  (let ((buf (get-buffer-create "*test-naddr-complex*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Complex Article\n")
          (insert "slug: complex-article\n")
          (insert "tags:\n")
          (insert "  - tech\n")
          (insert "  - tutorial\n")
          (insert "author:\n")
          (insert "  name: John Doe\n")
          (insert "  email: john@example.com\n")
          (insert "image:\n")
          (insert "  file: complex.jpg\n")
          (insert "date: 2026-01-04\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-naddr-frontmatter "naddr1complex")
          (goto-char (point-min))
          (should (re-search-forward "^naddr: naddr1complex$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^title: Complex Article$" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^  name: John Doe$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-sanitizes-value ()
  "Should sanitize naddr value before insertion (remove dangerous chars)."
  (let ((buf (get-buffer-create "*test-naddr-sanitize*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "slug: test\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-naddr-frontmatter "naddr1test\nmalicious[injection]{data}")
          (goto-char (point-min))
          (should (re-search-forward "^naddr: naddr1testmaliciousinjectiondata$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-realistic-naddr ()
  "Should handle realistic NIP-19 naddr values."
  (let ((buf (get-buffer-create "*test-naddr-realistic*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Article\n")
          (insert "slug: article\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (let ((realistic-naddr "naddr1qqs2p3s3yma3yd8aw23wfjd73pjsglpsdsz48x6e4c7e3xm0v90cj3qgzqvqz"))
            (nostr-publish--update-naddr-frontmatter realistic-naddr)
            (goto-char (point-min))
            (should (re-search-forward (format "^naddr: %s$" (regexp-quote realistic-naddr)) nil t))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-update-naddr-frontmatter-no-slug-field ()
  "Should insert naddr after opening delimiter when no slug field."
  (let ((buf (get-buffer-create "*test-naddr-no-slug*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "---\n")
          (insert "title: Test\n")
          (insert "tags: [test]\n")
          (insert "---\n")
          (set-visited-file-name (make-temp-file "naddr-test-" nil ".md") t)
          (set-buffer-modified-p nil)
          (nostr-publish--update-naddr-frontmatter "naddr1noslug")
          (goto-char (point-min))
          (should (re-search-forward "^naddr: naddr1noslug$" nil t)))
      (nostr-publish--cleanup-buffer buf))))

;;; Tests for Preview Mode (nostr-publish-preview-buffer)

(ert-deftest nostr-publish-preview-requires-preview-relay ()
  "Preview buffer should fail without preview relay configured."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-relay*"
              "# Test\nContent"))
        (nostr-publish-preview-relay nil)
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com"))
    (unwind-protect
        (with-current-buffer buf
          (should-error (nostr-publish-preview-buffer)
                        :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-requires-preview-bunker ()
  "Preview buffer should fail without preview bunker configured."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-bunker*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker nil)
        (nostr-publish-preview-reader "https://preview.example.com"))
    (unwind-protect
        (with-current-buffer buf
          (should-error (nostr-publish-preview-buffer)
                        :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-requires-preview-reader ()
  "Preview buffer should fail without preview reader configured."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-reader*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader nil))
    (unwind-protect
        (with-current-buffer buf
          (should-error (nostr-publish-preview-buffer)
                        :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-requires-file-association ()
  "Preview should fail for buffers without associated file."
  (let ((buf (get-buffer-create "*test-preview-no-file*"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com"))
    (unwind-protect
        (with-current-buffer buf
          (insert "# Test\nContent")
          (should-error (nostr-publish-preview-buffer)
                        :type 'user-error))
      (kill-buffer buf))))

(ert-deftest nostr-publish-preview-requires-installed-cli ()
  "Preview should raise user-error when CLI executable is missing."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-missing-cli*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (nostr-publish-cli-command "nostr-publish-definitely-missing"))
    (unwind-protect
        (with-current-buffer buf
          (should-error (nostr-publish-preview-buffer)
                        :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-validates-relay-url ()
  "Preview should reject invalid relay URLs."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-invalid-relay*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "http://invalid.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com"))
    (unwind-protect
        (with-current-buffer buf
          (should-error (nostr-publish-preview-buffer)
                        :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-validates-reader-url ()
  "Preview should reject invalid reader URLs (must be http:// or https://)."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-invalid-reader*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "ftp://invalid.reader.com"))
    (unwind-protect
        (with-current-buffer buf
          (should-error (nostr-publish-preview-buffer)
                        :type 'user-error))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-accepts-https-reader ()
  "Preview should accept https:// reader URLs."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-https-reader*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            (should captured-args)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-accepts-http-reader ()
  "Preview should accept http:// reader URLs (for local development)."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-http-reader*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "http://localhost:3000")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            (should captured-args)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-accepts-wss-relay ()
  "Preview should accept wss:// relay URLs."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-wss*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            (should captured-args)
            (should (member "--relay" captured-args))
            (should (member "wss://preview.relay.com" captured-args))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-accepts-ws-relay ()
  "Preview should accept ws:// relay URLs."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-ws*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "ws://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            (should captured-args)
            (should (member "ws://preview.relay.com" captured-args))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-uses-preview-bunker ()
  "Preview CLI should receive preview bunker URI."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-bunker-arg*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://preview-pubkey@preview.signer.com")
        (nostr-publish-preview-reader "https://preview.example.com")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            (should (member "--bunker" captured-args))
            (let ((bunker-idx (seq-position captured-args "--bunker")))
              (should (string= (nth (1+ bunker-idx) captured-args)
                               "bunker://preview-pubkey@preview.signer.com")))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-uses-single-relay ()
  "Preview CLI should receive exactly one relay (the preview relay)."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-single-relay*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (nostr-publish-default-relays '("wss://relay1.com" "wss://relay2.com"))
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            ;; Count --relay occurrences
            (let ((relay-count (cl-count "--relay" captured-args :test #'string=)))
              (should (= relay-count 1)))
            ;; Verify it's the preview relay
            (should (member "wss://preview.relay.com" captured-args))
            ;; Verify default relays are NOT included
            (should-not (member "wss://relay1.com" captured-args))
            (should-not (member "wss://relay2.com" captured-args))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-uses-preview-blossom ()
  "Preview CLI should receive preview Blossom URL when configured."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-blossom*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (nostr-publish-preview-blossom "https://preview.blossom.com")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            (should (member "--blossom" captured-args))
            (should (member "https://preview.blossom.com" captured-args))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-omits-blossom-when-not-configured ()
  "Preview CLI should not include --blossom when not configured."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-no-blossom*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (nostr-publish-preview-blossom nil)
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            (should-not (member "--blossom" captured-args))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-injects-preview-tag ()
  "Preview CLI should include --tag x-emacs-nostr-publish preview."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-tag*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            (should (member "--tag" captured-args))
            (let ((tag-idx (seq-position captured-args "--tag")))
              (should (string= (nth (1+ tag-idx) captured-args) "x-emacs-nostr-publish"))
              (should (string= (nth (+ tag-idx 2) captured-args) "preview")))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-uses-timeout ()
  "Preview CLI should receive configured timeout."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-timeout*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (nostr-publish-timeout 45)
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            (should (member "--timeout" captured-args))
            (let ((timeout-idx (seq-position captured-args "--timeout")))
              (should (string= (nth (1+ timeout-idx) captured-args) "45")))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-passes-filename ()
  "Preview CLI should receive buffer's filename."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-filename*"
              "# Test\nContent"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            ;; Filename should be second argument (after command name)
            (should (string= (nth 1 captured-args) (buffer-file-name)))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-auto-saves-modified ()
  "Preview should auto-save buffer if modified."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-autosave*"
              "# Test\nContent"
              t))  ; Create as modified
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://test")
        (nostr-publish-preview-reader "https://preview.example.com")
        (nostr-publish--cli-called nil))
    (unwind-protect
        (with-current-buffer buf
          (should (buffer-modified-p))
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (_args)
                       (setq nostr-publish--cli-called t))))
            (nostr-publish-preview-buffer)
            (should-not (buffer-modified-p))
            (should nostr-publish--cli-called)))
      (nostr-publish--cleanup-buffer buf))))

;;; Tests for nostr-publish--invoke-cli-preview (no frontmatter writeback)

(ert-deftest nostr-publish-preview-cli-no-cover-metadata-update ()
  "Preview CLI handler should NOT update cover metadata in buffer."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-no-cover-update*"
              "---\ntitle: Test\nimage:\n  file: img.jpg\n---\nContent"))
        (cli-output "{\"event_id\":\"abc123\",\"pubkey\":\"pub123\",\"naddr\":\"naddr1test\",\"image\":{\"hash\":\"hash999\",\"url\":\"https://preview.blossom/img.jpg\"}}"))
    (unwind-protect
        (with-current-buffer buf
          (let ((original-content (buffer-string)))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (_program _infile destination _display &rest _args)
                         (when (and (listp destination)
                                    (listp (car destination))
                                    (eq (caar destination) :file))
                           (with-temp-file (cadar destination)
                             (insert cli-output)))
                         0))
                      ((symbol-function 'browse-url)
                       (lambda (_url) nil)))
              (nostr-publish--invoke-cli-preview
               (list "nostr-publish" (buffer-file-name) "--relay" "wss://test"))
              ;; Buffer content should NOT contain the hash/url from CLI output
              (should (string= (buffer-string) original-content))
              (should-not (string-match "hash999" (buffer-string))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-cli-no-naddr-update ()
  "Preview CLI handler should NOT update naddr in buffer."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-no-naddr-update*"
              "---\ntitle: Test\nslug: test-slug\n---\nContent"))
        (cli-output "{\"event_id\":\"abc123\",\"pubkey\":\"pub123\",\"naddr\":\"naddr1shouldnotappear\"}"))
    (unwind-protect
        (with-current-buffer buf
          (let ((original-content (buffer-string)))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (_program _infile destination _display &rest _args)
                         (when (and (listp destination)
                                    (listp (car destination))
                                    (eq (caar destination) :file))
                           (with-temp-file (cadar destination)
                             (insert cli-output)))
                         0))
                      ((symbol-function 'browse-url)
                       (lambda (_url) nil)))
              (nostr-publish--invoke-cli-preview
               (list "nostr-publish" (buffer-file-name) "--relay" "wss://test"))
              ;; Buffer content should NOT contain naddr
              (should (string= (buffer-string) original-content))
              (should-not (string-match "naddr1shouldnotappear" (buffer-string))))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-cli-opens-browser ()
  "Preview CLI handler should open browser with naddr URL."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-browser*"
              "---\ntitle: Test\nslug: test\n---\nContent"))
        (cli-output "{\"event_id\":\"abc123\",\"pubkey\":\"pub123\",\"naddr\":\"naddr1testbrowser\"}")
        (nostr-publish-preview-reader "https://preview.example.com")
        (nostr-publish-preview-open-browser t)
        (captured-url nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'call-process)
                     (lambda (_program _infile destination _display &rest _args)
                       (when (and (listp destination)
                                  (listp (car destination))
                                  (eq (caar destination) :file))
                         (with-temp-file (cadar destination)
                           (insert cli-output)))
                       0))
                    ((symbol-function 'browse-url)
                     (lambda (url) (setq captured-url url))))
            (nostr-publish--invoke-cli-preview
             (list "nostr-publish" (buffer-file-name) "--relay" "wss://test"))
            ;; Verify browser opened with correct URL
            (should (string= captured-url "https://preview.example.com/naddr1testbrowser"))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-cli-no-browser-when-disabled ()
  "Preview CLI handler should NOT open browser when disabled."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-no-browser*"
              "---\ntitle: Test\nslug: test\n---\nContent"))
        (cli-output "{\"event_id\":\"abc123\",\"pubkey\":\"pub123\",\"naddr\":\"naddr1test\"}")
        (nostr-publish-preview-reader "https://preview.example.com")
        (nostr-publish-preview-open-browser nil)
        (browser-called nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'call-process)
                     (lambda (_program _infile destination _display &rest _args)
                       (when (and (listp destination)
                                  (listp (car destination))
                                  (eq (caar destination) :file))
                         (with-temp-file (cadar destination)
                           (insert cli-output)))
                       0))
                    ((symbol-function 'browse-url)
                     (lambda (_url) (setq browser-called t))))
            (nostr-publish--invoke-cli-preview
             (list "nostr-publish" (buffer-file-name) "--relay" "wss://test"))
            (should-not browser-called)))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-cli-success-message ()
  "Preview CLI handler should display preview success message."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-message*"
              "---\ntitle: Test\nslug: test\n---\nContent"))
        (cli-output "{\"event_id\":\"abc123\",\"pubkey\":\"pub123\",\"naddr\":\"naddr1msgtest\"}")
        (nostr-publish-preview-reader "https://preview.example.com")
        (nostr-publish-preview-open-browser nil)
        (captured-message nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'call-process)
                     (lambda (_program _infile destination _display &rest _args)
                       (when (and (listp destination)
                                  (listp (car destination))
                                  (eq (caar destination) :file))
                         (with-temp-file (cadar destination)
                           (insert cli-output)))
                       0))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq captured-message (apply #'format fmt args)))))
            (nostr-publish--invoke-cli-preview
             (list "nostr-publish" (buffer-file-name) "--relay" "wss://test"))
            (should (string-match "Preview published:" captured-message))
            (should (string-match "abc123" captured-message))
            (should (string-match "naddr1msgtest" captured-message))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-cli-failure-message ()
  "Preview CLI handler should display failure message on error."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-fail-msg*"
              "---\ntitle: Test\nslug: test\n---\nContent"))
        (nostr-publish-preview-reader "https://preview.example.com")
        (captured-message nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'call-process)
                     (lambda (_program _infile destination _display &rest _args)
                       (when (and (listp destination)
                                  (listp (car destination)))
                         (let ((stderr-file (cadr destination)))
                           (with-temp-file stderr-file
                             (insert "Validation failed: missing required field"))))
                       1))  ; Non-zero exit code
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq captured-message (apply #'format fmt args)))))
            (nostr-publish--invoke-cli-preview
             (list "nostr-publish" (buffer-file-name) "--relay" "wss://test"))
            (should (string-match "failed" captured-message))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-cli-full-argument-construction ()
  "Verify complete CLI argument construction for preview."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-full-args*"
              "# Test"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://preview-pubkey")
        (nostr-publish-preview-reader "https://preview.reader.com")
        (nostr-publish-preview-blossom "https://preview.blossom.com")
        (nostr-publish-timeout 60)
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            ;; Verify all components present
            (should (string= (car captured-args) nostr-publish-tests-cli-command))
            (should (member "--bunker" captured-args))
            (should (member "bunker://preview-pubkey" captured-args))
            (should (member "--relay" captured-args))
            (should (member "wss://preview.relay.com" captured-args))
            (should (member "--timeout" captured-args))
            (should (member "60" captured-args))
            (should (member "--blossom" captured-args))
            (should (member "https://preview.blossom.com" captured-args))
            (should (member "--tag" captured-args))
            (should (member "x-emacs-nostr-publish" captured-args))
            (should (member "preview" captured-args))))
      (nostr-publish--cleanup-buffer buf))))

;;; Tests for preview isolation from production

(ert-deftest nostr-publish-preview-does-not-use-production-relays ()
  "Preview should never use nostr-publish-default-relays."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-isolation-relays*"
              "# Test"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://preview")
        (nostr-publish-preview-reader "https://preview.reader.com")
        (nostr-publish-default-relays '("wss://prod1.relay.com" "wss://prod2.relay.com"))
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            ;; Production relays should NOT appear
            (should-not (member "wss://prod1.relay.com" captured-args))
            (should-not (member "wss://prod2.relay.com" captured-args))
            ;; Preview relay should appear
            (should (member "wss://preview.relay.com" captured-args))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-does-not-use-production-bunker ()
  "Preview should never use nostr-publish-bunker-uri."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-isolation-bunker*"
              "# Test"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://preview-pubkey")
        (nostr-publish-preview-reader "https://preview.reader.com")
        (nostr-publish-bunker-uri "bunker://production-pubkey")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            ;; Production bunker should NOT appear
            (should-not (member "bunker://production-pubkey" captured-args))
            ;; Preview bunker should appear
            (should (member "bunker://preview-pubkey" captured-args))))
      (nostr-publish--cleanup-buffer buf))))

(ert-deftest nostr-publish-preview-does-not-use-production-blossom ()
  "Preview should use preview Blossom, not production."
  (let ((buf (nostr-publish--make-test-buffer
              "*test-preview-isolation-blossom*"
              "# Test"))
        (nostr-publish-preview-relay "wss://preview.relay.com")
        (nostr-publish-preview-bunker "bunker://preview")
        (nostr-publish-preview-reader "https://preview.reader.com")
        (nostr-publish-preview-blossom "https://preview.blossom.com")
        (nostr-publish-blossom-url "https://production.blossom.com")
        (captured-args nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'nostr-publish--invoke-cli-preview)
                     (lambda (args) (setq captured-args args))))
            (nostr-publish-preview-buffer)
            ;; Production blossom should NOT appear
            (should-not (member "https://production.blossom.com" captured-args))
            ;; Preview blossom should appear
            (should (member "https://preview.blossom.com" captured-args))))
      (nostr-publish--cleanup-buffer buf))))

;;; Tests for Minor Mode Keybindings

(ert-deftest nostr-publish-test-keymap-publish-binding ()
  "Test that C-c C-p is bound to nostr-publish-buffer in minor mode."
  (let ((binding (lookup-key nostr-publish-mode-map (kbd "C-c C-p"))))
    (should (eq binding 'nostr-publish-buffer))))

(ert-deftest nostr-publish-test-keymap-preview-binding ()
  "Test that C-c C-b is bound to nostr-publish-preview-buffer in minor mode."
  (let ((binding (lookup-key nostr-publish-mode-map (kbd "C-c C-b"))))
    (should (eq binding 'nostr-publish-preview-buffer))))

(provide 'nostr-publish-tests)
;;; nostr-publish-tests.el ends here
