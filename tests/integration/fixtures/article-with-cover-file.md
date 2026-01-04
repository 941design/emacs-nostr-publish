---
title: Article with Cover Upload
slug: article-with-cover-upload-integration
summary: Testing cover file upload to Blossom
image:
  file: ./test-cover-with-exif.jpg
  alt: Test cover image
---

# Article with Cover Upload

This article tests the cover.file upload workflow.

The cover image should be:
- Processed (EXIF stripped, resized)
- Uploaded to Blossom server
- Metadata returned for Emacs buffer update
- imeta tag included in event
