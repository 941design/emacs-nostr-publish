---
title: Introducing nostr-publish — Publish Long-Form Content From Emacs
slug: introducing-nostr-publish
summary: A lightweight tool for publishing Markdown articles to Nostr relays, with support for remote signing and Emacs integration.
tags:
  - nostr
  - tools
  - writing
  - open-source
---

## `C-c C-p` for publish

This post was created from [a markdown file](https://github.com/941design/emacs-nostr-publish/blob/master/announcement.md) and published from Emacs.

I wanted a clean workflow for publishing articles without leaving my editor. No web interfaces, no copy-pasting — just write and publish.

`nostr-publish` is a straightforward way to publish long-form articles ([NIP-23](https://github.com/nostr-protocol/nips/blob/master/23.md)). Write content in Markdown, add a simple header with your title, slug, summary, and it handles the rest.

The tool is designed to work with remote signers ([NIP-46](https://github.com/nostr-protocol/nips/blob/master/46.md)). This is a prerequisite and helps with separation of concerns: writing, publishing, signing -- each handled by specialized tools.

`nostr-publish` is free software (GPLv3+) and [available on GitHub](https://github.com/941design/emacs-nostr-publish).
