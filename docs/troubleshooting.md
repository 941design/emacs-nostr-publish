# Troubleshooting Guide

Common issues and solutions when using nostr-publish.

## Signer Issues

### "nak not found" Error

**Symptom**: CLI fails with "nak executable not found"

**Solution**:
1. Install nak from https://github.com/fiatjaf/nak
2. Ensure nak is in your PATH:
   ```bash
   which nak
   # Should return a path like /usr/local/bin/nak
   ```
3. If using Emacs, ensure `exec-path` includes nak's location:
   ```elisp
   (add-to-list 'exec-path "/path/to/nak/directory")
   ```

### Signer Connection Timeout

**Symptom**: CLI hangs or times out when connecting to signer

**Causes & Solutions**:

1. **Bunker URI incorrect**: Verify your bunker URI format:
   ```
   bunker://<hex-pubkey>?relay=wss://relay.example.com&secret=<optional-secret>
   ```

2. **Signer app not running**: Ensure your NIP-46 signer is running and connected to the relay specified in the bunker URI.

3. **Network issues**: Test relay connectivity:
   ```bash
   nak req -k 0 -l 1 wss://relay.example.com
   ```

4. **Firewall blocking WebSocket**: Ensure outbound WSS connections (port 443) are allowed.

### "Permission denied" from Signer

**Symptom**: Signer rejects the signing request

**Solutions**:
1. Check if you've approved the connection in your signer app
2. Some signers require re-approval after timeout - restart the publish

## Relay Issues

### "At least one --relay is required" Error

**Symptom**: CLI fails with relay requirement error

**Solution**: The `--relay` flag is mandatory. Always specify at least one relay:
```bash
nostr-publish article.md --relay wss://relay.example.com
```

For Emacs, configure `nostr-publish-default-relays`:
```elisp
(setq nostr-publish-default-relays '("wss://relay1.example.com" "wss://relay2.example.com"))
```

### "Relay not in allowlist" Error

**Symptom**: CLI fails because frontmatter relay isn't in CLI allowlist

**Cause**: Frontmatter `relays:` must be a subset of CLI `--relay` arguments.

**Solutions**:

1. **Add the relay to CLI**: Include all frontmatter relays in CLI:
   ```bash
   nostr-publish article.md --relay wss://relay1.example.com --relay wss://relay2.example.com
   ```

2. **Remove from frontmatter**: Delete the `relays:` field to use CLI defaults:
   ```yaml
   ---
   title: My Article
   slug: my-article
   # No relays field - uses all CLI relays
   ---
   ```

3. **Use wildcard**: Explicitly request all CLI relays:
   ```yaml
   ---
   title: My Article
   slug: my-article
   relays:
     - "*"
   ---
   ```

### "Connection refused" to Relay

**Symptom**: Cannot connect to specified relay

**Solutions**:

1. **Test relay availability**:
   ```bash
   nak req -k 0 -l 1 wss://relay.example.com
   ```

2. **Check relay URL format**: Must be `wss://` (secure WebSocket)

3. **Verify relay is operational**: Some relays have uptime issues or require payment/authentication

4. **Try alternative relays**:
   ```bash
   nostr-publish article.md --relay wss://relay1.example.com --relay wss://relay2.example.com
   ```

### Event Not Found After Publishing

**Symptom**: Publishing succeeds but event cannot be queried

**Causes**:

1. **Relay propagation delay**: Wait 5-10 seconds and retry
2. **Relay rejected event silently**: Some relays filter certain content
3. **Different relay queried**: Ensure you're querying the same relay(s) you published to

**Verification**:
```bash
# Query for your event by pubkey and kind
nak req -k 30023 -a <your-hex-pubkey> -l 5 wss://relay.example.com
```

### "Event too large" Error

**Symptom**: Relay rejects event due to size

**Solution**: Most relays have a 64KB-128KB limit. Reduce content size or split into multiple articles.

## Frontmatter Issues

### "Invalid frontmatter" Error

**Symptom**: CLI rejects the Markdown file

**Common causes**:

1. **Missing required fields**: Both `title` and `slug` are required:
   ```yaml
   ---
   title: My Article
   slug: my-article
   ---
   ```

2. **YAML syntax error**: Validate your YAML:
   ```bash
   python -c "import yaml; yaml.safe_load(open('article.md').read().split('---')[1])"
   ```

3. **Unknown field**: Only these fields are allowed:
   - `title` (required)
   - `slug` (required)
   - `summary`
   - `published_at`
   - `image`
   - `tags`
   - `relays`

4. **Frontmatter not at file start**: Ensure `---` is the very first line with no preceding whitespace or BOM.

### "Invalid timestamp" Error

**Symptom**: `published_at` field rejected

**Solution**: Use Unix timestamp (seconds since epoch):
```yaml
published_at: 1700000000  # Correct
# NOT: published_at: "2023-11-14T00:00:00Z"
```

Generate current timestamp:
```bash
date +%s
```

### "Invalid slug" Error

**Symptom**: Slug validation fails

**Requirements**:
- Lowercase letters, numbers, hyphens only
- No spaces or special characters
- Example: `my-article-2024` (valid), `My Article!` (invalid)

### "Invalid image URL" Error

**Symptom**: Image field rejected during validation

**Requirements**:
- Image URL must use `http://` or `https://` scheme
- Valid formats:
  ```yaml
  # Simple format
  image: https://example.com/cover.jpg

  # Extended format
  image:
    url: https://example.com/cover.jpg
    mime: image/jpeg
    alt: Description
    dim: 1200x630
  ```

**Common issues**:
1. **Missing scheme**: Use full URL, not relative path
   - ✓ `https://example.com/image.jpg`
   - ✗ `/images/cover.jpg`
   - ✗ `example.com/image.jpg`

2. **Invalid dimension format**: Must be `WIDTHxHEIGHT`
   - ✓ `1200x630`
   - ✗ `1200,630`
   - ✗ `1200px x 630px`

3. **MIME type mismatch**: If specified, should match image type
   - Generally safe to omit - MIME is inferred from file extension

### Local Cover File Issues

#### "--blossom required" Error

**Symptom**: CLI fails with error about missing `--blossom` flag

**Cause**: Using `image.file` requires a Blossom server for upload

**Solution**: Provide the `--blossom` flag with your Blossom server URL:
```bash
nostr-publish article.md --bunker "bunker://..." --relay wss://relay.example.com \
  --blossom https://blossom.example.com
```

#### "File not found" for image.file

**Symptom**: CLI cannot find the local cover image file

**Solutions**:
1. **Use relative paths**: Paths are resolved relative to the Markdown file, not the current directory
   ```yaml
   image:
     file: ./images/cover.jpg  # Relative to article.md location
   ```

2. **Check file exists**: Verify the file path is correct
   ```bash
   ls -la ./images/cover.jpg
   ```

#### Idempotent Publishing with Hash

**Behavior**: When `image.file`, `image.url`, and `image.hash` all exist in frontmatter:
- Hash is computed from the processed image file
- If computed hash matches `image.hash`, upload is **skipped** and existing `image.url` is reused
- If hash differs, image is re-uploaded and frontmatter updated with new hash/url

**Example frontmatter after first publish**:
```yaml
image:
  file: ./images/cover.jpg
  url: https://cdn.example.com/abc123...jpg
  hash: abc123def456...
```

On second publish with same image file, upload is skipped automatically.

#### Blossom Upload Timeout

**Symptom**: Upload to Blossom server times out

**Solutions**:
1. **Increase timeout**:
   ```bash
   nostr-publish article.md --blossom https://blossom.example.com --blossom-timeout 60
   ```

2. **Check Blossom server**: Ensure the server is reachable and accepting uploads

3. **Check image size**: Large images may take longer to upload

#### Emacs Buffer Not Updated After Publish

**Symptom**: After successful publish with `image.file`, the buffer doesn't show `hash` and `url`

**Causes & Solutions**:

1. **Publish failed**: Check the message in the echo area - if it shows "Publish failed:", the CLI failed and no update occurs

2. **No image.file**: Buffer write-back only happens when publishing with `image.file`

3. **Missing image block**: The frontmatter must have an `image:` block (object format, not string)
   ```yaml
   # Correct - buffer will be updated
   image:
     file: ./cover.jpg

   # Incorrect - no buffer update (string format)
   image: https://example.com/cover.jpg
   ```

4. **Buffer changed during publish**: If you switch buffers during publish, the update is skipped

## Emacs Integration Issues

### "nostr-publish command not found" in Emacs

**Solutions**:

1. Ensure nostr-publish is installed in your shell PATH
2. Emacs may not inherit your shell PATH. Add to init.el:
   ```elisp
   (setenv "PATH" (concat (getenv "PATH") ":/path/to/nostr-publish"))
   (add-to-list 'exec-path "/path/to/nostr-publish")
   ```
3. Or use `exec-path-from-shell` package to inherit PATH

### Buffer Not Recognized as Markdown

**Symptom**: `C-c C-p` doesn't work

**Solution**: Ensure buffer is in markdown-mode:
```elisp
M-x markdown-mode
```

Or check file extension is `.md`.

## Debugging Tips

### Dry Run Mode

Test without publishing:
```bash
nostr-publish article.md --dry-run
```

This validates frontmatter and constructs the event without sending it.

### Check Event Structure

Inspect the generated event JSON:
```bash
nostr-publish article.md --dry-run 2>&1 | jq .
```

### Verify nak Installation

```bash
nak --version
# Should show version number

# Test basic functionality
echo "test" | nak event --sec $(nak genkey) -k 1
```

## Getting Help

If issues persist:

1. Check the [GitHub Issues](https://github.com/941design/emacs-nostr-publish/issues)
2. Ensure you're using the latest version
3. Include in bug reports:
   - nostr-publish version
   - nak version (`nak --version`)
   - Signer type and version
   - Error message (full output)
   - Redacted bunker URI (hide the secret)
