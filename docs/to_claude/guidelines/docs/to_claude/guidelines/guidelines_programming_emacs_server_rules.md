<!-- ---
!-- Timestamp: 2025-05-19 06:09:18
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.claude/guidelines/guidelines_programming_emacs_server_rules.md
!-- --- -->

# Emacs Server Usage Guidelines

## Server Access
- Server socket: `/home/ywatanabe/.emacs.d/server/server`
- Emacs executables: `/opt/emacs-30.1/bin/emacs` and `/opt/emacs-30.1/bin/emacsclient`
- If server is not started, prompt the user to run `M-x server-start`

## Using a Dedicated Frame
Always use your own dedicated frame to avoid interfering with the user's work:

```bash
# Create your dedicated frame (first time)
DISPLAY=:0 && /opt/emacs-30.1/bin/emacsclient -c -n -s /home/ywatanabe/.emacs.d/server/server \
  --frame-parameters='((name . "claude-frame") (left . 800) (top . 100) (width . 80) (height . 35))' \
  -e '(message "Claude working in dedicated frame")'
```

## Working in Your Existing Frame
After creating your frame, reuse it for subsequent operations:

```bash
# Execute code in your existing frame
DISPLAY=:0 && /opt/emacs-30.1/bin/emacsclient -s /home/ywatanabe/.emacs.d/server/server \
  -e '(with-selected-frame 
        (or (car (filtered-frame-list (lambda (f) (string= (frame-parameter f '\''name) "claude-frame"))))
            (selected-frame)) 
        (progn 
          ;; YOUR CODE HERE
          ))'
```

## Advanced Frame Management
To ensure your frame exists and is properly configured:

```bash
# Create or reuse claude-frame
DISPLAY=:0 && /opt/emacs-30.1/bin/emacsclient -s /home/ywatanabe/.emacs.d/server/server \
  -e '(progn 
        (let ((frame (car (filtered-frame-list (lambda (f) (string= (frame-parameter f '\''name) "claude-frame"))))))
          (if frame
              (select-frame-set-input-focus frame)
            (make-frame '\''((name . "claude-frame") (left . 800) (top . 100) (width . 80) (height . 35)))))
        (message "Claude frame ready"))'
```

## Best Practices
1. ALWAYS create your dedicated frame first for visual inspection
2. Keep output in your frame to avoid disrupting user's workflow
3. Reuse your existing frame for multiple operations
4. When showing results, ensure they're visible in your frame
5. Use appropriate titles and messages so the user knows which frame belongs to Claude

## Troubleshooting
- If a command fails, check if your frame still exists
- If multiple frames named "claude-frame" exist, use `(car (filtered-frame-list ...))` 
- For complex operations, consider using a temporary buffer in your frame


## Discussion in *scratch* buffer
Let's discuss with the user and other agents as well! The usage is below. Please name yourself as you like.

#### Send message to the *scratch* buffer
``` bash
DISPLAY=:0 && /opt/emacs-30.1/bin/emacsclient -s                   …
      /home/ywatanabe/.emacs.d/server/server \
        -e '(with-current-buffer "*scratch*"
              (goto-char (point-max))
              (insert "\n\n;; Claude: Hello! I'm now in the *scratch* buffer. "))'
```

## Read the *scratch* buffer

``` bash
DISPLAY=:0 && /opt/emacs-30.1/bin/emacsclient -s /home/ywatanabe/.emacs.d/server/server \
        -e '(with-current-buffer "*scratch*" (buffer-string))')
```

## Check Message and Enjoy Conversation with Your Friends!

# Message received. Read the *scratch* buffer and reply to your friends as soon as possible.

<!-- EOF -->
