# Emacs Claude Code

A streamlined Emacs interface for Claude AI coding assistance.

## Overview

Emacs Claude Code (ECC) enhances your workflow by seamlessly integrating Claude's AI capabilities directly into Emacs. The package provides:

- Optimized VTERM mode for high-performance Claude interaction
- Automatic response handling with state detection
- Smart paste functionality for large text (yank-as-file)
- Visual aids for Claude prompts
- Notification bell for prompt alerts
- Auto-scroll to follow Claude's output

## Installation

### Requirements

- Emacs 28+
- vterm package
- Claude CLI (Anthropic)

### Setup

1. Clone this repository:
   ```
   git clone https://github.com/ywatanabe/emacs-claude-code.git ~/.emacs.d/lisp/emacs-claude-code
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/lisp/emacs-claude-code")
   (require 'emacs-claude-code)
   ```

## Usage

Global key bindings for quick access:

- `C-c c v` - Start a new Claude vterm session
- `C-c c o` - Optimize current vterm for Claude
- `C-c c a` - Enable auto-response for Claude prompts
- `C-c c t` - Toggle visual aids for Claude interaction

### Quick Start

```elisp
M-x ecc-claude-vterm       ; Start a new optimized Claude vterm buffer
M-x ecc-claude-auto-respond ; Enable auto-response for the current buffer
```

### Interactive Demo

Try the comprehensive demo that showcases all features:

```elisp
(require 'claude-integrated-demo)
M-x claude-demo-all-features
```

## Key Features

### Optimized VTerm Mode

Significantly enhances vterm performance for Claude's high-output interaction:

- Increased process output buffer (1MB default)
- GC optimizations during buffer updates
- Disabled bidirectional text processing
- Reduced visual effects (line numbers, font variations)
- Buffer truncation to manage memory usage

### Auto-Response Mode

Automatically handles Claude's interactive prompts:

- Detects y/n and continuation prompts
- Sends customizable responses
- Notifies about auto-responses
- Timer-based checking for prompt detection
- ESC key instantly disables auto-response
- Smart throttling prevents duplicate responses
- Configurable interaction limits to prevent runaway responses

### Smart Yank (Paste-as-File)

Prevents terminal overflow with large content:

- Saves large clipboard content to temporary files
- Inserts `cat` commands instead of raw content
- Threshold-based decision (1000 chars by default)
- Automatic cleanup of temporary files

### Visual Aids

Makes Claude's state clearly visible:

- Highlighting of detected prompts
- State indicators displayed in the buffer
- Frame highlighting for active prompts
- Mode line state indicators

### Notification Bell

Get alerted when Claude is waiting for input:

- Audio bell notifications
- Mode line flashing for visual feedback
- State-based notification timing

### Follow Bottom

Keeps Claude's output visible:

- Auto-scrolls to show new output
- Preserves margins for readability
- Detects manual scrolling to pause auto-scrolling

## Customization

Configure the behavior with these variables:

```elisp
;; Auto-response settings
(setq ecc-auto-response-y/n "1")       ; Response for y/n prompts
(setq ecc-auto-response-y/y/n "2")     ; Response for y/y/n prompts
(setq ecc-auto-response-waiting "/auto") ; Response for continue prompts
(setq ecc-auto-response-throttle-time 5.0) ; Seconds between identical responses
(setq ecc-auto-response-esc-disables t)    ; ESC key disables auto-response

;; Auto-response limits
(setq ecc-interaction-limit-enabled nil)   ; Enable/disable interaction limits
(setq ecc-interaction-limit-count 50)      ; Max auto-responses per session
(setq ecc-interaction-limit-per-time-enabled nil) ; Enable hourly limits
(setq ecc-interaction-limit-per-hour 20)   ; Max auto-responses per hour

;; VTerm optimization settings
(setq vterm-max-scrollback 10000)      ; Increase scrollback buffer

;; Yank-as-file settings
(setq ecc-vterm-yank-threshold 2000)   ; Character threshold for file-based yanking
```

## Examples

The `examples/simplified/` directory contains easy-to-understand examples:

- `simplified-usage.el` - Core functionality demonstration
- `simplified-usage-interactive.el` - Interactive commands
- `vterm-optimization-usage.el` - VTerm performance enhancements
- `vterm-yank-file-usage.el` - Smart paste functionality
- `vterm-comprehensive-demo.el` - Complete feature showcase
- `claude-integrated-demo.el` - Interactive demo script

## Simplified Implementation

This release focuses on a streamlined, modular implementation. For more details, see the [src/README.md](src/README.md) file.

## Contact

Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)