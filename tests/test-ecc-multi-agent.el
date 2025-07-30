;;; test-ecc-multi-agent.el --- Tests for ecc-multi-agent -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the multi-agent management functionality

;;; Code:

(require 'ert)
(require 'ecc-multi-agent)

;; Configuration Tests
(ert-deftest test-ecc-multi-agent-default-agents-exists ()
  "Test that default agents configuration exists."
  (should (boundp 'ecc-multi-agent-default-agents))
  (should (listp ecc-multi-agent-default-agents)))

(ert-deftest test-ecc-multi-agent-window-layout-exists ()
  "Test that window layout configuration exists."
  (should (boundp 'ecc-multi-agent-window-layout))
  (should (symbolp ecc-multi-agent-window-layout)))

(ert-deftest test-ecc-multi-agent-auto-start-claude-exists ()
  "Test that auto-start configuration exists."
  (should (boundp 'ecc-multi-agent-auto-start-claude))
  (should (booleanp ecc-multi-agent-auto-start-claude)))

;; Function Existence Tests
(ert-deftest test-ecc-multi-agent-core-functions-exist ()
  "Test that core multi-agent functions exist."
  (should (fboundp 'ecc-multi-agent-create-agent))
  (should (fboundp 'ecc-multi-agent-setup-general))
  (should (fboundp 'ecc-multi-agent-list-agents))
  (should (fboundp 'ecc-multi-agent-switch-to-agent))
  (should (fboundp 'ecc-multi-agent-kill-agent))
  (should (fboundp 'ecc-multi-agent-kill-all-agents)))

(ert-deftest test-ecc-multi-agent-project-functions-exist ()
  "Test that project-specific functions exist."
  (should (fboundp 'ecc-multi-agent-setup-ecc-tracker))
  (should (fboundp 'ecc-multi-agent-setup-custom-projects)))

(ert-deftest test-ecc-multi-agent-utility-functions-exist ()
  "Test that utility functions exist."
  (should (fboundp 'ecc-multi-agent-send-to-all))
  (should (fboundp 'ecc-multi-agent-menu)))

;; Interactive Command Tests
(ert-deftest test-ecc-multi-agent-create-agent-interactive ()
  "Test that create-agent is interactive."
  (should (commandp 'ecc-multi-agent-create-agent)))

(ert-deftest test-ecc-multi-agent-setup-general-interactive ()
  "Test that setup-general is interactive."
  (should (commandp 'ecc-multi-agent-setup-general)))

(ert-deftest test-ecc-multi-agent-setup-ecc-tracker-interactive ()
  "Test that setup-ecc-tracker is interactive."
  (should (commandp 'ecc-multi-agent-setup-ecc-tracker)))

(ert-deftest test-ecc-multi-agent-list-agents-interactive ()
  "Test that list-agents is interactive."
  (should (commandp 'ecc-multi-agent-list-agents)))

(ert-deftest test-ecc-multi-agent-switch-to-agent-interactive ()
  "Test that switch-to-agent is interactive."
  (should (commandp 'ecc-multi-agent-switch-to-agent)))

(ert-deftest test-ecc-multi-agent-menu-interactive ()
  "Test that menu is interactive."
  (should (commandp 'ecc-multi-agent-menu)))

;; Keymap Tests
(ert-deftest test-ecc-multi-agent-keymap-exists ()
  "Test that multi-agent keymap exists."
  (should (boundp 'ecc-multi-agent-keymap))
  (should (keymapp ecc-multi-agent-keymap)))

(ert-deftest test-ecc-keymap-exists ()
  "Test that main ECC keymap exists."
  (should (boundp 'ecc-keymap))
  (should (keymapp ecc-keymap)))

;; Mock Tests (without actually creating terminals)
(ert-deftest test-ecc-multi-agent-list-agents-no-agents ()
  "Test list-agents when no agents exist."
  (let ((buffer-list-original (symbol-function 'buffer-list)))
    (unwind-protect
        (progn
          (fset 'buffer-list (lambda () '()))
          (with-temp-buffer
            (let ((message-log nil))
              (ecc-multi-agent-list-agents)
              ;; Should not error when no agents exist
              (should t))))
      (fset 'buffer-list buffer-list-original))))

;; Feature Load Test
(ert-deftest test-ecc-multi-agent-loadable ()
  "Test that ecc-multi-agent loads without error."
  (should (featurep 'ecc-multi-agent)))

(provide 'test-ecc-multi-agent)

;;; test-ecc-multi-agent.el ends here