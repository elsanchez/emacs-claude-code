<!-- ---
!-- Timestamp: 2025-05-17 07:14:11
!-- Author: ywatanabe
!-- File: /ssh:ywatanabe@sp:/home/ywatanabe/.dotfiles/.claude/to_claude/guidelines/guidelines_programming_refactoring_rules.md
!-- --- -->

## Refactoring Rules
- During development, do not change names of files, variables, functions, and so on
- Refactoring will be separately requested
- Work in `feature/refactoring`

## Refactoring Workflows
1. Commit the current status with appropriate chunks and comments
2. If `feature/refactor` already exits, determine if the current refactoring is in the step or just it is obsolete, already merged branch. In the latter case, please delete the existing `feature/refactor` branch to work on clean environment.
3. If current branch is not `feature/refactor`, create and switch to a new branch called `feature/refactor`.
4. Please try the followings as long as they will improve simplicity, readability, maintainability, while keeping original functionalities.
   - Re-organize project structure, standardize names of directories, files, variables, functions, classes/ and so on, move obsolete files under `.old` directory (e.g., `/path/to/obsolete/file.ext` -> `/path/to/obsolete/.old/file.ext`).
5. After refactoring, ensure functionalities are not changed by running tests
6. Once these steps are successfully completed, `git merge` the `feature/refactor` branch into the original branch.

## Module Consolidation with Symlinks

When consolidating multiple module versions into a unified implementation:

1. **Preparation**: 
   - Create a consolidated version with all merged functionality
   - Maintain backward compatibility with all previous versions
   - Ensure thorough test coverage

2. **Symlink Approach**:
   - Keep original files intact (e.g., `module-v1.el`, `module-v2.el`)
   - Create consolidated implementation (e.g., `module-consolidated.el`)
   - Create a symlink that existing code references (e.g., `module.el -> module-v1.el`)

3. **Testing Process**:
   ```bash
   # Start with original implementation
   ln -sf module-v1.el module.el  # Create/update symlink to original
   # Run tests to verify baseline

   # Switch to consolidated implementation
   ln -sf module-consolidated.el module.el  # Update symlink to consolidated version
   # Run tests to verify consolidated functionality
   ```

4. **Advantages**:
   - Zero changes to dependent code (all `require` statements remain unchanged)
   - Easy rollback if issues are discovered
   - Gradual migration possible for complex systems
   - Maintains all `provide` statements for backward compatibility

5. **Final Steps**:
   - Once tests pass with consolidated version, commit the changes
   - Consider adding deprecation notices in original files
   - Eventually remove original files after sufficient migration period

## Useful Tools
See `~/.claude/bin`

## Your Understanding Check
Did you understand the guideline? If yes, please say:
`CLAUDE UNDERSTOOD: <THIS FILE PATH HERE>`

<!-- EOF -->