<!-- ---
!-- Timestamp: 2025-05-21 01:11:01
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.claude/to_claude/guidelines/IMPORTANT-guidelines-programming-Cleanup-Rules.md
!-- --- -->

## Cleanup Guidelines
Follow these guidelines when preparing the codebase for production. The goal is to achieve product-ready quality.

1. Set up safety mechanisms
   - Create a feature branch: `feature/cleanup-YYYY-MMDD-HHmmss`
   - Create a checkpoint branch: `checkpoint/before-cleanup-YYYY-MMDD-HHmmss`
   - In the checkpoint branch, save ALL FILES (including uncommitted and hidden files)

2. Review code quality standards
   - `./docs/to_claude/guidelines/IMPORTANT-guidelines-programming-Clean-Code-Rules.md`
   - `./docs/to_claude/guidelines/the-art-of-readable-code.pdf`

3. Inventory project structure
   - Run `tree -a` to view the all the files

4. Develop a comprehensive cleanup plan
   - Prioritize tasks
   - Set clear acceptance criteria

5. Clean all project directories
   - Source code: `./src`, `./scripts`
   - Documentation: `./docs/`
   - Examples: `./examples`
   - Project management: `./project_management`
   - Tests: `./tests`

6. Handle file removal safely
   - Never use `rm` directly
   - Use `./docs/to_claude/bin/safe_rm.sh` to move obsolete files to `.old` directories
   - Reference `./docs/to_claude/guidelines/guidelines-command-rm.md`

7. Standardize file naming
   - Eliminate development patterns such as `-v01`, `-fix`, `-improved`, `-consolidated`, `-enhanced`
   - Use clean production names: `filename.ext`

8. Complete the process
   - Verify all tests pass
   - Merge `feature/cleanup-YYYY-MMDD-HHmmss` back to the original branch

<!-- EOF -->