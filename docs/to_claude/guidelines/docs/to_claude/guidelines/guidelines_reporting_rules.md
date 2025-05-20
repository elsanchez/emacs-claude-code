<!-- ---
!-- Timestamp: 2025-05-19 10:07:19
!-- Author: ywatanabe
!-- File: /ssh:ywatanabe@sp:/home/ywatanabe/.dotfiles/.claude/to_claude/guidelines/guidelines_reporting_rules.md
!-- --- -->

## Report using Org
- Summarize the current repository as a report
- `./project_management/reports/progress-report-YYYY-MMDD-<title>.org`
- Once org file created, convert to pdf using pandoc:
  ```bash
  pandoc -f org -t pdf -o "./project_management/reports/progress-report-YYYY-MMDD-<title>.pdf" "./project_management/reports/progress-report-YYYY-MMDD-<title>.org"
  ```
- Include diagrams, figures, and tables effectively

## Report Format

```org
# Timestamp: "YYYY-MM-DD HH:mm:ss (ywatanabe)"
# File: <path-to-projects>/<project-id>-<project-name>/reports/YYYY-MMDD-HHmmss-<title>-report.org

#+STARTUP: showall
#+OPTIONS: toc:nil num:nil
#+TITLE: <title>
#+DATE: YYYY-MMDD-HHmmss

  - Project Directory
<path-to-projects>/<project-id>-<project-name>/

  - Header 1

  -  - Header 2
description:

#+ATTR_HTML: :width 600px
[[file:/path/to/filename.xxx]]


  - PDF
[[file:<path-to-projects>/<project-id>-<project-name>/reports/YYYY-MMDD-HHmmss-<title>/report.pdf]]
```

## Your Understanding Check
Did you understand the guideline? If yes, please say:
`CLAUDE UNDERSTOOD: <THIS FILE PATH HERE>`

<!-- EOF -->