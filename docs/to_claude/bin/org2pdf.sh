#!/bin/bash

# org_to_pdf.sh - Convert Org files to PDF format
# Usage: ./org_to_pdf.sh [file.org] [directory] [options]

set -e  # Exit immediately if a command exits with a non-zero status

# ANSI color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Print help message
print_help() {
    echo "Usage: $(basename "$0") [OPTIONS] [FILE/DIRECTORY]"
    echo ""
    echo "Convert Org mode files to PDF using Emacs."
    echo ""
    echo "Options:"
    echo "  -h, --help              Display this help message"
    echo "  -r, --recursive         Process directories recursively"
    echo "  -o, --output DIR        Specify output directory for PDFs"
    echo "  -q, --quiet             Suppress verbose output"
    echo "  -f, --force             Overwrite existing PDF files"
    echo "  -e, --emacs PATH        Path to Emacs executable (default: emacs)"
    echo "  -s, --style FILE        Path to custom LaTeX style file"
    echo "  -t, --template FILE     Path to custom LaTeX template"
    echo ""
    echo "Examples:"
    echo "  $(basename "$0") document.org"
    echo "  $(basename "$0") --recursive ~/Documents"
    echo "  $(basename "$0") --output ~/PDFs --recursive ~/Org"
    echo ""
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check dependencies
check_dependencies() {
    if ! command_exists emacs; then
        echo -e "${RED}Error: Emacs is not installed or not in PATH${NC}"
        echo "Please install Emacs to use this script."
        exit 1
    fi

    # Check for pdflatex (optional, but good to know)
    if ! command_exists pdflatex; then
        echo -e "${YELLOW}Warning: pdflatex not found. LaTeX-based PDF export might not work.${NC}"
        echo "Consider installing TeX Live or another LaTeX distribution."
    fi
}

# Convert a single Org file to PDF
convert_org_to_pdf() {
    local org_file="$1"
    local output_dir="$2"

    # Check if file exists
    if [ ! -f "$org_file" ]; then
        echo -e "${RED}Error: File not found: $org_file${NC}"
        return 1
    fi

    # Check if it's an Org file
    if [[ ! "$org_file" =~ \.org$ ]]; then
        echo -e "${YELLOW}Skipping non-Org file: $org_file${NC}"
        return 0
    fi

    # Get the base name without extension
    local base_name=$(basename "$org_file" .org)

    # Default output location (same directory as org file)
    local pdf_output="$(dirname "$org_file")/$base_name.pdf"

    # If output directory is specified, use it
    if [ -n "$output_dir" ]; then
        mkdir -p "$output_dir"
        pdf_output="$output_dir/$base_name.pdf"
    fi

    # Check if PDF already exists and --force is not set
    if [ -f "$pdf_output" ] && [ "$FORCE" != "true" ]; then
        echo -e "${YELLOW}PDF file already exists: $pdf_output${NC}"
        echo "Use --force to overwrite."
        return 0
    fi

    # Create Emacs Lisp expression for converting Org to PDF
    local elisp_command="(progn
                            (require 'ox-latex)
                            (find-file \"$org_file\")
                            (org-latex-export-to-pdf)
                            (kill-buffer))"

    # Add custom style if specified
    if [ -n "$STYLE_FILE" ]; then
        elisp_command="(progn
                          (require 'ox-latex)
                          (setq org-latex-default-packages-alist
                                (append org-latex-default-packages-alist
                                        '((\"\" \"$STYLE_FILE\" t))))
                          (find-file \"$org_file\")
                          (org-latex-export-to-pdf)
                          (kill-buffer))"
    fi

    # Add custom template if specified
    if [ -n "$TEMPLATE_FILE" ]; then
        elisp_command="(progn
                          (require 'ox-latex)
                          (setq org-latex-classes
                                (cons '(\"article\"
                                        \"$(cat $TEMPLATE_FILE)\")
                                      org-latex-classes))
                          (find-file \"$org_file\")
                          (org-latex-export-to-pdf)
                          (kill-buffer))"
    fi

    # Run Emacs with the elisp command
    if [ "$QUIET" != "true" ]; then
        echo -e "Converting ${YELLOW}$org_file${NC} to ${GREEN}$pdf_output${NC}"
    fi

    "$EMACS_PATH" --batch --no-init-file --eval "$elisp_command" 2>/dev/null

    # Move the PDF if output_dir is specified
    if [ -n "$output_dir" ]; then
        mv "$(dirname "$org_file")/$base_name.pdf" "$pdf_output" 2>/dev/null || true
    fi

    # Check if conversion was successful
    if [ -f "$pdf_output" ]; then
        if [ "$QUIET" != "true" ]; then
            echo -e "${GREEN}Successfully created: $pdf_output${NC}"
        fi
    else
        echo -e "${RED}Error: Failed to create PDF for $org_file${NC}"
        return 1
    fi

    return 0
}

# Process a directory of Org files
process_directory() {
    local dir="$1"
    local output_dir="$2"

    if [ ! -d "$dir" ]; then
        echo -e "${RED}Error: Directory not found: $dir${NC}"
        return 1
    fi

    if [ "$QUIET" != "true" ]; then
        echo -e "Processing directory: ${YELLOW}$dir${NC}"
    fi

    # Process all Org files in the directory
    for org_file in "$dir"/*.org; do
        # Skip if no org files found (glob doesn't expand)
        [ -e "$org_file" ] || continue

        convert_org_to_pdf "$org_file" "$output_dir"
    done

    # If recursive flag is set, process subdirectories
    if [ "$RECURSIVE" = "true" ]; then
        for subdir in "$dir"/*/; do
            # Skip if no subdirectories
            [ -d "$subdir" ] || continue

            process_directory "$subdir" "$output_dir"
        done
    fi

    return 0
}

# Main execution
main() {
    # Set default values
    RECURSIVE="false"
    OUTPUT_DIR=""
    QUIET="false"
    FORCE="false"
    EMACS_PATH="emacs"
    STYLE_FILE=""
    TEMPLATE_FILE=""
    TARGET=""

    # Parse command line arguments
    while [ $# -gt 0 ]; do
        case "$1" in
            -h|--help)
                print_help
                exit 0
                ;;
            -r|--recursive)
                RECURSIVE="true"
                shift
                ;;
            -o|--output)
                OUTPUT_DIR="$2"
                shift 2
                ;;
            -q|--quiet)
                QUIET="true"
                shift
                ;;
            -f|--force)
                FORCE="true"
                shift
                ;;
            -e|--emacs)
                EMACS_PATH="$2"
                shift 2
                ;;
            -s|--style)
                STYLE_FILE="$2"
                shift 2
                ;;
            -t|--template)
                TEMPLATE_FILE="$2"
                shift 2
                ;;
            -*)
                echo -e "${RED}Error: Unknown option: $1${NC}"
                print_help
                exit 1
                ;;
            *)
                TARGET="$1"
                shift
                ;;
        esac
    done

    # Check dependencies
    check_dependencies

    # Check if a target was specified
    if [ -z "$TARGET" ]; then
        echo -e "${RED}Error: No input file or directory specified${NC}"
        print_help
        exit 1
    fi

    # Process the target (file or directory)
    if [ -f "$TARGET" ]; then
        convert_org_to_pdf "$TARGET" "$OUTPUT_DIR"
    elif [ -d "$TARGET" ]; then
        process_directory "$TARGET" "$OUTPUT_DIR"
    else
        echo -e "${RED}Error: Target not found: $TARGET${NC}"
        exit 1
    fi

    if [ "$QUIET" != "true" ]; then
        echo -e "${GREEN}Conversion process completed.${NC}"
    fi

    exit 0
}

# Run the main function
main "$@"
