#!/bin/bash

# Default values
HOST="0.0.0.0"
PORT=3838
BROWSER="FALSE"

# Help message
usage() {
    echo "Usage: $0 [options]"
    echo "Options:"
    echo "  -h, --host      Host IP (default: 0.0.0.0)"
    echo "  -p, --port      Port number (default: 3838)"
    echo "  -b, --browser   Enable auto-opening browser"
    echo "  --help          Show this help message"
    exit 1
}

# Parse arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -h|--host) HOST="$2"; shift ;;
        -p|--port) PORT="$2"; shift ;;
        -b|--browser) BROWSER="TRUE" ;;
        --help) usage ;;
        *) echo "Unknown parameter passed: $1"; usage ;;
    esac
    shift
done

echo "Starting PlaViSCA on http://$HOST:$PORT..."
echo "Press Ctrl+C to stop the application."

# Run the Shiny app
R -e "shiny::runApp('.', host='$HOST', port=$PORT, launch.browser=$BROWSER)"
