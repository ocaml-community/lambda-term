#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Make the configure script executable
chmod +x configure

# Cleanup
rm -f predist.sh boring
