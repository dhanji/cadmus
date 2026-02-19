#!/bin/bash
# Build and install cadmus to ~/.local/bin

set -e

cd "$(dirname "$0")/.."

INSTALL_DIR="$HOME/.local/bin"
mkdir -p "$INSTALL_DIR"

echo "Building cadmus (release)..."
cargo build --release

echo "Installing to $INSTALL_DIR..."
cp target/release/cadmus "$INSTALL_DIR/"

# Re-sign binary after copying (required on macOS to avoid security policy rejection)
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Re-signing binary for macOS..."
    codesign --force --sign - "$INSTALL_DIR/cadmus"
fi

echo "Done! Installed:"
echo "  $INSTALL_DIR/cadmus"

# Check if ~/.local/bin is in PATH and fix if needed
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    echo ""
    echo "⚠️  $INSTALL_DIR is not in your PATH"

    # Detect shell config file
    SHELL_NAME=$(basename "$SHELL")
    case "$SHELL_NAME" in
        zsh)  RC_FILE="$HOME/.zshrc" ;;
        bash)
            # macOS uses .bash_profile, Linux uses .bashrc
            if [[ "$OSTYPE" == "darwin"* ]]; then
                RC_FILE="$HOME/.bash_profile"
            else
                RC_FILE="$HOME/.bashrc"
            fi
            ;;
        fish) RC_FILE="$HOME/.config/fish/config.fish" ;;
        *)    RC_FILE="" ;;
    esac

    if [ -n "$RC_FILE" ]; then
        # Check if it's already in the rc file (just not loaded in current session)
        if grep -q '\.local/bin' "$RC_FILE" 2>/dev/null; then
            echo "   (Already in $RC_FILE, just not loaded in this session)"
            echo "   Run: source $RC_FILE"
        else
            echo ""
            read -p "   Add to $RC_FILE? [Y/n] " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
                echo '' >> "$RC_FILE"
                if [[ "$SHELL_NAME" == "fish" ]]; then
                    echo 'set -gx PATH $HOME/.local/bin $PATH' >> "$RC_FILE"
                else
                    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$RC_FILE"
                fi
                echo "   ✅ Added to $RC_FILE"
                echo "   Run: source $RC_FILE"
            else
                echo "   Skipped. Add manually:"
                echo "   export PATH=\"\$HOME/.local/bin:\$PATH\""
            fi
        fi
    else
        echo "   Unknown shell ($SHELL_NAME). Add this to your shell rc file:"
        echo "   export PATH=\"\$HOME/.local/bin:\$PATH\""
    fi
fi
