#!/usr/bin/env bash
# halemans-gen-password [email] (design_docs/milestone_7.md §4 D4): generates a
# random password, hashes it with the pwstore-fast replica and prints the
# plaintext once plus a ready-to-paste users.items fragment for the provision
# config. The plaintext goes to stdout only — never into a file.
set -euo pipefail

email="${1:-}"
password="$(pwgen -s 24 1)"
hash="$(halemans-hash-password "$password")"

echo "password:     $password"
echo "passwordHash: $hash"
if [ -n "$email" ]; then
    jq -nc --arg email "$email" --arg hash "$hash" \
        '{email: $email, passwordHash: $hash, roles: []}'
fi
