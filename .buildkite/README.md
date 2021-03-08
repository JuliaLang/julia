# All

Install:

- jq
- Git
- Bash
- AWS CLI

# Linux

- https://github.com/rr-debugger/rr/wiki/Zen

Install:

- Docker

# Windows

Install:

- Docker


# Secrets

All secrets have prefix `buildkite.` (e.g. `buildkite.secret_key`)

- `mac_keychain_password`
- `mac_codesign_identity`
- `mac_appleid_password`
- `aws_access_key_id` (has S3 permissions)
- `aws_secret_access_key` (has S3 permissions)
- `gpg_key` (Binary private key, Base64-encoded)
- `documenter_key`
