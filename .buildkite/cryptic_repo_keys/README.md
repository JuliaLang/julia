## Cryptic repository keys

This folder contains RSA-encrypted symmetric AES keys.
These are used by buildkite agents to decrypt the secrets embedded within this repository.
Each buildkite agent contains an RSA secret key that is used to unlock the symmetric AES key that was used to encrypt the secrets within this repository.
For more information, see the [`cryptic` buildkite plugin repository](https://github.com/staticfloat/cryptic-buildkite-plugin).
