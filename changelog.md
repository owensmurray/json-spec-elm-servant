# Changelog

## 0.5.0.0

- Added support for GHC 9.14.
  - This release is expected to be compatible with the `0.4.4.x` series in
    most practical cases.
  - The major version bump is required because exposed type class instances
    and their constraints changed, which is a technical API change.
