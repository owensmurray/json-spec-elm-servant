# Changelog

## 0.6.0.0

- Require `json-spec` >= 1.4 and `json-spec-elm` >= 0.6, which provide
  `JsonDict` support (Elm `Dict String a`) via `json-spec-elm`.
- The test API now exercises `JsonDict` in `DashboardData`.

## 0.5.1.0

- Improved Elm generator output.
- Relaxed dependency bounds for `containers` and `time`.

## 0.5.0.0

- Added support for GHC 9.14.
  - This release is expected to be compatible with the `0.4.4.x` series in
    most practical cases.
  - The major version bump is required because exposed type class instances
    and their constraints changed, which is a technical API change.
