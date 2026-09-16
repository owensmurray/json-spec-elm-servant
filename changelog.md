# Changelog

## 2.0.0.0

- Require `json-spec` >= 2.0 and adapt to its 2.0 API (`'Module` /
  `JsonModule`, `SpecJson`, `TupleDecoding`, and `TupleEncoding`).
- The major (epoch) version is bumped to 2.0 to indicate compatibility
  with the `json-spec` 2.0 major/epoch version.

## 0.6.0.1

- Relaxed the upper bound on `aeson` to allow version 2.3.

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
