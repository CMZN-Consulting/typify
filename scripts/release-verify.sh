#!/usr/bin/env bash
set -euo pipefail

usage() {
  printf '%s\n' 'usage: scripts/release-verify.sh --platform NAME --builds DIR --racket FILE --fixture DIR --output DIR' >&2
  exit 2
}

PLATFORM=
BUILDS=
RACKET=
FIXTURE=
OUTPUT=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --platform) PLATFORM=${2:?}; shift 2 ;;
    --builds) BUILDS=${2:?}; shift 2 ;;
    --racket) RACKET=${2:?}; shift 2 ;;
    --fixture) FIXTURE=${2:?}; shift 2 ;;
    --output) OUTPUT=${2:?}; shift 2 ;;
    *) usage ;;
  esac
done
[ -n "$PLATFORM" ] && [ -n "$BUILDS" ] && [ -n "$RACKET" ] && [ -n "$FIXTURE" ] && [ -n "$OUTPUT" ] || usage

REPOSITORY=$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
case "$(uname -s)" in
  MINGW* | MSYS* | CYGWIN*) WINDOWS=1 ;;
  *) WINDOWS=0 ;;
esac
posix_path() { if [ "$WINDOWS" = 1 ]; then cygpath -u "$1"; else printf '%s\n' "$1"; fi; }
native_path() { if [ "$WINDOWS" = 1 ]; then cygpath -w "$1"; else printf '%s\n' "$1"; fi; }
BUILDS=$(posix_path "$BUILDS")
FIXTURE_POSIX=$(posix_path "$FIXTURE")
# The workflow pairs a physical C: directory on Windows with /Volumes/C on POSIX.
BASELINE=$FIXTURE_POSIX/v12
EXTENSION=$FIXTURE_POSIX/extension
BASELINE_NATIVE=$(native_path "$BASELINE")
OUTPUT=$(posix_path "$OUTPUT")
mkdir -p -- "$OUTPUT"
OUTPUT=$(cd -P -- "$OUTPUT" && pwd -P)
WORK=$(mktemp -d)
trap 'rm -rf -- "$WORK"' EXIT

sha256_of() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum <"$1" | cut -d ' ' -f 1
  else
    shasum -a 256 <"$1" | cut -d ' ' -f 1
  fi
}

asset_of() {
  case "$1" in
    windows-x86_64) echo typify-windows-x86_64.zip ;;
    linux-x86_64 | linux-aarch64 | macos-aarch64) echo "typify-$1.tar.gz" ;;
    *) printf 'unknown platform: %s\n' "$1" >&2; exit 2 ;;
  esac
}

program_of() { if [ "$1" = windows-x86_64 ]; then echo typify/bin/typify.exe; else echo typify/bin/typify; fi; }

unpack() {
  rm -rf -- "$2"
  mkdir -p -- "$2"
  case "$1" in
    *.zip) powershell -NoProfile -NonInteractive -Command "\$ProgressPreference = 'SilentlyContinue'; Expand-Archive -LiteralPath '$(native_path "$1")' -DestinationPath '$(native_path "$2")'" ;;
    *) (cd "$2" && tar -xpzf "$1") ;;
  esac
}

rkt() { "$RACKET" "$(native_path "$REPOSITORY/$1")" "${@:2}"; }

FAILURES=0
pass() { printf 'pass  %s\n' "$1" | tee -a "$OUTPUT/summary.txt"; }
fail() { printf 'FAIL  %s\n' "$1" | tee -a "$OUTPUT/summary.txt"; FAILURES=$((FAILURES + 1)); }
expect() {
  local label=$1 want=$2 log=$3
  shift 3
  local status=0
  "$@" >"$OUTPUT/$log" 2>"$OUTPUT/$log.stderr" || status=$?
  if { [ "$want" = 0 ] && [ "$status" = 0 ]; } || { [ "$want" = refuse ] && [ "$status" = 1 ]; }; then
    pass "$label"
  else
    fail "$label (exit $status, see $log)"
  fi
}

: >"$OUTPUT/summary.txt"
ASSET=$(asset_of "$PLATFORM")
PROGRAM=$(program_of "$PLATFORM")
if [ "$WINDOWS" = 1 ]; then PYTHON=python; else PYTHON=python3; fi
EPOCH=
for REPLICA in a b; do
  DIRECTORY=$BUILDS/build-$PLATFORM-$REPLICA
  ARCHIVE=$DIRECTORY/$ASSET
  if [ ! -f "$ARCHIVE" ]; then
    fail "build $REPLICA is present"
    continue
  fi
  WANT=$(cut -d ' ' -f 1 "$ARCHIVE.sha256")
  MANIFEST=$(sha256_of "$DIRECTORY/$PLATFORM.MANIFEST.txt")
  EPOCH=$(rkt scripts/release.rkt epoch "$(native_path "$DIRECTORY/$PLATFORM.build.json")")
  cp -- "$DIRECTORY/$PLATFORM.build.json" "$OUTPUT/build-$REPLICA.json"
  expect "build $REPLICA: archive hash, manifest tree and architecture" 0 "verify-$REPLICA.json" \
    rkt scripts/release.rkt verify "$(native_path "$ARCHIVE")" --platform "$PLATFORM" --sha256 "$WANT" --manifest-sha256 "$MANIFEST"
  expect "build $REPLICA: inspect native archive content" 0 "native-inspect-$REPLICA.json" \
    "$PYTHON" -I "$(native_path "$REPOSITORY/scripts/inspect-native.py")" "$(native_path "$ARCHIVE")" --sha256 "$WANT"
  expect "build $REPLICA: no embedded runtime native library paths" 0 "native-runtime-paths-$REPLICA.json" \
    "$PYTHON" -I -c 'import json, pathlib, sys; d = json.loads(pathlib.Path(sys.argv[1]).read_bytes()); print(json.dumps({"runtimePaths": d["runtimePaths"]})); sys.exit(0 if d["runtimePaths"] == {} else 1)' \
    "$(native_path "$OUTPUT/native-inspect-$REPLICA.json")"
  expect "build $REPLICA: native library inventory and notices match archive" 0 "native-license-$REPLICA.json" \
    rkt scripts/license-check.rkt --release --root "$(native_path "$REPOSITORY")" --native-receipt "$(native_path "$OUTPUT/native-inspect-$REPLICA.json")"
  unpack "$ARCHIVE" "$WORK/$REPLICA"
  expect "build $REPLICA: license provenance, packed LICENSE and manifest" 0 "license-$REPLICA.json" \
    rkt scripts/license-check.rkt --release --root "$(native_path "$REPOSITORY")" --archive "$(native_path "$WORK/$REPLICA")"
  expect "build $REPLICA: native $PROGRAM --version" 0 "version-$REPLICA.txt" "$WORK/$REPLICA/$PROGRAM" --version
done

A=$BUILDS/build-$PLATFORM-a/$ASSET
B=$BUILDS/build-$PLATFORM-b/$ASSET
if [ -f "$A" ] && [ -f "$B" ]; then
  rkt scripts/release.rkt compare "$(native_path "$A")" "$(native_path "$B")" --platform "$PLATFORM" >"$OUTPUT/compare-builds.json"
  if cmp -s "$A" "$B"; then
    printf 'note  builds a and b are byte-identical\n' | tee -a "$OUTPUT/summary.txt"
  else
    printf 'note  builds a and b differ in bytes (reported, not blocking)\n' | tee -a "$OUTPUT/summary.txt"
  fi
fi

for OTHER in linux-x86_64 linux-aarch64 macos-aarch64 windows-x86_64; do
  [ "$OTHER" = "$PLATFORM" ] && continue
  FOREIGN=$BUILDS/build-$OTHER-a/$(asset_of "$OTHER")
  if [ ! -f "$FOREIGN" ]; then
    fail "the $OTHER archive is present for the wrong-platform control"
    continue
  fi
  cp -- "$FOREIGN" "$WORK/$OTHER-as-$ASSET"
  expect "the $OTHER archive named $ASSET is refused by its executables' headers" refuse "foreign-$OTHER.json" \
    rkt scripts/release.rkt architecture "$(native_path "$WORK/$OTHER-as-$ASSET")" --platform "$PLATFORM"
done

if [ -f "$A" ]; then
  WANT=$(cut -d ' ' -f 1 "$A.sha256")
  MANIFEST=$(sha256_of "$BUILDS/build-$PLATFORM-a/$PLATFORM.MANIFEST.txt")
  expect "four wrong archives: each refused by archive hash and by manifest tree" 0 canaries.json \
    rkt scripts/release.rkt canaries "$(native_path "$A")" --platform "$PLATFORM" --sha256 "$WANT" \
    --manifest-sha256 "$MANIFEST" --epoch "$EPOCH" --output "$(native_path "$OUTPUT/canaries")"
fi

if [ -e "$BASELINE" ] || [ -L "$BASELINE" ] || [ -e "$EXTENSION" ] || [ -L "$EXTENSION" ]; then
  printf 'fixture paths already exist; allocate a fresh fixture directory\n' >&2
  exit 2
fi
mkdir -p -- "$BASELINE"
cp -R "$REPOSITORY/test/corpus/." "$BASELINE/"
FROZEN=$REPOSITORY/test/release-fixtures
MANIFEST_PIN=$(cat "$FROZEN/stage4-manifest.sha256")
for REPLICA in a b; do
  [ -x "$WORK/$REPLICA/$PROGRAM" ] || continue
  EXECUTABLE=$(native_path "$WORK/$REPLICA/$PROGRAM")
  TRANSCRIPT=$(native_path "$OUTPUT/transcript-$REPLICA.json")
  expect "build $REPLICA: capture the baseline and extension on $PLATFORM" 0 "target-$REPLICA.json" \
    "$PYTHON" "$(native_path "$REPOSITORY/scripts/capture-release-corpus.py")" "$EXECUTABLE" "$BASELINE_NATIVE" "$TRANSCRIPT" \
    --extension-manifest "$(native_path "$FROZEN/stage4-manifest.json")" --extension-manifest-sha256 "$MANIFEST_PIN" \
    --extension-archive "$(native_path "$FROZEN/stage4.tar")" --extension-root "$(native_path "$EXTENSION")" --target "$PLATFORM"
  expect "build $REPLICA: independently compare the extension semantics" 0 "extension-$REPLICA.json" \
    "$PYTHON" "$(native_path "$REPOSITORY/scripts/check-release-extension.py")" "$EXECUTABLE" \
    "$(native_path "$FROZEN/stage4.tar")" "$(native_path "$WORK/extension-detail-$REPLICA.json")"
  expect "build $REPLICA: compare native baseline results with frozen expectations" 0 "semantic-$REPLICA.json" \
    rkt scripts/release-corpus.rkt "$BASELINE_NATIVE" "$TRANSCRIPT"
done
if cmp -s "$OUTPUT/transcript-a.json" "$OUTPUT/transcript-b.json"; then
  pass "builds a and b: combined native transcripts byte-identical"
else
  fail "builds a and b: combined native transcripts byte-identical"
fi

if [ -f "$REPOSITORY/test/test-inputs.rkt" ]; then
  expect "racket test/test-inputs.rkt on this host" 0 test-inputs.log \
    bash -c 'cd "$1" && "$2" test/test-inputs.rkt' _ "$REPOSITORY" "$RACKET"
else
  fail "racket test/test-inputs.rkt on this host (the file is missing)"
fi

if [ "$FAILURES" = 0 ]; then
  printf 'verify %s: ok\n' "$PLATFORM" | tee -a "$OUTPUT/summary.txt"
else
  printf 'verify %s: %s failed\n' "$PLATFORM" "$FAILURES" | tee -a "$OUTPUT/summary.txt"
  exit 1
fi
