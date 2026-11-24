#!/usr/bin/env bash
set -euo pipefail

usage() {
  printf '%s\n' 'usage: scripts/docs-smoke.sh --archive FILE --sha256 HEX --manifest-sha256 HEX [--tampered DIR] [--install-page FILE] [--readme FILE] [--work DIR]' >&2
  exit 2
}

ARCHIVE=
WANT_ARCHIVE=
WANT_MANIFEST=
TAMPERED=
REPOSITORY=$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
INSTALL_PAGE=$REPOSITORY/docs/install.md
README=$REPOSITORY/README.md
WORK=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --archive) ARCHIVE=${2:?}; shift 2 ;;
    --sha256) WANT_ARCHIVE=${2:?}; shift 2 ;;
    --manifest-sha256) WANT_MANIFEST=${2:?}; shift 2 ;;
    --tampered) TAMPERED=${2:?}; shift 2 ;;
    --install-page) INSTALL_PAGE=${2:?}; shift 2 ;;
    --readme) README=${2:?}; shift 2 ;;
    --work) WORK=${2:?}; shift 2 ;;
    *) usage ;;
  esac
done
[ -n "$ARCHIVE" ] && [ -n "$WANT_ARCHIVE" ] && [ -n "$WANT_MANIFEST" ] || usage
ARCHIVE=$(cd -P -- "$(dirname -- "$ARCHIVE")" && pwd -P)/$(basename -- "$ARCHIVE")
WORK=${WORK:-$(mktemp -d)}
mkdir -p -- "$WORK"
WORK=$(cd -P -- "$WORK" && pwd -P)
if [ -n "$TAMPERED" ]; then
  TAMPERED=$(cd -P -- "$TAMPERED" && pwd -P)
fi

case "$(uname -s)" in
  MINGW* | MSYS* | CYGWIN*) WINDOWS=1; PROGRAM=typify/bin/typify.exe ;;
  *) WINDOWS=0; PROGRAM=typify/bin/typify ;;
esac

FAILURES=0
pass() { printf 'pass  %s\n' "$1"; }
fail() { printf 'FAIL  %s\n' "$1"; FAILURES=$((FAILURES + 1)); }
note() { printf 'note  %s\n' "$1"; }

sha256_of() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum <"$1" | cut -d ' ' -f 1
  else
    shasum -a 256 <"$1" | cut -d ' ' -f 1
  fi
}

native_path() {
  if [ "$WINDOWS" = 1 ]; then cygpath -w "$1"; else printf '%s\n' "$1"; fi
}

fenced_block() {
  awk -v want="$2" -v marker="$3" '
    /^```/ {
      if (inside) {
        if (index(body, marker) > 0 && !done) { printf "%s", body; done = 1 }
        inside = 0; body = ""; next
      }
      language = substr($0, 4)
      if (language == want) { inside = 1; body = "" }
      next
    }
    inside { body = body $0 "\n" }
  ' "$1"
}

unpack() {
  rm -rf -- "$2"
  mkdir -p -- "$2"
  case "$1" in
    *.zip) powershell -NoProfile -NonInteractive -Command "\$ProgressPreference = 'SilentlyContinue'; Expand-Archive -LiteralPath '$(native_path "$1")' -DestinationPath '$(native_path "$2")'" ;;
    *) (cd "$2" && tar -xpzf "$1") ;;
  esac
}

tree_check() {
  local directory=$1 manifest
  [ -f "$directory/typify/MANIFEST.txt" ] || return 1
  manifest=$(sha256_of "$directory/typify/MANIFEST.txt")
  [ "$manifest" = "$WANT_MANIFEST" ] || return 1
  if [ "$WINDOWS" = 1 ]; then
    local output
    output=$(cd "$directory" && powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "$(native_path "$WORK/check.ps1")" | tr -d '\r')
    printf '%s\n' "$output" | sed 's/^/      /'
    [ "$(printf '%s\n' "$output" | sed -n 1p)" = "$WANT_MANIFEST" ] && [ "$(printf '%s\n' "$output" | tail -n 1)" = ok ]
  else
    (cd "$directory" && bash "$WORK/check.sh") | sed 's/^/      /'
    return "${PIPESTATUS[0]}"
  fi
}

runnable() {
  local found
  found=$(type -P "$1" 2>/dev/null) && [ -n "$found" ] && [ -x "$found" ]
}

racket_trace() {
  local name
  for name in PLTCOLLECTS PLTADDONDIR PLTUSERHOME PLTCONFIGDIR PLTCOMPILEDROOTS; do
    [ -z "${!name:-}" ] || return 0
  done
  for name in /usr/share/racket /usr/racket /usr/local/racket "$HOME/.local/share/racket" "$HOME/Library/Racket" \
    /Applications/Racket* "/c/Program Files/Racket" "${APPDATA:-/nonexistent}/Racket"; do
    [ ! -e "$name" ] || return 0
  done
  return 1
}

if runnable racket || runnable raco || runnable racket.exe || runnable raco.exe || racket_trace; then
  fail "Racket is absent from this image"
else
  pass "Racket is absent from this image"
fi

if [ "$(sha256_of "$ARCHIVE")" = "$WANT_ARCHIVE" ]; then pass "archive hash"; else fail "archive hash"; fi

HAVE_CHECK=0
if [ -f "$INSTALL_PAGE" ]; then
  if [ "$WINDOWS" = 1 ]; then
    fenced_block "$INSTALL_PAGE" powershell 'Get-FileHash -Algorithm SHA256 -LiteralPath' >"$WORK/check.ps1"
    [ -s "$WORK/check.ps1" ] && HAVE_CHECK=1
  else
    fenced_block "$INSTALL_PAGE" sh 'sha256_of "$FILE"' >"$WORK/check.sh"
    [ -s "$WORK/check.sh" ] && HAVE_CHECK=1
  fi
fi
if [ "$HAVE_CHECK" = 1 ]; then pass "the install page documents the tree check"; else fail "the install page documents the tree check ($INSTALL_PAGE)"; fi

unpack "$ARCHIVE" "$WORK/genuine"
if [ "$HAVE_CHECK" = 1 ]; then
  if tree_check "$WORK/genuine"; then pass "documented tree check accepts the release"; else fail "documented tree check accepts the release"; fi
fi

VERSION_OUTPUT=$(cd "$WORK/genuine" && "./$PROGRAM" --version) && VERSION_STATUS=0 || VERSION_STATUS=$?
if [ "$VERSION_STATUS" = 0 ] && [ -n "$VERSION_OUTPUT" ]; then
  pass "typify --version: $VERSION_OUTPUT"
else
  fail "typify --version exited $VERSION_STATUS"
fi

if (cd "$WORK/genuine" && "./$PROGRAM" check typify/share/typify.typify --json >"$WORK/self-check.json"); then
  pass "the installed reader accepts its self-declaration"
else
  fail "the installed reader refused its self-declaration"
fi

CATALOG=$WORK/catalog
mkdir -p -- "$CATALOG"
fenced_block "$README" typify 'declare : Catalog' >"$CATALOG/catalog-language.typify"
fenced_block "$README" typify 'item : journal' >"$CATALOG/catalog.typify"
if ! grep -Fxq 'typify check catalog-language.typify Catalog catalog.typify --json' "$README"; then
  fail "the README shows the catalog check command"
else
  # Run the documented command with the freshly unpacked distribution on PATH.
  # On Windows an executable ending in .exe is also found by its plain name.
  OUTPUT=$(cd "$CATALOG" && PATH="$WORK/genuine/typify/bin:$PATH" typify check catalog-language.typify Catalog catalog.typify --json) && STATUS=0 || STATUS=$?
  EXPECTED_TERMS=$(sed -n 's/.*The `terms` field is `\(.*\)`[.].*/\1/p' "$README" | head -n 1)
  if [ "$STATUS" = 0 ] && [ -n "$EXPECTED_TERMS" ] && printf '%s' "$OUTPUT" | grep -Fq "\"terms\":$EXPECTED_TERMS"; then
    pass "README catalog check prints $EXPECTED_TERMS"
  else
    fail "README catalog check: exit $STATUS, output $OUTPUT"
  fi
fi

if [ -n "$TAMPERED" ]; then
  ASSET=$(basename -- "$ARCHIVE")
  for KIND in flipped-byte extra-file missing-file rewritten-manifest; do
    NAME=$KIND-$ASSET
    CANARY=$TAMPERED/$NAME
    if [ ! -f "$CANARY" ]; then
      fail "$NAME is present"
      continue
    fi
    if [ "$(sha256_of "$CANARY")" = "$WANT_ARCHIVE" ]; then
      fail "$NAME: the archive hash refuses it"
    else
      pass "$NAME: the archive hash refuses it"
    fi
    if [ "$HAVE_CHECK" = 1 ]; then
      if ! unpack "$CANARY" "$WORK/canary" >/dev/null 2>&1; then
        fail "$NAME: it unpacks, so the documented tree check can judge it"
      elif tree_check "$WORK/canary"; then
        fail "$NAME: the documented tree check refuses it"
      else
        pass "$NAME: the documented tree check refuses it"
      fi
    fi
  done
fi

if [ "$FAILURES" = 0 ]; then
  printf 'docs smoke: ok\n'
else
  printf 'docs smoke: %s failed\n' "$FAILURES"
  exit 1
fi
