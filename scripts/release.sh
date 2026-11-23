#!/usr/bin/env bash
set -euo pipefail

usage() {
  printf '%s\n' \
    'usage: scripts/release.sh build [--platform NAME] [--output DIR] [--root DIR] [--racket-installer FILE]' \
    '       scripts/release.sh racket [--platform NAME] [--root DIR] [--racket-installer FILE]' >&2
  exit 2
}

RACKET_BASE_URL=https://download.racket-lang.org/releases/9.3/installers

racket_pin() {
  case "$1" in
    linux-x86_64) printf '%s %s %s\n' racket-9.3-x86_64-linux-buster-cs.sh 30dc59d9b5af083eacf793253d35782c5231e8bfb7a7331fe32ccd9a41fd792f 272724156 ;;
    linux-aarch64) printf '%s %s %s\n' racket-9.3-aarch64-linux-buster-cs.sh 26e4d1c3bf8954c4413fb6737af5d211d44c1e158e34e4c90e0af1cde6353c9a 264306763 ;;
    macos-aarch64) printf '%s %s %s\n' racket-9.3-aarch64-macosx-cs.dmg 21f4937ca0bf37c31e60c1d77f5fd20f175e3ad51f6197f27149055bc222e559 278994444 ;;
    windows-x86_64) printf '%s %s %s\n' racket-9.3-x86_64-win32-cs.exe 5bea9560f8bc6c9d31d55b52a2f83fb5947e57cf625e1865e6091a428c13fe60 194479288 ;;
    *) printf 'unknown platform: %s\n' "$1" >&2; exit 2 ;;
  esac
}

host_platform() {
  case "$(uname -s)-$(uname -m)" in
    Linux-x86_64) echo linux-x86_64 ;;
    Linux-aarch64 | Linux-arm64) echo linux-aarch64 ;;
    Darwin-arm64) echo macos-aarch64 ;;
    MINGW*-x86_64 | MSYS*-x86_64 | CYGWIN*-x86_64) echo windows-x86_64 ;;
    *) printf 'this host has no release platform: %s\n' "$(uname -s)-$(uname -m)" >&2; exit 2 ;;
  esac
}

sha256_of() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | cut -d ' ' -f 1
  else
    shasum -a 256 "$1" | cut -d ' ' -f 1
  fi
}

native_path() {
  if [ "$PLATFORM" = windows-x86_64 ]; then cygpath -w "$1"; else printf '%s\n' "$1"; fi
}

[ "$#" -ge 1 ] || usage
COMMAND=$1
shift
case "$COMMAND" in build | racket) ;; *) usage ;; esac

PLATFORM=
OUTPUT=
ROOT=
RACKET_INSTALLER=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --platform) PLATFORM=${2:?}; shift 2 ;;
    --output) OUTPUT=${2:?}; shift 2 ;;
    --root) ROOT=${2:?}; shift 2 ;;
    --racket-installer | --racket-archive) RACKET_INSTALLER=${2:?}; shift 2 ;;
    *) usage ;;
  esac
done

HOST=$(host_platform)
PLATFORM=${PLATFORM:-$HOST}
if [ "$PLATFORM" != "$HOST" ]; then
  printf 'the host is %s, not %s\n' "$HOST" "$PLATFORM" >&2
  exit 2
fi
read -r RACKET_NAME RACKET_SHA256 RACKET_BYTES <<EOF
$(racket_pin "$PLATFORM")
EOF

posix_path() {
  if [ "$PLATFORM" = windows-x86_64 ] && [ -n "$1" ]; then cygpath -u "$1"; else printf '%s\n' "$1"; fi
}

REPOSITORY=$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
OUTPUT=$(posix_path "$OUTPUT")
ROOT=$(posix_path "$ROOT")
RACKET_INSTALLER=$(posix_path "$RACKET_INSTALLER")
ROOT=${ROOT:-/tmp/typify-release}
OUTPUT=${OUTPUT:-$REPOSITORY/release}
MARK=.typify-release-root

if [ -e "$ROOT" ]; then
  if [ -f "$ROOT/$MARK" ]; then
    rm -rf -- "$ROOT"
  else
    printf 'refusing to replace %s, which this script did not make\n' "$ROOT" >&2
    exit 2
  fi
fi
mkdir -p -- "$ROOT"
: >"$ROOT/$MARK"

file_bytes() { wc -c <"$1" | tr -d ' '; }

install_racket() {
  local installer=$1 destination=$2 status=0
  case "$PLATFORM" in
    linux-*)
      sh "$installer" --create-dir --in-place --dest "$destination" >&2
      ;;
    macos-*)
      local mount=$ROOT/mount candidate
      local found=()
      mkdir -p -- "$mount"
      hdiutil attach -readonly -nobrowse -mountpoint "$mount" "$installer" >&2
      for candidate in "$mount"/*/; do
        if [ ! -L "${candidate%/}" ] && [ -x "${candidate}bin/racket" ] && [ -x "${candidate}bin/raco" ]; then found+=("${candidate%/}"); fi
      done
      if [ "${#found[@]}" -eq 1 ]; then
        ditto "${found[0]}" "$destination" || status=$?
      else
        printf 'the disk image holds %s directories with bin/racket and bin/raco, not one\n' "${#found[@]}" >&2
        status=1
      fi
      local tries=0
      until hdiutil detach "$mount" >&2; do
        tries=$((tries + 1))
        if [ "$tries" -ge 3 ]; then
          hdiutil detach -force "$mount" >&2 || status=1
          break
        fi
        sleep 5
      done
      [ "$status" -eq 0 ] || exit 1
      ;;
    windows-*)
      TYPIFY_INSTALLER=$(cygpath -w "$installer") TYPIFY_DESTINATION=$(cygpath -w "$destination") \
        powershell -NoProfile -NonInteractive -Command \
          '$p = Start-Process -FilePath $env:TYPIFY_INSTALLER -ArgumentList ("/S /D=" + $env:TYPIFY_DESTINATION) -Wait -PassThru -ErrorAction Stop; exit $p.ExitCode' >&2
      ;;
  esac
}

if [ -z "$RACKET_INSTALLER" ]; then
  mkdir -p -- "$ROOT/download"
  RACKET_INSTALLER=$ROOT/download/$RACKET_NAME
  curl --fail --silent --show-error --location --retry 3 --proto-redir '=https' --proto '=https' --tlsv1.2 \
    "$RACKET_BASE_URL/$RACKET_NAME" --output "$RACKET_INSTALLER"
fi
ACTUAL=$(sha256_of "$RACKET_INSTALLER")
ACTUAL_BYTES=$(file_bytes "$RACKET_INSTALLER")
if [ "$ACTUAL" != "$RACKET_SHA256" ] || [ "$ACTUAL_BYTES" != "$RACKET_BYTES" ]; then
  printf 'the Racket installer %s has sha256 %s and %s bytes, not the pinned %s and %s bytes\n' \
    "$RACKET_INSTALLER" "$ACTUAL" "$ACTUAL_BYTES" "$RACKET_SHA256" "$RACKET_BYTES" >&2
  exit 1
fi
install_racket "$RACKET_INSTALLER" "$ROOT/racket"
if [ "$PLATFORM" = windows-x86_64 ]; then
  RACKET=$ROOT/racket/Racket.exe
else
  RACKET=$ROOT/racket/bin/racket
fi
[ -x "$RACKET" ] || { printf 'the Racket installation holds no %s\n' "$RACKET" >&2; exit 1; }

unset PLTCOLLECTS PLTCOMPILEDROOTS PLTADDONDIR PLTUSERHOME PLT_ZO_DISABLE PLTCONFIGDIR PLT_CS_DEBUG
export PLTUSERHOME="$(native_path "$ROOT/home")"
export PLTADDONDIR="$(native_path "$ROOT/addon")"
mkdir -p -- "$ROOT/home" "$ROOT/addon"

if [ "$COMMAND" = racket ]; then
  printf '%s\n' "$RACKET"
  exit 0
fi

COMMIT=$(git -C "$REPOSITORY" rev-parse --verify 'HEAD^{commit}')
TREE=$(git -C "$REPOSITORY" rev-parse --verify 'HEAD^{tree}')
EPOCH=$(git -C "$REPOSITORY" log -1 --format=%ct HEAD)
mkdir -p -- "$ROOT/typify"
git -C "$REPOSITORY" -c core.autocrlf=false -c core.eol=lf archive --format=tar HEAD -- src LICENSE.md | tar -xf - -C "$ROOT/typify"
mkdir -p -- "$OUTPUT"

"$RACKET" "$(native_path "$REPOSITORY/scripts/release.rkt")" build \
  --root "$(native_path "$ROOT")" \
  --platform "$PLATFORM" \
  --output "$(native_path "$OUTPUT")" \
  --commit "$COMMIT" \
  --tree "$TREE" \
  --epoch "$EPOCH" \
  --racket-installer "$RACKET_NAME" \
  --racket-sha256 "$RACKET_SHA256" \
  --checkout "$(native_path "$REPOSITORY")"
