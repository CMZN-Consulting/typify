# Installing Typify

The commands below apply to the four packaged readers when they appear on the [1.0.0 release](https://github.com/CMZN-Consulting/typify/releases/tag/v1.0.0). For the source prerelease `v1.0.0-rc.1`, follow the [contributor instructions](#contributors-run-from-source). The macOS and Windows sections describe vendor-documented first-run behavior. The observed Linux checks are identified below.

## Archives

One archive per platform, with names that carry no version:

| Platform | Oldest system supported | Archive |
| --- | --- | --- |
| Linux x86_64 | Ubuntu 22.04 and its glibc | `typify-linux-x86_64.tar.gz` |
| Linux aarch64 (ARM64) | Ubuntu 22.04 and its glibc | `typify-linux-aarch64.tar.gz` |
| macOS aarch64 (Apple silicon) | macOS 15 | `typify-macos-aarch64.tar.gz` |
| Windows x86_64 | Windows Server 2022 | `typify-windows-x86_64.zip` |

No other platform has an archive. Each archive unpacks to a directory `typify/` holding the program, `typify/share/typify.typify` (the output of `typify emit Typify`), `typify/LICENSE.md` (the license of Typify and the notices of the Racket runtime inside the archive) and `typify/MANIFEST.txt`. No Racket needs to be installed.

## Download

The newest release's archive is at `https://github.com/CMZN-Consulting/typify/releases/latest/download/ASSET`. A script or a project that must run the same program every time uses the versioned URL `https://github.com/CMZN-Consulting/typify/releases/download/vX.Y.Z/ASSET` and checks the hashes below.

```sh
curl -fLO https://github.com/CMZN-Consulting/typify/releases/download/vX.Y.Z/typify-linux-x86_64.tar.gz
```

On Windows, PowerShell has `curl.exe`:

```powershell
curl.exe -fLO https://github.com/CMZN-Consulting/typify/releases/download/vX.Y.Z/typify-windows-x86_64.zip
```

## Unpack and run

Verify the archive first (next section). On Linux and macOS:

```sh
tar -xpzf typify-linux-x86_64.tar.gz
typify/bin/typify --version
```

On Windows, in PowerShell:

```powershell
Expand-Archive typify-windows-x86_64.zip -DestinationPath .
.\typify\bin\typify.exe --version
```

The program is `typify/bin/typify` (`typify/bin/typify.exe` on Windows). `--version` prints the version and the reader-source digest on one line, for example `1.0.0 sha256:` followed by 64 hexadecimal digits; `--version --json` gives the `version` and `build` fields. A checkout of the same commit prints the same digest (see [Contributors](#contributors-run-from-source)).

Keep the `typify/` directory whole: the program runs from the files under it, and a copy of `bin/typify` alone stops with `failed to start`. The whole directory can be moved. To run `typify` by name, put `typify/bin` on your `PATH`. The commands are described in the project's wiki page Command line.

## Verify a download

Each archive carries `typify/MANIFEST.txt`, one line per other file in the archive, sorted by path: `sha256`, mode (four octal digits, `0755` or `0644`) and path from the archive root, separated by two spaces. A release publishes, for each platform archive, the archive hash (the sha256 of the archive) and the manifest hash (the sha256 of its `MANIFEST.txt`). Both are subjects of the release's attestation and are listed in its notes.

**Attestation.** With the GitHub CLI, signed in to GitHub (`gh auth login`, or a token in `GH_TOKEN`; without a sign-in and without `--bundle` the command stops before it checks anything):

```sh
gh attestation verify typify-linux-x86_64.tar.gz --repo CMZN-Consulting/typify
gh attestation verify typify/MANIFEST.txt --repo CMZN-Consulting/typify
```

The command exits with status 0 when an attestation issued by a workflow of this repository names the file's sha256, and fails otherwise, for an altered file or another repository. `--format json` prints the verified attestation, including the commit the file was built from. The first command checks the archive; run the second from the directory that holds `typify/`, after unpacking, to check the manifest.

**By hand.** Compare the archive's sha256 with the hash from the attestation or from a pin you committed earlier: `sha256sum ARCHIVE` on Linux, `shasum -a 256 ARCHIVE` on macOS, `Get-FileHash -Algorithm SHA256 ARCHIVE` in PowerShell (it prints upper case; compare without regard to case). The release notes come from the same place as the archives, so a match with them shows only that the download is complete.

**The unpacked tree.** Run from the directory that holds `typify/`. On Linux and macOS, after `tar -xp` (without `-p`, a umask such as `077` clears mode bits and the mode check fails):

```sh
if command -v sha256sum >/dev/null; then
  sha256_of() { sha256sum "$1"; }
else
  sha256_of() { shasum -a 256 "$1"; }
fi
BAD=0
LISTED=0
[ -f typify/MANIFEST.txt ] || { echo "no typify/MANIFEST.txt here"; BAD=1; }
while read -r WANT MODE FILE; do
  LISTED=$((LISTED + 1))
  HAVE=$(sha256_of "$FILE" | cut -d ' ' -f 1)
  if [ "$HAVE" != "$WANT" ]; then
    echo "mismatch: $FILE"
    BAD=1
  elif [ -z "$(find "$FILE" -perm "$MODE")" ]; then
    echo "mode: $FILE"
    BAD=1
  fi
done < typify/MANIFEST.txt
[ "$LISTED" -gt 0 ] || BAD=1
FOUND=$(( $(find typify ! -type d ! -path typify/MANIFEST.txt | wc -l) ))
if [ "$FOUND" -ne "$LISTED" ]; then
  echo "files: $FOUND, listed: $LISTED"
  BAD=1
fi
if [ "$BAD" -eq 0 ]; then echo ok; else echo FAILED; fi
[ "$BAD" -eq 0 ]
```

It prints each missing file or one whose sha256 or mode differs, counts the entries under `typify/` against the manifest's lines, and ends with `ok` or `FAILED`. First check the manifest itself: verify its attestation, or compare `sha256sum typify/MANIFEST.txt` with the manifest hash from the attestation or your pin.

On Windows, in PowerShell, from the same directory (the files carry no modes, so every line of the manifest reads `0644` and only the sha256 column and the entry count are compared):

```powershell
(Get-FileHash -Algorithm SHA256 typify/MANIFEST.txt).Hash.ToLower()
$bad = 0
if (-not (Test-Path -LiteralPath typify/MANIFEST.txt -PathType Leaf)) { 'no typify/MANIFEST.txt here'; $bad = 1 }
$lines = @(Get-Content typify/MANIFEST.txt)
if ($lines.Count -eq 0) { $bad = 1 }
foreach ($line in $lines) {
  $want, $mode, $file = $line -split '  ', 3
  $have = (Get-FileHash -Algorithm SHA256 -LiteralPath $file -ErrorAction SilentlyContinue).Hash
  if ($have -ne $want) { "mismatch: $file"; $bad = 1 }
}
$manifest = (Get-Item -LiteralPath typify/MANIFEST.txt).FullName
$found = @(Get-ChildItem typify -Recurse -Force | Where-Object { (-not $_.PSIsContainer -or $_.LinkType) -and $_.FullName -ne $manifest }).Count
if ($found -ne $lines.Count) { "files: $found, listed: $($lines.Count)"; $bad = 1 }
if ($bad -eq 0) { 'ok' } else { 'FAILED' }
```

The Linux and macOS loop was run, unchanged, on a Linux archive built from this reader: it printed `ok` on a clean tree and `FAILED` with the named file for an altered file, a changed mode, an extra file and a removed file. The PowerShell version has not been run.

**Tags.** Release tags will be signed with the maintainer's key, which does not exist yet, so the tags of 1.0.x are unsigned; for a 1.0.x archive, the attestation is the check. Every tag the repository holds today is unsigned (`git verify-tag v1.0.0-rc.1` stops with `error: no signature found`). When a tag is signed, `git verify-tag vX.Y.Z` must exit with status 0 and `git rev-list -n 1 vX.Y.Z` must print the commit the attestation names; for an SSH signature `git verify-tag` exits with status 1 unless `gpg.ssh.allowedSignersFile` lists the maintainer's key, even when it prints `Good "git" signature`.

## What macOS and Windows may show

The programs in the 1.0 archives carry no publisher's code signature, and the macOS program is not notarized. Both systems may warn before the first run of such a program that came from a browser download. What they show for `typify` has not been observed; the release's clean-box run on macOS 15 and Windows Server 2022 will record it, and until then the paths below are what the vendors document, not what was seen.

- **macOS.** Apple's Platform Security guide says Gatekeeper "requests user approval before opening downloaded software for the first time". Apple Support's "Safely open apps on your Mac" describes opening an app that has not been notarized: try to open it, then open System Settings, Privacy & Security, scroll down and click Open Anyway. Removing the quarantine attribute from the unpacked directory is the other way: `xattr -dr com.apple.quarantine typify`.
- **Windows.** Microsoft's Smart App Control, on a Windows 11 machine where it is on, blocks an app that is unsigned. Microsoft's `Unblock-File` page says the cmdlet "removes the Zone.Identifier alternate data stream", the mark that files downloaded from the internet carry: `Unblock-File typify-windows-x86_64.zip` before unpacking, or `Get-ChildItem -Recurse typify | Unblock-File` on an unpacked directory. SmartScreen may also warn; its wording for `typify` is not recorded.

Whether a download made with `curl` or `curl.exe`, or the unpacking of an archive, leaves such a mark depends on the tool and the system; this guide does not promise either way.

## Contributors: run from source

Contributors and anyone who wants Typify before the release need Racket 9.3 CS. A user of the release archives does not.

```sh
raco pkg install --scope user --no-setup --deps fail --name typify --link ./src
racket src/main.rkt --version
racket src/main.rkt emit Typify
```

`racket src/main.rkt --version` prints the same version and reader-source digest as a build of the same sources. The tests, the grammar validator they need, and the format and leak checks are described in the [README](../README.md). The license check, `racket scripts/license-check.rkt`, fails closed on a file, a data file, a notice or a locked dependency that the inventory `scripts/license-inventory.json` does not cover; a change that adds or alters a data file adds its inventory entry. The optional [MCP server](../mcp/README.md) has its own setup guide.

## Pin Typify in a project

A project that runs Typify in its build, its CI or its own install commits a pin and installs only what matches it: the release's tag, the Typify commit the attestation names, and for each platform it supports the archive hash and the manifest hash. Its install takes the archive for its platform and stops on one the pin does not list, downloads it by the versioned URL, checks the archive's sha256 against the pin, unpacks it into a cache keyed by that hash, checks the manifest hash and every file against the manifest, and checks both again on every use. It never builds Typify and never chooses between downloading and building by what the machine has installed. A pin moves to a new release with hashes taken from that release's attestation, never from the release notes or a file beside the release.
