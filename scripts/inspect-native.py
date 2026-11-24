#!/usr/bin/env python3
"""Inspect the native content of one Typify release archive, read-only.

    python3 -I inspect-native.py ASSET --sha256 HEX [--artifact ZIP --artifact-sha256 HEX]
                                 [--reference FOLDER]

ASSET is a typify-PLATFORM.tar.gz or typify-windows-x86_64.zip. Nothing is
unpacked to disk: the archive is read into memory and every member is parsed
there. The archive must hash to --sha256, or nothing is inspected. With
--artifact, the Actions artifact zip must hash to --artifact-sha256 and hold a
member named like ASSET whose bytes equal ASSET's.

One JSON receipt goes to stdout. It holds no absolute path and no time, so the
same inputs give the same bytes. The facts are kept apart:

  shipped       every regular file in the archive, with its format
  embedded      PE images found inside a PE file (Windows embeds its DLLs)
  linked        what each native image's own headers ask the loader for:
                PE import and delay-import DLL names, Mach-O load commands,
                ELF DT_NEEDED
  runtimePaths  Racket's own record, in the program, of each runtime path
                that names a native library: the module, the library name,
                and the archive file the build made for it (null when none).
                This is what Racket's code will try to load at run time.
  strings       library names that appear only as text in a native image,
                at most a dynamic load; inferred, never a link. Only ASCII
                is scanned and compressed bytes are not read, so an absence
                here proves nothing.
  origins       with --reference FOLDER (reference.sh fills one from the
                pinned installers): the known file each shipped file equals
                by sha256, and the known file found whole at each embedded
                image's offset.

Exit status: 0 on a receipt, 1 on a pin mismatch, 2 on bad usage.
Standard library only.
"""

import argparse
import hashlib
import io
import json
import os
import re
import struct
import sys
import tarfile
import zipfile


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def cstring(data, offset, limit):
    end = data.find(b"\0", offset, limit)
    if end < 0:
        end = limit
    return data[offset:end].decode("latin-1")


# ---------------------------------------------------------------- PE

PE_MACHINES = {0x8664: "x86_64", 0xAA64: "aarch64", 0x14C: "i386", 0x1C4: "arm"}


def pe_header(data, base):
    """Parse the PE image starting at file offset base, or return None."""
    if data[base : base + 2] != b"MZ" or base + 0x40 > len(data):
        return None
    lfanew = struct.unpack_from("<I", data, base + 0x3C)[0]
    pe = base + lfanew
    if lfanew < 0x40 or lfanew > 0x10000 or pe + 24 > len(data):
        return None
    if data[pe : pe + 4] != b"PE\0\0":
        return None
    machine, nsections = struct.unpack_from("<HH", data, pe + 4)
    optsize, characteristics = struct.unpack_from("<HH", data, pe + 20)
    opt = pe + 24
    if opt + optsize > len(data) or nsections == 0 or nsections > 96:
        return None
    magic = struct.unpack_from("<H", data, opt)[0]
    if magic == 0x20B:
        count_at, dirs_at = 108, 112
    elif magic == 0x10B:
        count_at, dirs_at = 92, 96
    else:
        return None
    if optsize < dirs_at:
        return None
    size_of_headers = struct.unpack_from("<I", data, opt + 60)[0]
    ndirs = min(struct.unpack_from("<I", data, opt + count_at)[0], 16, (optsize - dirs_at) // 8)
    dirs = [struct.unpack_from("<II", data, opt + dirs_at + 8 * i) for i in range(ndirs)]
    sections = []
    table = opt + optsize
    if table + 40 * nsections > len(data):
        return None
    for i in range(nsections):
        s = table + 40 * i
        vsize, va, rawsize, rawptr = struct.unpack_from("<IIII", data, s + 8)
        sections.append((va, max(vsize, rawsize), rawptr, rawsize))
    extent = max([size_of_headers] + [p + n for (_, _, p, n) in sections if n])
    if len(dirs) > 4 and dirs[4][0]:  # the certificate table is a file offset
        extent = max(extent, dirs[4][0] + dirs[4][1])
    symbols, nsymbols = struct.unpack_from("<II", data, pe + 12)
    if symbols and nsymbols:  # a COFF symbol table, then its string table, ends a MinGW file
        strings_at = base + symbols + 18 * nsymbols
        if strings_at + 4 <= len(data):
            extent = max(extent, symbols + 18 * nsymbols + struct.unpack_from("<I", data, strings_at)[0])
    return dict(
        base=base,
        machine=PE_MACHINES.get(machine, hex(machine)),
        dll=bool(characteristics & 0x2000),
        dirs=dirs,
        sections=sections,
        extent=min(extent, len(data) - base),
    )


def pe_offset(h, rva):
    for va, span, rawptr, _ in h["sections"]:
        if va <= rva < va + span:
            return h["base"] + rawptr + (rva - va)
    return None


def pe_name(data, h, rva):
    at = pe_offset(h, rva) if rva else None
    if at is None or at >= len(data):
        return None
    return cstring(data, at, min(len(data), at + 256))


def pe_export_name(data, h):
    if not h["dirs"] or not h["dirs"][0][0]:
        return None
    at = pe_offset(h, h["dirs"][0][0])
    if at is None or at + 40 > len(data):
        return None
    return pe_name(data, h, struct.unpack_from("<I", data, at + 12)[0])


def pe_imports(data, h):
    names = []
    if len(h["dirs"]) > 1 and h["dirs"][1][0]:
        at = pe_offset(h, h["dirs"][1][0])
        while at is not None and at + 20 <= len(data):
            entry = struct.unpack_from("<5I", data, at)
            if not any(entry):
                break
            name = pe_name(data, h, entry[3])
            if name:
                names.append(name)
            at += 20
    return names


def pe_delay_imports(data, h):
    names = []
    if len(h["dirs"]) > 13 and h["dirs"][13][0]:
        at = pe_offset(h, h["dirs"][13][0])
        while at is not None and at + 32 <= len(data):
            entry = struct.unpack_from("<8I", data, at)
            if not any(entry):
                break
            name = pe_name(data, h, entry[1]) if entry[0] & 1 else None
            if name:
                names.append(name)
            at += 32
    return names


def pe_describe(data, h):
    return dict(
        machine=h["machine"],
        dll=h["dll"],
        exportName=pe_export_name(data, h),
        imports=pe_imports(data, h),
        delayImports=pe_delay_imports(data, h),
    )


def pe_embedded(data, outer):
    """Every PE image that starts inside the file at a nonzero offset."""
    found = []
    at = data.find(b"MZ", 1)
    while at >= 0:
        h = pe_header(data, at)
        if h is not None:
            image = data[at : at + h["extent"]]
            row = dict(offset=at, bytes=h["extent"], sha256=sha256(image))
            row.update(pe_describe(data, h))
            found.append((h, row))
            at = data.find(b"MZ", at + max(h["extent"], 2))
        else:
            at = data.find(b"MZ", at + 1)
    return found


# ---------------------------------------------------------------- Mach-O

MACHO_CPUS = {0x100000C: "arm64", 0x1000007: "x86_64", 12: "arm", 7: "i386"}
MACHO_TYPES = {1: "object", 2: "execute", 6: "dylib", 8: "bundle", 7: "dylinker"}
MACHO_DYLIB_COMMANDS = {
    0xC: "LC_LOAD_DYLIB",
    0xD: "LC_ID_DYLIB",
    0x20: "LC_LAZY_LOAD_DYLIB",
    0x80000018: "LC_LOAD_WEAK_DYLIB",
    0x8000001F: "LC_REEXPORT_DYLIB",
    0x80000023: "LC_LOAD_UPWARD_DYLIB",
}


def macho_slice(data, base):
    magic = data[base : base + 4]
    if magic == b"\xcf\xfa\xed\xfe":
        end, size = "<", 32
    elif magic == b"\xce\xfa\xed\xfe":
        end, size = "<", 28
    elif magic == b"\xfe\xed\xfa\xcf":
        end, size = ">", 32
    elif magic == b"\xfe\xed\xfa\xce":
        end, size = ">", 28
    else:
        return None
    cputype, _, filetype, ncmds, sizeofcmds = struct.unpack_from(end + "iiIII", data, base + 4)
    row = dict(
        cpu=MACHO_CPUS.get(cputype, hex(cputype)),
        filetype=MACHO_TYPES.get(filetype, filetype),
        id=None,
        loads=[],
        rpaths=[],
        dylinker=None,
        codeSignature=False,
    )
    at, stop = base + size, min(len(data), base + size + sizeofcmds)
    for _ in range(ncmds):
        if at + 8 > stop:
            break
        cmd, cmdsize = struct.unpack_from(end + "II", data, at)
        if cmdsize < 8:
            break
        if cmd in MACHO_DYLIB_COMMANDS or cmd in (0x8000001C, 0xE):
            name = cstring(data, at + struct.unpack_from(end + "I", data, at + 8)[0], at + cmdsize)
            if cmd == 0xD:
                row["id"] = name
            elif cmd == 0x8000001C:
                row["rpaths"].append(name)
            elif cmd == 0xE:
                row["dylinker"] = name
            else:
                row["loads"].append(dict(command=MACHO_DYLIB_COMMANDS[cmd], name=name))
        elif cmd == 0x1D:
            row["codeSignature"] = True
        at += cmdsize
    return row


def macho(data):
    if data[:4] in (b"\xca\xfe\xba\xbe", b"\xca\xfe\xba\xbf"):
        wide = data[3] == 0xBF
        count = struct.unpack_from(">I", data, 4)[0]
        if not 0 < count < 30:
            return None
        slices = []
        for i in range(count):
            if wide:
                _, _, offset, _, _, _ = struct.unpack_from(">iiQQII", data, 8 + 32 * i)
            else:
                _, _, offset, _, _ = struct.unpack_from(">iiIII", data, 8 + 20 * i)
            s = macho_slice(data, offset)
            if s is None:
                return None
            slices.append(s)
        return slices
    s = macho_slice(data, 0)
    return None if s is None else [s]


# ---------------------------------------------------------------- ELF

ELF_MACHINES = {62: "x86_64", 183: "aarch64", 3: "i386", 40: "arm"}
ELF_TYPES = {1: "relocatable", 2: "executable", 3: "shared", 4: "core"}


def elf(data):
    if data[:4] != b"\x7fELF" or len(data) < 64:
        return None
    if data[4] != 2 or data[5] != 1:
        return dict(machine=None, type=None, needed=None, note="not ELF64 little-endian; not parsed")
    etype, machine = struct.unpack_from("<HH", data, 16)
    phoff = struct.unpack_from("<Q", data, 32)[0]
    phentsize, phnum = struct.unpack_from("<HH", data, 54)
    loads, dynamic = [], None
    for i in range(phnum):
        p = phoff + i * phentsize
        if p + 56 > len(data):
            break
        ptype, _, offset, vaddr, _, filesz, _, _ = struct.unpack_from("<IIQQQQQQ", data, p)
        if ptype == 1:
            loads.append((vaddr, offset, filesz))
        elif ptype == 2:
            dynamic = (offset, filesz)
    row = dict(
        machine=ELF_MACHINES.get(machine, machine),
        type=ELF_TYPES.get(etype, etype),
        needed=[],
        soname=None,
        runpath=None,
    )
    if dynamic is None:
        return row
    entries, strtab = [], None
    for at in range(dynamic[0], min(len(data), dynamic[0] + dynamic[1]) - 15, 16):
        tag, value = struct.unpack_from("<qQ", data, at)
        if tag == 0:
            break
        if tag == 5:
            strtab = value
        entries.append((tag, value))
    base = next((o + strtab - v for v, o, n in loads if strtab is not None and v <= strtab < v + n), None)
    if base is None:
        return row
    for tag, value in entries:
        text = cstring(data, base + value, min(len(data), base + value + 4096))
        if tag == 1:
            row["needed"].append(text)
        elif tag == 14:
            row["soname"] = text
        elif tag in (15, 29):
            row["runpath"] = text
    return row


# ---------------------------------------------------------------- strings

STRING_PATTERNS = {
    "pe": re.compile(rb"[A-Za-z0-9_+-][A-Za-z0-9_.+-]{0,79}\.[dD][lL][lL]"),
    "macho": re.compile(rb"[A-Za-z0-9_@/.+-]{0,160}lib[A-Za-z0-9_.+-]{1,80}\.dylib"),
    "elf": re.compile(rb"lib[A-Za-z0-9_+-]{1,80}\.so(?:\.[0-9]+)*(?![A-Za-z0-9_])"),
}


# Racket's record of each native library a runtime path names, as the embedder
# writes it: ((#%embedded:MODULE: so "NAME" [VERSIONS]) RESOLVED ...), where
# RESOLVED is #f when no file of the archive was made for it.
RUNTIME_SO = re.compile(
    rb'\(\(\|?#%embedded:([^|:\s]+):\|? so "([^"]+)"((?: \([^()]*\))*)\) (#f|#"[^"]*")'
)


def runtime_paths(data):
    rows = {
        (m.group(1).decode("latin-1"), m.group(2).decode("latin-1"), m.group(3).decode("latin-1").strip(),
         m.group(4).decode("latin-1"))
        for m in RUNTIME_SO.finditer(data)
    }
    return [dict(module=a, so=b, versions=c or None, resolved=None if d == "#f" else d[2:-1])
            for a, b, c, d in sorted(rows)]


def named_as_strings(data, form, regions):
    """Library names found as text, each assigned to the region it lies in.

    regions is a list of (label, start, end); text outside every region is
    labelled "outside any image".
    """
    out = {}
    for match in STRING_PATTERNS[form].finditer(data):
        at = match.start()
        label = next((r for r, s, e in regions if s <= at < e), "outside any image")
        out.setdefault(label, set()).add(match.group().decode("latin-1"))
    return {k: sorted(v) for k, v in sorted(out.items())}


# ---------------------------------------------------------------- archive

def members(blob, asset):
    if asset.endswith(".zip"):
        with zipfile.ZipFile(io.BytesIO(blob)) as z:
            for info in z.infolist():
                unix = info.create_system == 3
                mode = (info.external_attr >> 16) & 0o7777 if unix else None
                if info.is_dir():
                    yield info.filename.rstrip("/"), "dir", mode, None
                else:
                    yield info.filename, "file", mode, z.read(info)
    elif asset.endswith(".tar.gz"):
        with tarfile.open(fileobj=io.BytesIO(blob), mode="r:gz") as t:
            for m in t.getmembers():
                if m.isreg():
                    yield m.name, "file", m.mode & 0o7777, t.extractfile(m).read()
                elif m.isdir():
                    yield m.name, "dir", m.mode & 0o7777, None
                else:
                    kind = {tarfile.SYMTYPE: "symlink", tarfile.LNKTYPE: "hardlink"}.get(m.type, repr(m.type))
                    yield m.name, kind, m.mode & 0o7777, None
    else:
        raise SystemExit(f"inspect-native: unknown archive type: {asset}")


def load_reference(folder):
    """Every file under folder, keyed by the folder's own name and the path below it."""
    files, top = {}, os.path.basename(os.path.normpath(folder))
    for root, dirs, names in os.walk(folder):
        dirs.sort()
        for n in sorted(names):
            full = os.path.join(root, n)
            with open(full, "rb") as f:
                files[top + "/" + os.path.relpath(full, folder).replace(os.sep, "/")] = f.read()
    return files


def inspect(blob, asset, reference=None):
    shipped, embedded, linked, strings, origins, runtime = [], [], {}, {}, {}, {}
    ref_by_sha = {}
    for rel, content in (reference or {}).items():
        ref_by_sha.setdefault(sha256(content), []).append(rel)
    for path, kind, mode, data in members(blob, asset):
        row = dict(path=path, kind=kind, mode=None if mode is None else f"{mode:04o}")
        if data is None:
            shipped.append(row)
            continue
        row.update(bytes=len(data), sha256=sha256(data), format=None)
        for rel in ref_by_sha.get(row["sha256"], []):
            origins.setdefault(path, []).append(dict(path=rel, bytes=len(data), sha256=row["sha256"], match="same file"))
        outer = pe_header(data, 0)
        slices = None if outer else macho(data)
        elf_row = None if outer or slices else elf(data)
        if outer or slices or elf_row:
            found = runtime_paths(data)
            if found:
                runtime[path] = found
        if outer:
            described = pe_describe(data, outer)
            row.update(format="pe", machine=described["machine"], dll=described["dll"])
            linked[path] = dict(imports=described["imports"], delayImports=described["delayImports"])
            regions = []
            for h, image in pe_embedded(data, outer):
                name = image["exportName"] or f"image@{image['offset']}"
                embedded.append(dict(host=path, **image))
                at = image["offset"]
                for rel, content in (reference or {}).items():
                    if data[at : at + len(content)] == content:
                        origins.setdefault(f"{path}!{name}", []).append(
                            dict(path=rel, bytes=len(content), sha256=sha256(content), match="whole file at the image's offset"))
                linked[f"{path}!{name}"] = dict(imports=image["imports"], delayImports=image["delayImports"])
                regions.append((f"{path}!{name}", image["offset"], image["offset"] + image["bytes"]))
            regions.append((path, 0, outer["extent"]))
            strings.update(named_as_strings(data, "pe", regions))
        elif slices:
            row.update(format="macho", slices=[dict(cpu=s["cpu"], filetype=s["filetype"]) for s in slices])
            linked[path] = [
                dict(cpu=s["cpu"], id=s["id"], loads=s["loads"], rpaths=s["rpaths"],
                     dylinker=s["dylinker"], codeSignature=s["codeSignature"])
                for s in slices
            ]
            strings.update(named_as_strings(data, "macho", [(path, 0, len(data))]))
        elif elf_row:
            row.update(format="elf", machine=elf_row["machine"], elfType=elf_row["type"])
            linked[path] = {k: v for k, v in elf_row.items() if k in ("needed", "soname", "runpath", "note")}
            strings.update(named_as_strings(data, "elf", [(path, 0, len(data))]))
        shipped.append(row)
    result = dict(shipped=shipped, embedded=embedded, linked=linked, strings=strings, runtimePaths=runtime)
    if reference is not None:
        result["origins"] = origins
        result["reference"] = {rel: dict(bytes=len(c), sha256=sha256(c)) for rel, c in reference.items()}
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("asset")
    parser.add_argument("--sha256", required=True)
    parser.add_argument("--artifact")
    parser.add_argument("--artifact-sha256")
    parser.add_argument("--reference", help="a folder of known files, such as libraries from a pinned installer; "
                        "a shipped file with the same sha256, or an embedded image whose bytes begin with a whole "
                        "known file, is named by that file's path in the folder")
    args = parser.parse_args()
    if bool(args.artifact) != bool(args.artifact_sha256):
        parser.error("--artifact and --artifact-sha256 go together")
    name = os.path.basename(args.asset)
    with open(args.asset, "rb") as f:
        blob = f.read()
    digest = sha256(blob)
    if digest != args.sha256.lower():
        print(f"inspect-native: {name} is sha256 {digest}, not the pinned {args.sha256}", file=sys.stderr)
        return 1
    receipt = dict(
        tool=dict(name="inspect-native.py", sha256=sha256(open(__file__, "rb").read())),
        asset=dict(name=name, bytes=len(blob), sha256=digest, pinned=True),
    )
    if args.artifact:
        with open(args.artifact, "rb") as f:
            outer = f.read()
        outer_digest = sha256(outer)
        expected = args.artifact_sha256.lower().removeprefix("sha256:")
        if outer_digest != expected:
            print(f"inspect-native: the artifact is sha256 {outer_digest}, not the pinned {expected}", file=sys.stderr)
            return 1
        with zipfile.ZipFile(io.BytesIO(outer)) as z:
            if name not in z.namelist() or z.read(name) != blob:
                print(f"inspect-native: the artifact holds no member {name} equal to the asset", file=sys.stderr)
                return 1
            listing = sorted(z.namelist())
        receipt["artifact"] = dict(sha256=outer_digest, members=listing, assetMemberEqual=True)
    receipt.update(inspect(blob, name, load_reference(args.reference) if args.reference else None))
    json.dump(receipt, sys.stdout, indent=2, sort_keys=True)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
