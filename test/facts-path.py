import sys

UNICODE_VERSION = "17.0.0"
UNICODE_SOURCE = "https://www.unicode.org/Public/17.0.0/ucd/extracted/DerivedGeneralCategory.txt"
UNICODE_SOURCE_SHA256 = "d62e5bab70ca74f099343f71224fa051cb1fdd61a1ab45c0488c44cfc0b6102e"
UNSAFE = (
    (0, 32),
    (127, 160),
    (173, 173),
    (1536, 1541),
    (1564, 1564),
    (1757, 1757),
    (1807, 1807),
    (2192, 2193),
    (2274, 2274),
    (5760, 5760),
    (6158, 6158),
    (8192, 8207),
    (8232, 8239),
    (8287, 8292),
    (8294, 8303),
    (12288, 12288),
    (55296, 57343),
    (65279, 65279),
    (65529, 65531),
    (69821, 69821),
    (69837, 69837),
    (78896, 78911),
    (113824, 113827),
    (119155, 119162),
    (917505, 917505),
    (917536, 917631),
)


def canonical(value):
    if len(value) > 4096 or not value.startswith("/"):
        return False
    if value == "/":
        return True
    parts = value[1:].split("/")
    return all(part not in ("", ".", "..") for part in parts) and not any(
        low <= ord(character) <= high
        for character in value
        for low, high in UNSAFE
    )


if __name__ == "__main__":
    raise SystemExit(0 if len(sys.argv) == 2 and canonical(sys.argv[1]) else 1)
