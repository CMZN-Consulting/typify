set -eu
LC_ALL=C
export LC_ALL

json_string() {
    JSON_VALUE=$1 awk '
        BEGIN {
            value = ENVIRON["JSON_VALUE"]
            printf "\""
            for (i = 0; i < 32; i++) controls[sprintf("%c", i)] = i
            for (i = 1; i <= length(value); i++) {
                character = substr(value, i, 1)
                if (character == "\"") printf "\\\""
                else if (character == "\\") printf "\\\\"
                else if (character == "\b") printf "\\b"
                else if (character == "\f") printf "\\f"
                else if (character == "\n") printf "\\n"
                else if (character == "\r") printf "\\r"
                else if (character == "\t") printf "\\t"
                else if (character in controls) printf "\\u%04x", controls[character]
                else printf "%s", character
            }
            printf "\""
        }'
}

emit() {
    printf '{"args":[%s],"fact":%s' "$path_json" "$(json_string "$1")" >> "$rows"
    if [ "$#" -eq 2 ]; then
        printf ',"value":%s' "$(json_string "$2")" >> "$rows"
    fi
    printf '}\n' >> "$rows"
}

if [ "${1-}" = --entry ]; then
    [ "$#" -eq 3 ] || exit 2
    path=$2
    rows=$3
    path_json=$(json_string "$path")
    if metadata=$(stat -c '%a %u' -- "$path" 2>/dev/null); then
        :
    elif metadata=$(stat -f '%Lp %u' "$path" 2>/dev/null); then
        :
    else
        : > "$rows.failed"
        exit 1
    fi
    set -- $metadata
    mode=$(printf '%04s' "$1" | tr ' ' 0)
    owner=$2
    emit exists
    if [ -L "$path" ]; then
        if target=$(readlink "$path" && printf '.'); then
            target=${target%.}
            target=${target%?}
            emit symlink "$target"
        else
            : > "$rows.failed"
            exit 1
        fi
    elif [ -d "$path" ]; then
        emit directory
    fi
    emit mode "$mode"
    emit owner "$owner"
    exit 0
fi

[ "$#" -eq 1 ] || exit 2
case $1 in
    /*) candidate=$1 ;;
    *) candidate=$(pwd -P)/$1 ;;
esac
root=$(FACT_ROOT=$candidate awk '
    BEGIN {
        count = split(ENVIRON["FACT_ROOT"], parts, "/")
        depth = 0
        for (i = 1; i <= count; i++) {
            if (parts[i] == "" || parts[i] == ".") continue
            if (parts[i] == "..") { if (depth > 0) depth--; continue }
            stack[++depth] = parts[i]
        }
        if (depth == 0) printf "/"
        for (i = 1; i <= depth; i++) printf "/%s", stack[i]
    }')
temporary=$(mktemp -d "${TMPDIR:-/tmp}/typify-facts.XXXXXXXX")
trap 'rm -rf "$temporary"' 0 HUP INT TERM
rows=$temporary/rows
: > "$rows"
if ! find "$root" -exec sh "$0" --entry '{}' "$rows" \;; then
    exit 1
fi
[ ! -f "$rows.failed" ] || exit 1
printf '{"collector":"typify 1.0","facts":['
sort "$rows" | awk 'BEGIN { first = 1 } { if (!first) printf ","; printf "%s", $0; first = 0 }'
printf '],"root":%s}\n' "$(json_string "$root")"
