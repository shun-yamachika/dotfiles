function svg2eps
# 引数があればそれらをファイルリストとして使い、無ければ *.svg を使う
if test (count $argv) -gt 0
set files $argv
else
set files *.svg
end

for f in $files
if test -f "$f"
set out (string replace -r '\.svg$' '.eps' -- $f)
if inkscape --export-type=eps --export-filename="$out" "$f" 2>/dev/null
echo "OK: $f -> $out"
else
echo "FAILED: $f" >&2
end
else
echo "skip: $f (not a file)" >&2
end
end
end
