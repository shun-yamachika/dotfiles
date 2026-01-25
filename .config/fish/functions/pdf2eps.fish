function pdf2eps
# 引数があればそれらをファイルリストとして使い、無ければ *.pdf を使う
if test (count $argv) -gt 0
set files $argv
else
set files *.pdf
end

for f in $files
if test -f "$f"
set out (string replace -r '\.pdf$' '.eps' -- $f)
if pdftops -eps "$f" "$out"
echo "OK: $f -> $out"
else
echo "FAILED: $f" >&2
end
else
echo "skip: $f (not a file)" >&2
end
end
end
