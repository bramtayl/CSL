#!/bin/bash
set -e

# Word can't display the raw PDF images pandoc embeds by default (hence
# "This image can't be displayed"), so point includegraphics at the existing
# .svg figures instead. Requires rsvg-convert (apt install librsvg2-bin) so
# pandoc can generate the PNG fallback Word needs alongside the SVG.
sed -E 's/(\\includegraphics\[width=\\maxwidth\]\{figure\/[^}]+)\}/\1.svg}/' CSL.tex > CSL_docx.tex

# pandoc doesn't understand \makecell, so flatten \makecell[l]{a\\b} to
# "a \newline b" before conversion.
sed -i -E 's/\\makecell\[l\]\{([^{}]*)\\\\([^{}]*)\}/\1 \\newline \2/g' CSL_docx.tex

pandoc CSL_docx.tex --from=latex --to=docx --output=CSL.docx --bibliography=cost_of_life.bib --citeproc

rm CSL_docx.tex