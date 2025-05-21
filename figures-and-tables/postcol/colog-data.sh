#!/bin/bash

# Set input and output
DOTFILE="figures-and-tables/postcol/colog-data.dot"
OUTBASE="figures-and-tables/postcol/colog-data"

# SVG output (vector, best for HTML)
dot -Tsvg "$DOTFILE" -o "${OUTBASE}.svg"

# PNG output (raster, good for presentations, can set size)
# dot -Tpng "$DOTFILE" -Gdpi=300 -o "${OUTBASE}.png"

# echo "Rendered: ${OUTBASE}.svg and ${OUTBASE}.png"
echo "Rendered: ${OUTBASE}.svg (svg is best for html)."

