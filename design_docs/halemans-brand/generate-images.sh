#!/usr/bin/env bash
# Regenerates all PNG assets + favicon ICO from SVG masters.
# Requires: rsvg-convert, ImageMagick, and Space Grotesk + JetBrains Mono fonts.
set -euo pipefail
cd "$(dirname "$0")/.."
SRC=svg; DST=png
mkdir -p "$DST"
render() { rsvg-convert -w "$3" -h "$3" "$SRC/$1" -o "$DST/$2"; }
for v in color darkbg mono-ink mono-white; do
  render "halemans-glyph-$v.svg" "halemans-glyph-$v-1024.png" 1024
done
for s in 1024 512 256 192 180; do
  render halemans-app-icon.svg "halemans-app-icon-$s.png" "$s"
done
for s in 16 24 32 48 64; do
  render halemans-micromark-ondark.svg "halemans-micromark-$s.png" "$s"
done
rsvg-convert -w 432 -h 432 svg/halemans-adaptive-foreground.svg -o "$DST/halemans-adaptive-foreground-432.png"
rsvg-convert -w 432 -h 432 svg/halemans-adaptive-background.svg -o "$DST/halemans-adaptive-background-432.png"
rsvg-convert -w 1200 svg/halemans-og-card.svg -o "$DST/halemans-og-card-1200x630.png"
rsvg-convert -w 2400 svg/halemans-lockup-dark.svg  -o "$DST/halemans-lockup-dark.png"
rsvg-convert -w 2400 svg/halemans-lockup-light.svg -o "$DST/halemans-lockup-light.png"
# multi-size ICO: 16/32/48/64 (fixes single-16 ico)
convert "$DST"/halemans-micromark-{64,48,32,16}.png halemans-favicon.ico
echo "done"
