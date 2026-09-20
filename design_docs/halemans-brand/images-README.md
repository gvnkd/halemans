# Halemans brand assets

Masters are the SVG files in `svg/` — edit those, never the PNGs.
Regenerate all raster assets with `scripts/generate-images.sh` (requires
rsvg-convert, plus Space Grotesk & JetBrains Mono fonts installed).

| File | Use |
|---|---|
| halemans-glyph-color.svg | default symbol, light backgrounds |
| halemans-glyph-darkbg.svg | symbol on dark surfaces (beams #A9B5C1) |
| halemans-glyph-mono-ink / -white.svg | single-color contexts (print, embroidery) |
| halemans-lockup-*.svg | marketing, docs headers, presentation title slides |
| halemans-micromark-*.svg | favicons, browser tabs, avatars ≤ 32 px |
| halemans-app-icon.svg | iOS / macOS / generic app icon source |
| halemans-adaptive-*.svg | Android adaptive icon layers (108 dp) |
| halemans-og-card.svg | OpenGraph / social preview (1200×630) |

Rules of thumb: full glyph ≥ 24 px; below that use the micro-mark.
The ping-ring is part of the symbol in every variant. Severity pip
colors (#E5484D #F5A524 #4C8DFF #98A2AD, top to bottom) are semantic —
never recolor them. See design_docs/halemans-brand-guidelines.md.

## Files to place in the repo

- `svg/*` → `images/svg/`
- `png/*` → `images/png/`
- `halemans-favicon.ico` → `images/` (replaces the old 16-only ICO)
- `site.webmanifest` → `images/` (adjust paths if served elsewhere;
  in this repo the LIVE manifest is `static/manifest.webmanifest` —
  icon paths there are app-root absolute, keep the two in sync)
- `generate-images.sh` → `scripts/`
- `images-README.md` → `images/README.md`
- `halemans-brand-guidelines.md` → `design_docs/`
- `halemans-brand-tokens-patch.txt` → apply to `design_docs/halemans-brand-tokens.txt` §1
- `web-head-snippet.html` → paste into the app's HTML head
