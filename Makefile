# JS_FILES/CSS_FILES bundles (static/prod.js|css) were removed in milestone
# 12 §8: Layout.hs serves individual versioned assets from static/vendor/
# via assetPath, nothing references the prod bundles, and the variables
# pointed at stale ${IHP}-bundled vendor versions.
include ${IHP}/Makefile.dist
