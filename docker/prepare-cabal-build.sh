#!/bin/sh
# Prepares a nix-less cabal build, replicating what the IHP flake module does
# in NixSupport/default.nix:
#   1. build-generated-code: Application/Schema.sql -> build/Generated/*.hs
#   2. refreshes the exposed-modules list in halemans.cabal between the
#      BEGIN/END GENERATED MODULES markers
#   3. writes the RunJobs / script entry-point wrappers to build/exe/
# Run from the repository root. Requires build-generated-code on PATH
# (cabal install ihp-schema-compiler:exe:build-generated-code).
set -eu

cd "$(dirname "$0")/.."

export IHP_RELATION_SUPPORT=0
build-generated-code

mkdir -p build/exe

# Isolate entry points from app sources: with hs-source-dirs "." GHC --make
# would recompile (and shadow) library modules from source instead of using
# the halemans library package.
cp Main.hs build/exe/RunProdServer.hs

cat > build/exe/RunJobs.hs <<'EOF'
module Main (main) where
import Application.Script.Prelude
import IHP.ScriptSupport
import IHP.Job.Runner
import qualified Config
import WorkerMain ()
main :: IO ()
main = runScript Config.config (runJobWorkers (workers RootApplication))
EOF

for script in EnqueuePollers GenPassword HalemansMcp; do
    cat > "build/exe/${script}.hs" <<EOF
module Main (main) where
import IHP.ScriptSupport
import qualified Config
import Application.Script.${script} (run)
main = runScript Config.config run
EOF
done

{
    find Application Web -name '*.hs'
    find Config -name '*.hs' | sed 's|^Config/||'
    echo WorkerMain.hs
} | sed 's|\.hs$||; s|/|.|g' | sort > build/.modules-lib
(cd build && find Generated -name '*.hs' | sed 's|\.hs$||; s|/|.|g' | sort) > build/.modules-generated

awk '
    /-- BEGIN GENERATED MODULES/ {
        print
        while ((getline line < "build/.modules-lib") > 0) print "        " line
        while ((getline line < "build/.modules-generated") > 0) print "        " line
        skip = 1
        next
    }
    /-- END GENERATED MODULES/ { skip = 0; print; next }
    !skip
' halemans.cabal > halemans.cabal.new

mv halemans.cabal.new halemans.cabal
