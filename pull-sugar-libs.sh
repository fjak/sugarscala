#!/bin/bash

# Not as sophisticated as pull-binary-libs.sh, but still better than grabbing
# dependencies manually

base=$(dirname "$0")
mkdir -p "$base/lib/extra"
cd "$base/lib/extra"
curl -LO http://files.fjak.de/sugarscala-v0.3.1.jar
curl -LO http://update.sugarj.org/plugins/org.spoofax.jsglr_1.2.0.201309171843.jar
curl -LO http://update.sugarj.org/plugins/org.spoofax.terms_1.2.0.201309171843.jar
curl -LO http://search.maven.org/remotecontent?filepath=commons-lang/commons-lang/2.6/commons-lang-2.6.jar
sha1sum -c ../../sugar-libs.sha1sum
