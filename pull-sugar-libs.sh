#!/bin/bash

# Not as sophisticated as pull-binary-libs.sh, but still better than grabbing
# dependencies manually

base=$(dirname "$0")
mkdir -p "$base/lib/extra"
cd "$base/lib/extra"
curl -LO http://files.fjak.de/scala-sugar-20131211.jar
curl -LO http://update.sugarj.org/plugins/org.spoofax.jsglr_1.2.0.201309171843.jar
curl -LO http://update.sugarj.org/plugins/org.spoofax.terms_1.2.0.201309171843.jar
curl -LO https://kiama.googlecode.com/files/kiama_2.10-1.5.1.jar
curl -LO http://search.maven.org/remotecontent?filepath=org/bitbucket/inkytonik/dsinfo/dsinfo_2.10/0.2.0/dsinfo_2.10-0.2.0.jar
curl -LO http://search.maven.org/remotecontent?filepath=org/bitbucket/inkytonik/dsprofile/dsprofile_2.10/0.2.0/dsprofile_2.10-0.2.0.jar
sha1sum -c ../../sugar-libs.sha1sum
