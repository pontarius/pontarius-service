#!/bin/sh
set -e

TMPFILE=tmp.xml
INTERFACEFILE=dbus-interface.xml

rm ${TMPFILE} || true
cabal run pontarius-service -- --write-interface ${TMPFILE}
emacs --batch -l ./format-interface.el ${TMPFILE} -f format-xml -f save-buffer
mv -f ${TMPFILE} ${INTERFACEFILE}
