#!/bin/sh

rm /dev/random
ln -s /dev/urandom /dev/random

echo "starting pontarius"

/sbin/my_init -- $@
