#!/bin/sh

if [ "$(id -u)" -ne 0 ]; then
    echo "Root is required"
    exit
fi

TEMPFILE=tempfile

echo ">> Benchmarking location $(pwd)"

echo ">> Testing write speed"
dd if=/dev/zero of=$TEMPFILE bs=1M count=1024 conv=fdatasync,notrunc status=progress

echo ">> Testing read speed (from disk)"
echo 3 > /proc/sys/vm/drop_caches
dd if=$TEMPFILE of=/dev/null bs=1M count=1024 status=progress

echo ">> Testing read speed (buffer-cache)"
dd if=$TEMPFILE of=/dev/null bs=1M count=1024 status=progress

echo ">> Cleaning"
rm $TEMPFILE
