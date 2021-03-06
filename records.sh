#! /bin/bash

# extents caches extents calculated on draw
# proc is called on draw and takes the obj itself as a parameter
# attrib is an alist of parameters
# libs: hash of name->lib, insts: hash of instname->inst
#
# Add -safe when doing development
#
export MODE='-safe'
(echo ";; Created by records.sh. DO NOT EDIT THIS FILE. Edit records.sh instead"
make-vector-record $MODE vg lib     comps
make-vector-record $MODE vg comp    objs name file
make-vector-record $MODE vg obj     type pts fill-color text line-color call-back angle font attrib extents proc
make-vector-record $MODE vg inst    libname compname theta xoff yoff scalex scaley mirrx mirry call-back cache
make-vector-record $MODE vg drawing libs insts scalex scaley xoff yoff cnv cache
) > vg_records.scm

