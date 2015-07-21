#!/bin/bash

csi -I ../.. << EOF
(require-library margs)
(load "../../common.scm")
(load "../../common_records.scm")
(load "../../margs.scm")
(load "../../megatest-version.scm")
(load "../../portlogger.scm")
(load "../../tasks.scm")
(load "../../db.scm")
(load "../../configf.scm")
(load "../../keys.scm")
(load "../../tree.scm")
(load "../../multi-dboard.scm")
EOF
