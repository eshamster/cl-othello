#!/bin/bash

set -u

from_dir=~/in_git/Lisp/Othello

name=${2:-$1}

to_src=src/${name}.lisp
to_t=t/${name}.lisp

cat > ${to_src}<<EOF
(in-package :cl-user)
(defpackage cl-othello.${name}
  (:use :cl)
  (:export))
(in-package :cl-othello.${name})

EOF

cat > ${to_t}<<EOF
(in-package :cl-user)
(defpackage cl-othello-test.${name}
  (:use :cl
        :cl-othello.${name}
        :prove))
(in-package :cl-othello-test.${name})

EOF

cat ${from_dir}/$1.lisp >> ${to_src}
sed -e 's/prove://g' ${from_dir}/TEST/$1.lisp >> ${to_t} 

echo "make ${to_src} & ${to_t}"
