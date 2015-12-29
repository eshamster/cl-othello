#!/bin/bash

dir=`dirname ${0}`
ros run -- --version
ros build ${dir}/bench.ros -o ${dir}/bench
${dir}/bench
