#!/bin/ksh

set -e
#set -e -x

echo $0

if [[ $# -ne 3  ]]; then
echo "ERROR: start from run_offl.ksh"
exit 1
fi

oppath=$1
cosppath=$2
ifiles=$3


if [[ ! -e $oppath ]]; then
mkdir $oppath
fi

cd $cosppath/pp

if [[ ! -e pp ]]; then
make
fi


for ifile in ${ifiles[*]}; do
./pp  <<EOF
&ppctl
ifile="$ifile"
opath="$oppath"
/
EOF
done




exit