#!/bin/sh

set `date`
case $1 in
	mardi) j=Tuesday;;
	mercredi) j=Wenesday;;
esac
case $3 in
	septembre) n=September;;
esac
echo $j $2 $n $4
