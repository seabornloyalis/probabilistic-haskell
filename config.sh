#!/bin/bash

#
if [ -n $1 ] && [ "$1" = "R" ];
then
	`cp ProbabR.hs Probab.hs`
	`cp SolutionsR.hs Solutions.hs`       
elif [ -n $1 ] && [ "$1" = "FP" ];
then
	`cp ProbabFP.hs Probab.hs`
	`cp SolutionsFP.hs Solutions.hs`
else
	echo "Call with R for rational, exact implementation"
	echo "Call with FP for floating-point implementation"
fi
