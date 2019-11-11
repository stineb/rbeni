#!/bin/bash

	##-----------------------------
	## Calculates global totals, integrating over gridcell area
	## argument 1: input file
	## argument 2: output file
	## argument 3: land area fraction file
	##-----------------------------
	cdo gridarea $1 gridarea.nc

	if [ -z "$3" ]
	then
		echo "Crating own mask..."
		cdo div $1 $1 mask.nc
		cdo -O selname,SIF mask.nc mask.nc
		cdo mul mask.nc gridarea.nc gridarea_masked.nc
	else
		echo "Using land mask provided as input..."
		cdo mul $3 gridarea.nc gridarea_masked.nc
	fi

	cdo mul gridarea_masked.nc $1 $2

	## remove temporary files
	rm gridarea.nc gridarea_masked.nc mask.nc
