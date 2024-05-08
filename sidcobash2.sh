#!/bin/bash

source /home/anaconda3/etc/profile.d/conda.sh

conda activate /home/condaenv/sidco

#echo "python pdCor.py $1 $2 $3 $4 $5 $6"

python /var/www/compLiMet/public_html/shiny/dev_site/sidco_partialcorr_test/pdCor.py "$1" "$2" "$3" $4 $5 $6

conda deactivate