#!/bin/bash

source /home/anaconda3/etc/profile.d/conda.sh

conda activate /home/condaenv/sidco

#echo "python dCor.py $1 $2 $3 $4 $5 $6 $7 $8"
echo "/var/www/compLiMet/public_html/shiny/sidco/dCor.py $1 $2 $3 $4 $5 $6 $7 $8" > /var/www/compLiMet/public_html/shiny/sidco/test/$(date +%Y%m%d_%H%M%S).txt

python /var/www/compLiMet/public_html/shiny/dev_site/sidco_partialcorr_test/dCor_pearson_spearmanOct162022.py "$1" "$2" "$3" $4 $5 $6 $7 $8

conda deactivate 
