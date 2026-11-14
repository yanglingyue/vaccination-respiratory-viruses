#!/bin/csh

cd /mnt/data1/yly/multi_pathogen/vaccine_virus_interaction/rEDM_02_vaccine_postcovid

mkdir -p 00_loop_script


set ss_list = (1  12 25) 
set aa_list = (11 24 35)

#set ss_list = (8 9 10) 
#set aa_list = (8 9 10)

@ i = 1
while ($i <= $#ss_list)
    set ss = $ss_list[$i]
    set aa = $aa_list[$i]

    echo ${ss} "---" ${aa}

    sed -e 's/SSSSSS/'${ss}'/g' \
        -e 's/AAAAAA/'${aa}'/g' \
        03_main_temp.Rmd > 00_loop_script/03_main_${ss}_${aa}.Rmd
    
   chmod +x 00_loop_script/03_main_${ss}_${aa}.Rmd

   nohup R -e "rmarkdown::render('00_loop_script/03_main_${ss}_${aa}.Rmd')" >& 00_loop_script/03_main_log_${ss}_${aa} &

    
    @ i++
end

echo "Scripts generation completed."
