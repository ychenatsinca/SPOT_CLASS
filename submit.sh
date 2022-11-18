#!/bin/bash

echo "running batch scripts to creat job files from template.job"
cd /lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/vivian_code/
# LOAD R MODULE 
#module load r/3.1.1

#echo "removing previous jobs!"
#rm -f R_* 
# /home/orchidee03/jryder/cru_regridded2 
# submit the job on que 
# chmod u+x $CONFIG_FILE
# qsub -jeo -q short $CONFIG_FILE
CONT=0

# argunment 1 to 6 are parameters for the function lai_change_analysis.R
#fun.dyn.lai <- function ( Ref.wind=3.0, Ref.rain=100, Win.size=40, offdays=30, pdays=60)
 
# arg1 : EXPERINMENTS 
# arg2 : start year & end year
# arg3 : start day & end day 
# arg4 :

#declare -A arg_arr
#case 00
#arg_arr=()
#arg_arr+=('12 30 50 0 50 TRACK_DATA_2D')
#arg_arr+=('12 30 50 0 50 TRACK_DATA_2D')




#for iwind in 12 14 16  

#do 
#arg1="5"   # wind speed
#arg1="${iwind}"

#for irainf in 20 
#do 

#arg2="80" #rainf 
#arg2="${irainf}"  
#arg3="50" #window size 
#arg4="0" #minimum offset 

# post days
#for i in 40 50 70 90 120 150 180    
#for i in 80 100 120 150 180      
#for i in  110 130 150 170 190 200   
#for i in  90 100 120 140 160 180 210   
#for ipost in   50    
#for i in 20 30 40 50 60  
#----------------
#do 
#
#arg5="50"   #"${ipost}"
#arg6="TRACK_DATA_2D"
#
for irun in {1..1}

do
#combine based
#case 1 
if [ "${irun}" == "1" ]; then 
xmin="235"; xmax="236"; ymin="210"; ymax="211"; wrk_yr="2015"; aoi_reg="XY_ID"   
fi
#case 2
#counter for the iteration or loop 
CONT=$(($CONT+1))
echo "The No. of for loop: $CONT ."

#copy the template from template.job (script for submitting R job with some argunments)
TEMPLATE_FILE="template.submit"
CONFIG_FILE="R_${wrk_yr}_${xmin}_${xmax}_${ymin}_${ymax}_${aoi_reg}.job"
cp $TEMPLATE_FILE $CONFIG_FILE

#dynamic text for the argunments, input and output files and directory
log_file="R_${wrk_yr}_${xmin}_${xmax}_${ymin}_${ymax}_${aoi_reg}.log"
#
TARGET_KEY="work_dir"
REPLACEMENT_VALUE="\/lfs\/home\/ychen\/scripts\/R\/Rscripts\/SPOT_CLASS\/vivian_code\/"
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="R_filename"
REPLACEMENT_VALUE="SPOT_step1.R"
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

# RE-SET THE PARAMETER VALUES FOR EACH JOB
TARGET_KEY="xmin"
REPLACEMENT_VALUE=${xmin}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="xmax"
REPLACEMENT_VALUE=${xmax}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="ymin"
REPLACEMENT_VALUE=${ymin}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="ymax"
REPLACEMENT_VALUE=${ymax}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="aoi_reg"
REPLACEMENT_VALUE=${aoi_reg}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="wrk_yr"
REPLACEMENT_VALUE=${wrk_yr}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

#TARGET_KEY="arg7"
#REPLACEMENT_VALUE=${arg7}
#sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE


TARGET_KEY="log_file"
REPLACEMENT_VALUE=${log_file}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE


echo " arg1:$xmin, arg2:$xmax, arg3:$ymin, arg4:$ymax, arg5:$wrk_yr, arg6:$aoi_reg "  
# submit the job on queuing system  
chmod u+x $CONFIG_FILE

#run on curie by tesing Queue
#ccc_msub -q standard -T 1800 -A gen6328 -Q TEST $CONFIG_FILE 

#normal run on CURIE
#ccc_msub -q standard -T 86400 -A gen6328 $CONFIG_FILE 
#run on airain
#ccc_msub -q ivybridge -T 86400 -A dsm $CONFIG_FILE 
# excute the job 
./$CONFIG_FILE &
echo $CONFIG_FILE

#done
#done


done 
