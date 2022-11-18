#!/bin/bash

echo "running batch scripts to creat job files from template.job"
cd /lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/vivian_code/
# LOAD R MODULE 
#module load r/3.1.1

# submit the job on queue 
# chmod u+x $CONFIG_FILE
# qsub -jeo -q short $CONFIG_FILE
CONT=0

# argunment 1 to 6 are parameters for the function SPOT_step1.R
 
# arg1 : xmin min of XID of GRID BOX within the AOI(Area Of Interest)
# arg2 : xmax max of XID of GRID BOX within the AOI
# arg3 : ymin min of YID of GRID BOX within the AOI 
# arg4 : ymax max of YID of GRID BOX within the AOI
# arg5 : wrk_yr selected working year
# arg6 : aoi_region selected region of AOI (which was define in the function)
#
for irun in {1..3}

do
#combine based
#case 1 
if [ "${irun}" == "1" ]; then 
xmin="235"; xmax="236"; ymin="210"; ymax="211"; wrk_yr="2015"; aoi_reg="NORTH"   
fi
#case 2
if [ "${irun}" == "2" ]; then 
xmin="235"; xmax="236"; ymin="210"; ymax="211"; wrk_yr="2015"; aoi_reg="CENTRAL"   
fi
#case 3
if [ "${irun}" == "3" ]; then 
xmin="235"; xmax="236"; ymin="210"; ymax="211"; wrk_yr="2015"; aoi_reg="SOUTH"   
fi


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

done 
