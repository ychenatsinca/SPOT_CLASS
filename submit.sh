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
# arg7 : tunning paramter for the decision tree 

wrk_yr="2020"; aoi_reg="AOI"

xmin="224"; xmax="240"; 

for irun in {1..20}

do
#combine based
#case 1 
if [ "${irun}" == "1" ]; then 
ymin="185"; ymax="187";     
fi
#case 2
if [ "${irun}" == "2" ]; then 
ymin="188"; ymax="189";   
fi
#case 3 
if [ "${irun}" == "3" ]; then 
ymin="190"; ymax="191";   
fi
#case 4
if [ "${irun}" == "4" ]; then 
ymin="192"; ymax="193";   
fi
#case 5
if [ "${irun}" == "5" ]; then 
ymin="194"; ymax="194";   
fi
#case 6
if [ "${irun}" == "6" ]; then 
ymin="195"; ymax="195";   
fi
#case 7
if [ "${irun}" == "7" ]; then 
ymin="196"; ymax="196";   
fi
#case 8
if [ "${irun}" == "8" ]; then 
ymin="197"; ymax="197";   
fi

#case 9
if [ "${irun}" == "9" ]; then 
ymin="198"; ymax="198";   
fi
#case 10
if [ "${irun}" == "10" ]; then 
ymin="199"; ymax="199";   
fi
#case 11
if [ "${irun}" == "11" ]; then 
ymin="200"; ymax="200";   
fi
#case 12
if [ "${irun}" == "12" ]; then 
ymin="201"; ymax="201";   
fi
#case 13
if [ "${irun}" == "13" ]; then 
ymin="202"; ymax="202";   
fi
#case 14
if [ "${irun}" == "14" ]; then 
ymin="203"; ymax="203";   
fi
#case 15
if [ "${irun}" == "15" ]; then 
ymin="204"; ymax="204";   
fi
#case 16
if [ "${irun}" == "16" ]; then 
ymin="205"; ymax="205";   
fi
#case 
if [ "${irun}" == "17" ]; then 
ymin="206"; ymax="206";   
fi
#case 
if [ "${irun}" == "18" ]; then 
ymin="207"; ymax="207";   
fi
#case 
if [ "${irun}" == "19" ]; then 
ymin="208"; ymax="209";   
fi
#case 
if [ "${irun}" == "20" ]; then 
ymin="210"; ymax="212";   
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
REPLACEMENT_VALUE="SPOT_step1_221217.R"
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
