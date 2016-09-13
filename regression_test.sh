#!/bin/bash

##############
# UNIT TESTS #
##############

# Remake Spatial
cd ${PUB_HOME}
fastmake="cp -r ${SPATIAL_HOME}/extern/compiler/src/ops/* ${PUB_HOME}/compiler/src/spatial/compiler/ops;cd ${PUB_HOME}/;sbt compile 2>&1 | tee -a log"
eval "$fastmake"
wc=$(cat log | grep "success" | wc -l)
if [ "$wc" -ne 1 ]; then
	echo "Error remaking!  Check log"
	exit
fi
rm log

# Collect tests and their args from file
IFS=$'\n'
test_list=(`cat $SPATIAL_HOME/apps/src/CodegenUnitTests.scala | grep "^object .*Test extends .*" | sed "s/object .* with //g" | sed "s/ \/\/ Args:.*//g" | awk -F' ' '{print $0}' | sed "s/\/\///g"`)
args_list=(`cat $SPATIAL_HOME/apps/src/CodegenUnitTests.scala | grep "^object .*Test extends .*" | sed "s/object .* with .* \/\/ Args://g" | awk -F' ' '{print $0}' | sed "s/\/\///g"`)

###########################
# MANUALLY ADD TESTS HERE #
###########################

# add to test_list and args_list here

###########################

# Create vulture dir
rm -rf ${SPATIAL_HOME}/regression_test;mkdir ${SPATIAL_HOME}/regression_test
mkdir ${SPATIAL_HOME}/regression_test/results

# Initialize results
for i in `seq 0 $((${#test_list[@]}-1))`
do
	touch ${SPATIAL_HOME}/regression_test/results/inprogress.${i}_${test_list[i]}
done

# Create vulture commands
for i in `seq 0 $((${#test_list[@]}-1))`
do
	# Make dir for this job
	vulture_dir="${SPATIAL_HOME}/regression_test/${i}_${test_list[i]}"
	rm -rf $vulture_dir;mkdir $vulture_dir
	cmd_file="${vulture_dir}/cmd"

	# Compile and run commands

	echo "#!/bin/bash" >> $cmd_file
	echo "cd ${PUB_HOME}" >> $cmd_file
	echo "${PUB_HOME}/bin/spatial --outdir=../../regression_test/${i}_${test_list[i]}/out ${test_list[i]}Test 2>&1 | tee -a ${vulture_dir}/log" >> $cmd_file
	echo '' >> $cmd_file
	echo "sed -i \"s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g\" ${vulture_dir}/log" >> $cmd_file
	echo '' >> $cmd_file
	echo "wc=\$(cat ${vulture_dir}/log | grep \"error\" | wc -l)" >> $cmd_file
	echo 'if [ "$wc" -ne 0 ]; then' >> $cmd_file
	echo "	echo \"PASS: -1 (${test_list[i]} Spatial Error)\"" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_test/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_test/results/failed_spatial_compile.${i}_${test_list[i]}" >> $cmd_file
	echo '	exit' >> $cmd_file
	echo 'fi' >> $cmd_file
	echo '' >> $cmd_file
	echo "cd ${vulture_dir}/out" >> $cmd_file
	echo "make clean sim 2>&1 | tee -a ${vulture_dir}/log" >> $cmd_file
	echo "wc=\$(cat ${vulture_dir}/log | grep \"BUILD FAILED\\|Error 1\" | wc -l)" >> $cmd_file
	echo 'if [ "$wc" -ne 1 ]; then' >> $cmd_file
	echo "	echo \"PASS: -1 (${test_list[i]} Spatial Error)\"" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_test/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_test/results/failed_maxj_compile.${i}_${test_list[i]}" >> $cmd_file
	echo '	exit' >> $cmd_file
	echo 'fi' >> $cmd_file
	echo '' >> $cmd_file
	echo 'cd out' >> $cmd_file
	echo "bash ${vulture_dir}/out/run.sh ${args_list[i]} 2>&1 | tee -a ${vulture_dir}/log" >> $cmd_file
	echo "if grep -q \"PASS: 1\" ${vulture_dir}/log; then" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_test/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_test/results/pass.${i}_${test_list[i]}" >> $cmd_file
	echo "else" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_test/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_test/results/failed_validation.${i}_${test_list[i]}" >> $cmd_file
	echo "fi" >> $cmd_file

done

# Run vulture
cd ${SPATIAL_HOME}/regression_test/
bash ${SPATIAL_HOME}/vulture.sh workers_for_regression

# Wait 3 min and publish results
sleep 180
cd results
result_file=${SPATIAL_HOME}/spatial.wiki/MaxJ-Regression-Tests-Status.md
echo "Current status on ${USER}'s local repo:" > $result_file
echo "-------------------------------" >> $result_file
echo "" >> $result_file
echo "" >> $result_file
echo "*Updated `date`*" >> $result_file
echo "" >> $result_file
echo "" >> $result_file
progress=(`find . -type f -maxdepth 1 | sort`)
for p in ${progress[@]}; do
	if [[ $p == *"pass"* ]]; then
		echo "<span style=\"color:green\">$p</span>  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"inprogress"* ]]; then
		echo "$p  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"failed_spatial"* ]]; then
		echo "<span style=\"color:red\">$p</span>  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"failed_maxj"* ]]; then
		echo "<span style=\"color:red\">$p</span>  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"failed_validation"* ]]; then
		echo "<span style=\"color:red\">$p</span>  " | sed "s/\.\///g" >> $result_file
	else
		echo "$p  " | sed "s/\.\///g" >> $result_file
	fi

done
echo "" >> $result_file
echo "" >> $result_file
echo "############" >> $result_file
echo "# COMMENTS #" >> $result_file
echo "############" >> $result_file
echo "* Expected FifoLoadStore to fail validation"

# git push
cd ${SPATIAL_HOME}/spatial.wiki
git add MaxJ-Regression-Tests-Status.md
git commit -m "automated status update"
git push
