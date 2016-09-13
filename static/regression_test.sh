#!/bin/bash

# Seconds to pause while waiting for apps to run
delay=600
export TESTS_HOME=/home/mattfel/regression_tests
export SPATIAL_HOME=${TESTS_HOME}/hyperdsl/spatial
export PUB_HOME=${SPATIAL_HOME}/published/Spatial
export HYPER_HOME=${TESTS_HOME}/hyperdsl
export PATH=/opt/maxcompiler/bin:/opt/maxcompiler2016/bin:/opt/maxeler/bin:/opt/altera/quartus/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games

# Reclone
rm -rf $TESTS_HOME && mkdir $TESTS_HOME && cd $TESTS_HOME
git clone git@github.com:stanford-ppl/hyperdsl.git
if [ ! -d "./hyperdsl" ]; then
  echo "hyperdsl directory does not exist!"
  exit 1
fi
cd hyperdsl
git submodule update --init
git fetch
git checkout spatial
cd delite && git fetch && git checkout plasticine && git pull
cd ../forge && git fetch && git checkout spatial && git pull
cd ../virtualization-lms-core && git fetch && git checkout spatial && git pull
cd ../
git clone git@github.com:stanford-ppl/spatial.git
cd spatial && git fetch && git checkout maxj
cd ../
ls
echo "Making hyperdsl..."
sbt compile > /dev/null
echo "Making spatial..."
cd spatial && make > /dev/null
git clone git@github.com:stanford-ppl/spatial.wiki.git

# Fast remake Spatial
rm -rf ${SPATIAL_HOME}/regression_tests;mkdir ${SPATIAL_HOME}/regression_tests
if [ ! -d "${PUB_HOME}" ]; then
  echo "$PUB_HOME directory does not exist!"
  exit 1
fi

cd ${PUB_HOME}
fastmake="cp -r ${SPATIAL_HOME}/extern/compiler/src/ops/* ${PUB_HOME}/compiler/src/spatial/compiler/ops;cd ${PUB_HOME}/;sbt compile 2>&1 | tee -a log"
eval "$fastmake"
wc=$(cat log | grep "success" | wc -l)
if [ "$wc" -ne 1 ]; then
	result_file=${SPATIAL_HOME}/spatial.wiki/MaxJ-Regression-Tests-Status.md
	echo "Current global status on maxj branch:" > $result_file
	echo "-------------------------------" >> $result_file
	echo "" >> $result_file
	echo "" >> $result_file
	echo -e "*Updated `date`*" >> $result_file
	echo "" >> $result_file
	echo "Error building Spatial!  Could not validate anything!" >> $result_file
	# git push
	cd ${SPATIAL_HOME}/spatial.wiki
	git add MaxJ-Regression-Tests-Status.md
	git commit -m "automated status update"
	git push
	exit 1
fi
rm log


##############
# DENSE APPS #
##############

# ADD APPS HERE
test_list=("DotProduct" "MatMult_inner" "TPCHQ6")
args_list=("9600"       "8 192 192"     "1920"  )

# Create vulture dir
rm -rf ${SPATIAL_HOME}/regression_tests/dense;mkdir ${SPATIAL_HOME}/regression_tests/dense
mkdir ${SPATIAL_HOME}/regression_tests/dense/results
cd ${SPATIAL_HOME}/regression_tests/dense

# Initialize results
for i in `seq 0 $((${#test_list[@]}-1))`
do
	touch ${SPATIAL_HOME}/regression_tests/dense/results/inprogress.${i}_${test_list[i]}
done

# Create vulture commands
for i in `seq 0 $((${#test_list[@]}-1))`
do
	# Make dir for this job
	vulture_dir="${SPATIAL_HOME}/regression_tests/dense/${i}_${test_list[i]}"
	rm -rf $vulture_dir;mkdir $vulture_dir
	cmd_file="${vulture_dir}/cmd"

	# Compile and run commands

	echo "#!/bin/bash" >> $cmd_file
	echo 'export HYPER_HOME=${TESTS_HOME}/hyperdsl'
	echo "cd ${PUB_HOME}" >> $cmd_file
	echo "${PUB_HOME}/bin/spatial --outdir=${SPATIAL_HOME}/regression_tests/dense/${i}_${test_list[i]}/out ${test_list[i]} 2>&1 | tee -a ${vulture_dir}/log" >> $cmd_file
	echo '' >> $cmd_file
	echo "sed -i \"s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g\" ${vulture_dir}/log" >> $cmd_file
	echo '' >> $cmd_file
	echo "wc=\$(cat ${vulture_dir}/log | grep \"error\" | wc -l)" >> $cmd_file
	echo 'if [ "$wc" -ne 0 ]; then' >> $cmd_file
	echo "	echo \"PASS: -1 (${test_list[i]} Spatial Error)\"" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_tests/dense/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_tests/dense/results/failed_spatial_compile.${i}_${test_list[i]}" >> $cmd_file
	echo '	exit' >> $cmd_file
	echo 'fi' >> $cmd_file
	echo '' >> $cmd_file
	echo "cd ${vulture_dir}/out" >> $cmd_file
	echo "make clean sim 2>&1 | tee -a ${vulture_dir}/log" >> $cmd_file
	echo "wc=\$(cat ${vulture_dir}/log | grep \"BUILD FAILED\\|Error 1\" | wc -l)" >> $cmd_file
	echo 'if [ "$wc" -ne 1 ]; then' >> $cmd_file
	echo "	echo \"PASS: -1 (${test_list[i]} Spatial Error)\"" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_tests/dense/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_tests/dense/results/failed_maxj_compile.${i}_${test_list[i]}" >> $cmd_file
	echo '	exit' >> $cmd_file
	echo 'fi' >> $cmd_file
	echo '' >> $cmd_file
	echo 'cd out' >> $cmd_file
	echo "bash ${vulture_dir}/out/run.sh ${args_list[i]} 2>&1 | tee -a ${vulture_dir}/log" >> $cmd_file
	echo "if grep -q \"PASS: 1\" ${vulture_dir}/log; then" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_tests/dense/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_tests/dense/results/pass.${i}_${test_list[i]}" >> $cmd_file
	echo "else" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_tests/dense/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_tests/dense/results/failed_validation.${i}_${test_list[i]}" >> $cmd_file
	echo "fi" >> $cmd_file

done

# Run vulture
cd ${SPATIAL_HOME}/regression_tests/dense/
bash ${SPATIAL_HOME}/static/vulture.sh workers_for_dense



##############
# UNIT TESTS #
##############

# Collect tests and their args from file
IFS=$'\n'
test_list=(`cat $SPATIAL_HOME/apps/src/CodegenUnitTests.scala | grep "^object .* extends .*" | sed "s/object//g" | sed "s/extends .*//g" | sed "s/ //g" | awk -F' ' '{print $0}' | sed "s/\/\///g"`)
args_list=(`cat $SPATIAL_HOME/apps/src/CodegenUnitTests.scala | grep "^object .* extends .*" | sed "s/object .* with .* \/\/ Args://g" | awk -F' ' '{print $0}' | sed "s/\/\///g"`)

# Create vulture dir
rm -rf ${SPATIAL_HOME}/regression_tests/unit;mkdir ${SPATIAL_HOME}/regression_tests/unit
mkdir ${SPATIAL_HOME}/regression_tests/unit/results
cd ${SPATIAL_HOME}/regression_tests/unit

# Initialize results
for i in `seq 0 $((${#test_list[@]}-1))`
do
	touch ${SPATIAL_HOME}/regression_tests/unit/results/inprogress.${i}_${test_list[i]}
done

# Create vulture commands
for i in `seq 0 $((${#test_list[@]}-1))`
do
	# Make dir for this job
	vulture_dir="${SPATIAL_HOME}/regression_tests/unit/${i}_${test_list[i]}"
	rm -rf $vulture_dir;mkdir $vulture_dir
	cmd_file="${vulture_dir}/cmd"

	# Compile and run commands

	echo "#!/bin/bash" >> $cmd_file
	echo 'export HYPER_HOME=${TESTS_HOME}/hyperdsl'
	echo "cd ${PUB_HOME}" >> $cmd_file
	echo "${PUB_HOME}/bin/spatial --outdir=${SPATIAL_HOME}/regression_tests/unit/${i}_${test_list[i]}/out ${test_list[i]} 2>&1 | tee -a ${vulture_dir}/log" >> $cmd_file
	echo '' >> $cmd_file
	echo "sed -i \"s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g\" ${vulture_dir}/log" >> $cmd_file
	echo '' >> $cmd_file
	echo "wc=\$(cat ${vulture_dir}/log | grep \"error\" | wc -l)" >> $cmd_file
	echo 'if [ "$wc" -ne 0 ]; then' >> $cmd_file
	echo "	echo \"PASS: -1 (${test_list[i]} Spatial Error)\"" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_tests/unit/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_tests/unit/results/failed_spatial_compile.${i}_${test_list[i]}" >> $cmd_file
	echo '	exit' >> $cmd_file
	echo 'fi' >> $cmd_file
	echo '' >> $cmd_file
	echo "cd ${vulture_dir}/out" >> $cmd_file
	echo "make clean sim 2>&1 | tee -a ${vulture_dir}/log" >> $cmd_file
	echo "wc=\$(cat ${vulture_dir}/log | grep \"BUILD FAILED\\|Error 1\" | wc -l)" >> $cmd_file
	echo 'if [ "$wc" -ne 1 ]; then' >> $cmd_file
	echo "	echo \"PASS: -1 (${test_list[i]} Spatial Error)\"" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_tests/unit/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_tests/unit/results/failed_maxj_compile.${i}_${test_list[i]}" >> $cmd_file
	echo '	exit' >> $cmd_file
	echo 'fi' >> $cmd_file
	echo '' >> $cmd_file
	echo 'cd out' >> $cmd_file
	echo "bash ${vulture_dir}/out/run.sh ${args_list[i]} 2>&1 | tee -a ${vulture_dir}/log" >> $cmd_file
	echo "if grep -q \"PASS: 1\" ${vulture_dir}/log; then" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_tests/unit/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_tests/unit/results/pass.${i}_${test_list[i]}" >> $cmd_file
	echo "else" >> $cmd_file
	echo "  rm ${SPATIAL_HOME}/regression_tests/unit/results/inprogress.${i}_${test_list[i]}" >> $cmd_file
	echo "  touch ${SPATIAL_HOME}/regression_tests/unit/results/failed_validation.${i}_${test_list[i]}" >> $cmd_file
	echo "fi" >> $cmd_file

done

# Run vulture
cd ${SPATIAL_HOME}/regression_tests/unit/
bash ${SPATIAL_HOME}/static/vulture.sh workers_for_unit


#####################
# COLLECT & PUBLISH #
#####################

# Wait and publish results
sleep $delay

# Get git hash
cd ${SPATIAL_HOME}
hash=`git rev-parse HEAD`

# Get results
cd ${SPATIAL_HOME}/regression_tests/dense/results
result_file=${SPATIAL_HOME}/spatial.wiki/MaxJ-Regression-Tests-Status.md
echo -e "*Updated `date`* \n" > $result_file
echo "*hash: ${hash}*" >> $result_file
echo "" >> $result_file
echo "" >> $result_file

echo "Current Dense Apps' status on maxj branch:" >> $result_file
echo "-------------------------------" >> $result_file
echo "" >> $result_file
echo "" >> $result_file
progress=(`find . -type f -maxdepth 1 | sort`)
for p in ${progress[@]}; do
	if [[ $p == *"pass"* ]]; then
		echo "**$p**  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"inprogress"* ]]; then
		echo "$p  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"failed_spatial"* ]]; then
		echo "$p  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"failed_maxj"* ]]; then
		echo "$p  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"failed_validation"* ]]; then
		echo "$p  " | sed "s/\.\///g" >> $result_file
	else
		echo "$p  " | sed "s/\.\///g" >> $result_file
	fi

done

cd ${SPATIAL_HOME}/regression_tests/unit/results
echo "Current CodegenUnitTests' status on maxj branch:" >> $result_file
echo "-------------------------------" >> $result_file
echo "" >> $result_file
echo "" >> $result_file
progress=(`find . -type f -maxdepth 1 | sort`)
for p in ${progress[@]}; do
	if [[ $p == *"pass"* ]]; then
		echo "**$p**  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"inprogress"* ]]; then
		echo "$p  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"failed_spatial"* ]]; then
		echo "$p  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"failed_maxj"* ]]; then
		echo "$p  " | sed "s/\.\///g" >> $result_file
	elif [[ $p == *"failed_validation"* ]]; then
		echo "$p  " | sed "s/\.\///g" >> $result_file
	else
		echo "$p  " | sed "s/\.\///g" >> $result_file
	fi

done
echo "" >> $result_file
echo "" >> $result_file
echo "Comments" >> $result_file
echo "--------" >> $result_file
echo "* Expected FifoLoadStore to fail validation" >> $result_file


# git push
cd ${SPATIAL_HOME}/spatial.wiki
git add MaxJ-Regression-Tests-Status.md
git commit -m "automated status update"
git push
