#!/bin/bash

##########
# CONFIG #
##########
# App classes
app_classes=("dense" "sparse" "unit" "characterization")

# Apps for each class
dense_test_list=("DotProduct" "MatMult_inner" "TPCHQ6" "BlackScholes" "MatMult_outer"
	"Kmeans"  "GEMM"      "GDA"    "SGD"   "LogReg")
dense_args_list=("9600"       "8 192 192"     "1920"   "960"          "8 192 192"    
	"96 8 96" "8 192 192" "384 96" "96 96" "96")
sparse_test_list=("BFS" "PageRank" "TriangleCounting" "SparseSGD" "TPCHQ1")
sparse_args_list=("960" "960"      "960"              "960"       "960"   )    

# Seconds to pause while waiting for apps to run
delay=600

# Override env vars to point to a separate directory for this regression test
export TESTS_HOME=/home/mattfel/regression_tests
export SPATIAL_HOME=${TESTS_HOME}/hyperdsl/spatial
export PUB_HOME=${SPATIAL_HOME}/published/Spatial
export HYPER_HOME=${TESTS_HOME}/hyperdsl
export PATH=/opt/maxcompiler/bin:/opt/maxcompiler2016/bin:/opt/maxeler/bin:/opt/altera/quartus/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games

#############
# FUNCTIONS #
#############


function write_comments {
	echo "

Comments
--------
* Expected FifoLoadStore to fail validation
* Expected GEMM to fail validation
* Need to fix TPCHQ6 to mix SInts and Flts, currently uses SInts only
	* TPCHQ6 also stalls with tileSize > 96 and seems to filter out all prices
* Tighten validation margin on BlackScholes
" >> $1
}

function write_branches {
	echo "

Branches Used
-------------
* hyperdsl => spatial
* delite => plasticine
* virtualization-lms-core => spatial
* forge => spatial
* spatial => maxj" >> $1

} 


function update_log {
	echo "" >> $1
	echo "" >> $1
	progress=(`find . -type f -maxdepth 1 | sort -r`)
	for p in ${progress[@]}; do
		if [[ $p == *"pass"* ]]; then
			echo "**$p**  " | sed "s/\.\///g" >> $1
		elif [[ $p == *"failed_did_not_finish"* ]]; then
			echo "<------------$p  " | sed "s/\.\///g" >> $1
		elif [[ $p == *"failed_app_not_written"* ]]; then
			echo "<------------------------$p  " | sed "s/\.\///g" >> $1
		elif [[ $p == *"failed_build_in_spatial"* ]]; then
			echo "<--------------------$p  " | sed "s/\.\///g" >> $1
		elif [[ $p == *"failed_compile_maxj"* ]]; then
			echo "<----------------$p  " | sed "s/\.\///g" >> $1
		elif [[ $p == *"failed_no_validation_check"* ]]; then
			echo "<--------$p  " | sed "s/\.\///g" >> $1
		elif [[ $p == *"failed_validation"* ]]; then
			echo "<----$p  " | sed "s/\.\///g" >> $1
		else
			echo "Unknown result: $p  " | sed "s/\.\///g" >> $1
		fi
	done
}

function create_script {
	echo "#!/bin/bash
	export HYPER_HOME=${TESTS_HOME}/hyperdsl
	cd ${PUB_HOME}
	${PUB_HOME}/bin/spatial --outdir=${SPATIAL_HOME}/regression_tests/${2}/${3}_${4}/out ${4} 2>&1 | tee -a ${5}/log
	
	sed -i \"s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g\" ${5}/log
	
	wc=\$(cat ${5}/log | grep \"couldn't find DEG file\" | wc -l)
	if [ \"\$wc\" -ne 0 ]; then
		echo \"PASS: -1 (${4} Spatial Error)\"
	    rm ${SPATIAL_HOME}/regression_tests/${2}/results/did_not_finish.${3}_${4}
	    touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_app_not_written.${3}_${4}
		exit
	fi

	wc=\$(cat ${5}/log | grep \"error\" | wc -l)
	if [ \"\$wc\" -ne 0 ]; then
		echo \"PASS: -1 (${4} Spatial Error)\"
	    rm ${SPATIAL_HOME}/regression_tests/${2}/results/did_not_finish.${3}_${4}
	    touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_spatial_compile.${3}_${4}
		exit
	fi
	
	cd ${5}/out
	make clean sim 2>&1 | tee -a ${5}/log
	wc=\$(cat ${5}/log | grep \"BUILD FAILED\\|Error 1\" | wc -l)
	if [ \"\$wc\" -ne 1 ]; then
		echo \"PASS: -1 (${4} Spatial Error)\"
	    rm ${SPATIAL_HOME}/regression_tests/${2}/results/did_not_finish.${3}_${4}
	    touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_maxj_compile.${3}_${4}
		exit
	fi
	
	cd out
	bash ${5}/out/run.sh ${args_list[i]} 2>&1 | tee -a ${5}/log
	if grep -q \"PASS: 1\" ${5}/log; then
	  rm ${SPATIAL_HOME}/regression_tests/${2}/results/did_not_finish.${3}_${4}
	  touch ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
	elif grep -q \"PASS: true\" ${5}/log; then
	  rm ${SPATIAL_HOME}/regression_tests/${2}/results/did_not_finish.${3}_${4}
	  touch ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
	elif grep -q \"PASS: 0\" ${5}/log; then
	  rm ${SPATIAL_HOME}/regression_tests/${2}/results/did_not_finish.${3}_${4}
	  touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_validation.${3}_${4}
	elif grep -q \"PASS: false\" ${5}/log; then
	  rm ${SPATIAL_HOME}/regression_tests/${2}/results/did_not_finish.${3}_${4}
	  touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_validation.${3}_${4}
	else 
	  rm ${SPATIAL_HOME}/regression_tests/${2}/results/did_not_finish.${3}_${4}
	  touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_validation_unwritten.${3}_${4}		
	fi" >> $1
}


#################
# CLONE & BUILD #
#################

echo "[STATUS] `date`: Making test directory"
rm -rf $TESTS_HOME && mkdir $TESTS_HOME && cd $TESTS_HOME
echo "[STATUS] `date`: Cloning stuff..."
git clone git@github.com:stanford-ppl/hyperdsl.git > /dev/null
if [ ! -d "./hyperdsl" ]; then
  echo "hyperdsl directory does not exist!"
  result_file=/home/mattfel/hyperdsl/spatial/spatial.wiki/MaxJ-Regression-Tests-Status.md
  echo "Current global status on maxj branch:" > $result_file
  echo "-------------------------------" >> $result_file
  echo "" >> $result_file
  echo "" >> $result_file
  echo -e "*Updated `date`*" >> $result_file
  echo "" >> $result_file
  echo "Error cloning hyperdsl!  Could not validate anything!" >> $result_file
  # git push
  cd /home/mattfel/hyperdsl/spatial/spatial.wiki
  git add MaxJ-Regression-Tests-Status.md
  git commit -m "automated status update"
  git push
  exit 1
fi
cd hyperdsl
git submodule update --init > /dev/null
git fetch > /dev/null
git checkout spatial > /dev/null
cd delite 
git fetch > /dev/null 
git checkout plasticine > /dev/null 
git pull > /dev/null
cd ../forge 
git fetch > /dev/null 
git checkout spatial > /dev/null 
git pull > /dev/null
cd ../virtualization-lms-core 
git fetch > /dev/null 
git checkout spatial > /dev/null 
git pull > /dev/null
cd ../
git clone git@github.com:stanford-ppl/spatial.git > /dev/null
cd spatial 
git fetch > /dev/null 
git checkout maxj > /dev/null
cd ../
echo "[STATUS] `date`: Done cloning stuff!"
echo "[STATUS] `date`: Making hyperdsl..."
sbt compile > /dev/null 
echo "[STATUS] `date`: hyperdsl done!"
echo "[STATUS] `date`: Making spatial..."
cd spatial
make > /dev/null
echo "[STATUS] `date`: spatial done!"
echo "[STATUS] `date`: Cloning wiki..."
git clone git@github.com:stanford-ppl/spatial.wiki.git
echo "[STATUS] `date`: wiki clone done!"

# Check if things are OK now
rm -rf ${SPATIAL_HOME}/regression_tests;mkdir ${SPATIAL_HOME}/regression_tests
echo "[STATUS] `date`: Checking if spatial made correctly..."
if [ ! -d "${PUB_HOME}" ]; then
  echo "$PUB_HOME directory does not exist!"
  # Use main repo's wiki for update
  if [ ! -d "/home/mattfel/hyperdsl/spatial/spatial.wiki" ]; then
  	echo "FATAL ERROR! No default wiki!"
  	exit
  else 
  	cd /home/mattfel/hyperdsl/spatial/spatial.wiki
	git fetch
	git reset --hard
	result_file=/home/mattfel/hyperdsl/spatial/spatial.wiki/MaxJ-Regression-Tests-Status.md
	echo "Current global status on maxj branch:" > $result_file
	echo "-------------------------------" >> $result_file
	echo "" >> $result_file
	echo "" >> $result_file
	echo -e "*Updated `date`*" >> $result_file
	echo "" >> $result_file
	echo "Error building Spatial!  Could not validate anything!" >> $result_file
	# git push
	cd /home/mattfel/hyperdsl/spatial/spatial.wiki
	git add MaxJ-Regression-Tests-Status.md
	git commit -m "automated status update"
	git push
	exit 1
  fi
fi

# Check if compile worked
cd ${PUB_HOME}
echo "[STATUS] `date`: Making spatial again but faster because if it ain't broke, don't fix it..."
fastmake="cp -r ${SPATIAL_HOME}/extern/compiler/src/ops/* ${PUB_HOME}/compiler/src/spatial/compiler/ops;cd ${PUB_HOME}/;sbt compile 2>&1 | tee -a log"
eval "$fastmake"
echo "[STATUS] `date`: Remake spatial done!"
wc=$(cat log | grep "success" | wc -l)
if [ "$wc" -ne 1 ]; then
	if [ ! -d "${SPATIAL_HOME}/spatial.wiki" ]; then
		echo "FATAL ERROR. No wiki dir"
		exit 1
	else 
		cd $SPATIAL_HOME
		hash=`git log --stat --name-status HEAD^..HEAD`
		cd ${SPATIAL_HOME}/spatial.wiki
		result_file=${SPATIAL_HOME}/spatial.wiki/MaxJ-Regression-Tests-Status.md
		echo "Current global status on maxj branch:" > $result_file
		echo "-------------------------------" >> $result_file
		echo "" >> $result_file
		echo "" >> $result_file
		echo -e "*Status updated on `date`* \n" > $result_file
		echo -e "Latest commit: \n\`\`\`\n${hash}\n\`\`\`" >> $result_file
		echo "" >> $result_file
		echo "Error building Spatial!  Could not validate anything!" >> $result_file
		# git push
		git add MaxJ-Regression-Tests-Status.md
		git commit -m "automated status update"
		git push
		exit 1
	fi
fi
rm log


################
# LAUNCH TESTS #
################

IFS=$'\n'
# Unit test apps (Add more by editing CodegenUnitTests.scala)
unit_test_list=(`cat $SPATIAL_HOME/apps/src/CodegenUnitTests.scala | grep "^object .* extends .*" | sed "s/object//g" | sed "s/extends .*//g" | sed "s/ //g" | awk -F' ' '{print $0}' | sed "s/\/\///g"`)
unit_args_list=(`cat $SPATIAL_HOME/apps/src/CodegenUnitTests.scala | grep "^object .* extends .*" | sed "s/object .* with .* \/\/ Args://g" | awk -F' ' '{print $0}' | sed "s/\/\///g"`)
# Characterization test apps (Add more by editing CharacterizationUnitTests.scala)
characterization_test_list=(`cat $SPATIAL_HOME/apps/src/CharacterizationUnitTests.scala | grep "^object .* extends .*" | sed "s/object//g" | sed "s/extends .*//g" | sed "s/ //g" | awk -F' ' '{print $0}' | sed "s/\/\///g"`)
characterization_args_list=(`cat $SPATIAL_HOME/apps/src/CharacterizationUnitTests.scala | grep "^object .* extends .*" | sed "s/object .* with .* \/\/ Args://g" | awk -F' ' '{print $0}' | sed "s/\/\///g"`)


for ac in ${app_classes[@]}; do 
	# Create vulture dir
	rm -rf ${SPATIAL_HOME}/regression_tests/${ac};mkdir ${SPATIAL_HOME}/regression_tests/${ac}
	mkdir ${SPATIAL_HOME}/regression_tests/${ac}/results
	cd ${SPATIAL_HOME}/regression_tests/${ac}

	# Get apps list
	if [[ $ac == "dense" ]]; then
		test_list=("${dense_test_list[@]}")
		args_list=("${dense_args_list[@]}")
	elif [[ $ac == "sparse" ]]; then
		test_list=("${sparse_test_list[@]}")
		args_list=("${sparse_args_list[@]}")
	elif [[ $ac == "unit" ]]; then
		test_list=("${unit_test_list[@]}")
		args_list=("${unit_args_list[@]}")
	elif [[ $ac == "characterization" ]]; then
		test_list=("${characterization_test_list[@]}")
		args_list=("${characterization_args_list[@]}")
	fi

	# Initialize results
	for i in `seq 0 $((${#test_list[@]}-1))`
	do
		touch ${SPATIAL_HOME}/regression_tests/${ac}/results/did_not_finish.${i}_${test_list[i]}

		# Make dir for this vulture job
		vulture_dir="${SPATIAL_HOME}/regression_tests/${ac}/${i}_${test_list[i]}"
		rm -rf $vulture_dir;mkdir $vulture_dir
		cmd_file="${vulture_dir}/cmd"

		# Create script
		create_script $cmd_file ${ac} $i ${test_list[i]} ${vulture_dir}

		# Run vulture
		cd ${SPATIAL_HOME}/regression_tests/${ac}/
		bash ${SPATIAL_HOME}/static/vulture.sh workers_for_${ac}

	done
done





#####################
# COLLECT & PUBLISH #
#####################

# Wait and publish results
echo "[STATUS] `date`: Waiting $delay seconds..."
sleep $delay

# Get git hashes
cd ${HYPER_HOME}/delite
dhash=`git log --stat --name-status HEAD^..HEAD`
cd ${SPATIAL_HOME}
# hash=`git rev-parse HEAD`
hash=`git log --stat --name-status HEAD^..HEAD`

# Write header
result_file=${SPATIAL_HOME}/spatial.wiki/MaxJ-Regression-Tests-Status.md
echo -e "*Status updated on `date`* \n" > $result_file
echo -e "Latest spatial commit: \n\`\`\`\n${hash}\n\`\`\`" >> $result_file
echo -e "Latest delite commit (MaxJ templates): \n\`\`\`\n${dhash}\n\`\`\`" >> $result_file
echo -e "\n\n\* <---- indicates relative amount of work needed before app will **pass**" >> $result_file

for ac in ${app_classes[@]}; do
	cd ${SPATIAL_HOME}/regression_tests/${ac}/results
	echo "

Current ${ac} apps statuses:
-------------------------------" >> $result_file
	update_log $result_file
done


write_comments $result_file
write_branches $result_file


# git push
cd ${SPATIAL_HOME}/spatial.wiki
git add MaxJ-Regression-Tests-Status.md
git commit -m "automated status update via cron"
git push
