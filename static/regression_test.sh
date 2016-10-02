#!/bin/bash

##########
# CONFIG #
##########
# Length of history to maintain in pretty printer (remember to manually add enough git commits)
hist=48
branch=$1

# App classes
app_classes=("dense" "sparse" "unit" "characterization")

# Apps for each class
dense_test_list=("DotProduct" "MatMult_inner" "TPCHQ6" "BlackScholes" "MatMult_outer"
	"Kmeans"  "GEMM"      "GDA"    "SGD"   "LogReg" "OuterProduct")
dense_args_list=("9600"       "8 192 192"     "1920"   "960"          "8 192 192"    
	"96 8 96" "8 192 192" "96 96" "96 96" "96"     "192 192")
sparse_test_list=("BFS" "PageRank" "TriangleCounting" "SparseSGD" "TPCHQ1")
sparse_args_list=("960" "960"      "960"              "960"       "960"   )    

# Seconds to pause while waiting for apps to run
delay=900


# Override env vars to point to a separate directory for this regression test
export TESTS_HOME="/home/mattfel/${branch}_regression_tests"
export SPATIAL_HOME=${TESTS_HOME}/hyperdsl/spatial
export PUB_HOME=${SPATIAL_HOME}/published/Spatial
export HYPER_HOME=${TESTS_HOME}/hyperdsl
export PATH=/opt/maxcompiler/bin:/opt/maxcompiler2016/bin:/opt/maxeler/bin:/opt/altera/quartus/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games
export FORGE_HOME=${HYPER_HOME}/forge
export DELITE_HOME=${HYPER_HOME}/delite
export LMS_HOME=${HYPER_HOME}/virtualization-lms-core
export PIR_HOME=${HYPER_HOME}/spatial/published/Spatial

#############
# FUNCTIONS #
#############

function foo {
	echo "$branch"
}
function write_comments {
	echo "

Comments
--------
* Expected FifoLoadStore to fail validation
* Expected GEMM to fail validation
* Tighten validation margin on BlackScholes
* Make SGD work on more than 1 epoch
" >> $1
}

function write_branches {
	echo "

Branches Used
-------------
* hyperdsl => spatial
* delite => spatial
* virtualization-lms-core => spatial
* forge => spatial
* spatial => ${branch}" >> $1

} 


function update_log {
	perf_hist=72
	echo "" >> $1
	echo "" >> $1
	progress=(`find . -type f -maxdepth 1 | sort -r`)
	for p in ${progress[@]}; do
		pname=(`echo $p | sed "s/.*[0-9]\+_//g"`)
		cute_plot="[🗠](https://raw.githubusercontent.com/wiki/stanford-ppl/spatial/${branch}_$pname.png)"
		if [[ $p == *"pass"* ]]; then
			echo "**$p**${cute_plot}  " | sed "s/\.\///g" >> $1
			t=(`cat $p`)
		elif [[ $p == *"failed_did_not_finish"* ]]; then
			echo "<------------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
			t=0
		elif [[ $p == *"failed_app_not_written"* ]]; then
			echo "<------------------------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
			t=0
		elif [[ $p == *"failed_build_in_spatial"* ]]; then
			echo "<--------------------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
			t=0
		elif [[ $p == *"failed_compile_maxj"* ]]; then
			echo "<----------------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
			t=0
		elif [[ $p == *"failed_no_validation_check"* ]]; then
			echo "<--------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
			t=0
		elif [[ $p == *"failed_validation"* ]]; then
			echo "<----${p}${cute_plot}  " | sed "s/\.\///g" >> $1
			t=0
		else
			echo "Unknown result: $p  " | sed "s/\.\///g" >> $1
			t=0
		fi

		# Update performance file
		perf_file="${SPATIAL_HOME}/spatial.wiki/${branch}_${pname}.csv"
		lines=(`cat $perf_file | wc -l`)
		dline=$(($lines-$(($perf_hist-1))))
		last=(`tail -n1 < $perf_file`)
		last_color=(`echo ${last[@]} | sed "s/;.*//g"`)
		if [[ "${last[@]}" = *"$2 $3"* ]]; then
			color=$last_color
			echo "[SPATIAL NOTICE] Using old color $color for app $p and hash $2 $3"
		else
			if [ "$last_color" = "r" ]; then
				color="b"
				echo "[SPATIAL NOTICE] Using new color $color for app $p and hash $2 $3 from line ${last[@]}"
			else
				color="r"
				echo "[SPATIAL NOTICE] Using new color $color for app $p and hash $2 $3 from line ${last[@]}"
			fi
		fi

		echo -ne "\n${color};$t;$2 $3" >> $perf_file

		# Hack to get rid of empty first line for new files
		first=(`head -n1 < $perf_file`)
		if [ "$first" = "" ]; then 
			sed -i -e "1d" $perf_file
		fi

		if [ $dline -gt $perf_hist ]; then
			sed -i -e "1d" $perf_file
		fi

		cmd="python ${SPATIAL_HOME}/static/plotter.py ${branch}_${pname} ${SPATIAL_HOME}/spatial.wiki/"
		eval "$cmd"


	done
}

function create_script {
	echo "
#!/bin/bash
# Override env vars to point to a separate directory for this regression test
export TESTS_HOME=/home/mattfel/${branch}_regression_tests
export SPATIAL_HOME=${TESTS_HOME}/hyperdsl/spatial
export PUB_HOME=${SPATIAL_HOME}/published/Spatial
export HYPER_HOME=${TESTS_HOME}/hyperdsl
export PATH=/opt/maxcompiler/bin:/opt/maxcompiler2016/bin:/opt/maxeler/bin:/opt/altera/quartus/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games
export FORGE_HOME=${HYPER_HOME}/forge
export DELITE_HOME=${HYPER_HOME}/delite
export LMS_HOME=${HYPER_HOME}/virtualization-lms-core
export PIR_HOME=${HYPER_HOME}/spatial/published/Spatial

sleep ${3} # Backoff time to prevent those weird file IO errors

cd ${PUB_HOME}
${PUB_HOME}/bin/spatial --outdir=${SPATIAL_HOME}/regression_tests/${2}/${3}_${4}/out ${4} 2>&1 | tee -a ${5}/log

sed -i \"s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g\" ${5}/log
sed -i \"s/error retrieving current directory/Ignoring getcwd e r r o r/g\" log

wc=\$(cat ${5}/log | grep \"couldn't find DEG file\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
	echo \"PASS: -1 (${4} Spatial Error)\"
	if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4} ]; then
	    rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
   		echo \"[STATUS] Declaring failure app_not_written\"
    	touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_app_not_written.${3}_${4}
    fi
	exit 1
fi

wc=\$(cat ${5}/log | grep \"error\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
	echo \"PASS: -2 (${4} Spatial Error)\"
	if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4} ]; then
	    rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
	    echo \"[STATUS] Declaring failure build_in_spatial\"
    	touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_build_in_spatial.${3}_${4}
    fi
	exit 1
fi

cd ${5}/out
make clean sim 2>&1 | tee -a ${5}/log
wc=\$(cat ${5}/log | sed \"s/Error 1 (ignored)/ignore e r r o r/g\" | grep \"BUILD FAILED\\|Error 1\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
	echo \"PASS: -3 (${4} Spatial Error)\"
	if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4} ]; then
	    rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
	    echo \"[STATUS] Declaring failure compile_maxj\"
	    touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_maxj.${3}_${4}
	fi
	exit 1
fi

cd out
bash ${5}/out/run.sh ${args_list[i]} 2>&1 | tee -a ${5}/log
if grep -q \"PASS: 1\" ${5}/log; then
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
    touch ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
	cat ${5}/log | grep \"Kernel done, cycles\" | sed \"s/Kernel done, cycles = //g\" > ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
  fi
elif grep -q \"PASS: true\" ${5}/log; then
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
    touch ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
	cat ${5}/log | grep \"Kernel done, cycles\" | sed \"s/Kernel done, cycles = //g\" > ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
  fi
elif grep -q \"PASS: 0\" ${5}/log; then
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
    echo \"[STATUS] Declaring failure validation\"
	touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_validation.${3}_${4}
  fi
elif grep -q \"PASS: false\" ${5}/log; then
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
    echo \"[STATUS] Declaring failure validation\"
    touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_validation.${3}_${4}
  fi
else 
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
    echo \"[STATUS] Declaring failure no_validation_check\"
	touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_no_validation_check.${3}_${4}		
  fi
fi" >> $1
}



#################
# CLONE & BUILD #
#################

# Kill any maxcompilersims older than an hour
stale_sims=(`ps axh -O user,etimes | grep mattfel | grep maxcompilersim  | awk '{if ($3 >= 1) print $1}'`)
for job in ${stale_sims[@]}; do
	kill -9 $job
done

# Clone and everything
echo "[STATUS] `date`: Making test directory"
rm -rf $TESTS_HOME && mkdir $TESTS_HOME && cd $TESTS_HOME
echo "[STATUS] `date`: Cloning stuff..."
git clone git@github.com:stanford-ppl/hyperdsl.git > /dev/null
if [ ! -d "./hyperdsl" ]; then
  echo "hyperdsl directory does not exist!"
  result_file="/home/mattfel/hyperdsl/spatial/spatial.wiki/${branch}-Regression-Tests-Status.md"
  echo "Current global status on ${branch} branch:" > $result_file
  echo "-------------------------------" >> $result_file
  echo "" >> $result_file
  echo "" >> $result_file
  echo -e "*Updated `date`*" >> $result_file
  echo "" >> $result_file
  echo "Error cloning hyperdsl!  Could not validate anything!" >> $result_file
  # git push
  cd /home/mattfel/hyperdsl/spatial/spatial.wiki
  cmd="git add ${branch}-Regression-Tests-Status.md"
  eval "$cmd"
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
git checkout spatial > /dev/null 
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
cmd="git checkout ${branch}"
eval "$cmd" > /dev/null
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
  	exit 1
  else 
  	cd /home/mattfel/hyperdsl/spatial/spatial.wiki
	git fetch
	git reset --hard
	result_file="/home/mattfel/hyperdsl/spatial/spatial.wiki/${branch}-Regression-Tests-Status.md"
	echo "Current global status on ${branch} branch:" > $result_file
	echo "-------------------------------" >> $result_file
	echo "" >> $result_file
	echo "" >> $result_file
	echo -e "*Updated `date`*" >> $result_file
	echo "" >> $result_file
	echo "Error building Spatial!  No published dir. Could not validate anything!" >> $result_file
	# git push
	cd /home/mattfel/hyperdsl/spatial/spatial.wiki
	cmd="git add ${branch}-Regression-Tests-Status.md"
	eval "$cmd"
	git commit -m "automated status update"
	git push
	exit 1
  fi
fi

# Check if compile worked
result_file="${SPATIAL_HOME}/spatial.wiki/${branch}-Regression-Tests-Status.md"
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
		echo "Current global status on ${branch} branch:" > $result_file
		echo "-------------------------------" >> $result_file
		echo "" >> $result_file
		echo "" >> $result_file
		tucson_date=`ssh tucson.stanford.edu date`
		echo -e "*Status updated on $tucson_date* \n" > $result_file
		echo -e "Latest commit: \n\`\`\`\n${hash}\n\`\`\`" >> $result_file
		echo "" >> $result_file
		echo "Error building Spatial!  Remake seemed to fail.  Could not validate anything!" >> $result_file
		# git push
		cmd="git add ${branch}-Regression-Tests-Status.md"
		eval "$cmd"
		git commit -m "automated status update"
		git push
		exit 1
	fi
fi
rm log
cd ${PUB_HOME}


################
# LAUNCH TESTS #
################

# Use magic to free unused semaphores
for semid in `ipcs -s | cut -d" " -f 2` ; do pid=`ipcs -s -i $semid | tail -n 2 | head -n 1 | awk '{print $5}'`; running=`ps --no-headers -p $pid | wc -l` ; if [ $running -eq 0 ] ; then ipcrm -s $semid ; fi ; done

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
		touch ${SPATIAL_HOME}/regression_tests/${ac}/results/failed_did_not_finish.${i}_${test_list[i]}

		# Make dir for this vulture job
		vulture_dir="${SPATIAL_HOME}/regression_tests/${ac}/${i}_${test_list[i]}"
		rm -rf $vulture_dir;mkdir $vulture_dir
		cmd_file="${vulture_dir}/cmd"

		# Create script
		create_script $cmd_file ${ac} $i ${test_list[i]} ${vulture_dir}

		# Run vulture
		cd ${SPATIAL_HOME}/regression_tests/${ac}/
		bash ${SPATIAL_HOME}/static/vulture.sh ${ac}_${branch}

	done
done





#####################
# COLLECT & PUBLISH #
#####################


# Wait and publish results
echo "[STATUS] `date`: Waiting $delay seconds..."
sleep $delay

# Get git hashes
cd ${SPATIAL_HOME}
hash_str=`git rev-parse HEAD`
cd ../delite
dhash_str=`git rev-parse HEAD`
cd ${HYPER_HOME}/delite
dhash=`git log --stat --name-status HEAD^..HEAD`
cd ${SPATIAL_HOME}
# hash=`git rev-parse HEAD`
hash=`git log --stat --name-status HEAD^..HEAD`

# Get list of previous passed tests and previous commit
old_pass=(`cat ${result_file} | grep "^\*\*pass" | sed "s/\*\*//g" | sed "s/pass\.[0-9]\+\_//g" | sed "s/\[🗠.*//g" | sort`)
old_commit=(`cat ${result_file} | grep "^commit" | awk '/commit/{i++}i==1'`)

# Begin write to file
rm $result_file
echo -e "

*Status updated on `date`*

* <---- indicates relative amount of work needed before app will **pass**" > $result_file

for ac in ${app_classes[@]}; do
	cd ${SPATIAL_HOME}/regression_tests/${ac}/results
	echo "

# ${ac}:
" | awk '{print toupper($0)}' >> $result_file
	update_log $result_file `echo $hash_str | cut -c1-5` `echo $dhash_str | cut -c1-5`
done

echo -e "\n\n***\n\n" >> $result_file

# Link to logs
echo -e "\n## [History log](https://raw.githubusercontent.com/wiki/stanford-ppl/spatial/${branch}_Regression_Test_History.csv) \n" >> $result_file
echo -e "\n## [Prettier History log](https://raw.githubusercontent.com/wiki/stanford-ppl/spatial/${branch}_Pretty_Regression_Test_History.csv) \n" >> $result_file

write_comments $result_file

# Write commit info
echo -e "Latest spatial commit: \n\`\`\`\n${hash}\n\`\`\`" >> $result_file
echo -e "Latest delite commit (MaxJ templates): \n\`\`\`\n${dhash}\n\`\`\`" >> $result_file
write_branches $result_file

# Get list of current failed tests
new_fail=(`cat ${result_file} | grep "failed_\|failed_did_not_finish" | sed "s/<-\+//g" | sed "s/^.*[0-9]\+\_//g" | sed "s/\[🗠.*//g" | sort`)
new_commit=(`cat ${result_file} | grep "^commit" | awk '/commit/{i++}i==1'`)
emails=(`cat ${result_file} | grep "<.*>" | sed 's/Author.*<//g' | sed 's/>//g'`)
csvemails=(`echo ${email[@]} | sed 's/ /,/g'`)

# Check for intersection and send notice
diff=($(comm -12 <(for X in "${new_fail[@]}"; do echo "${X}"; done|sort)  <(for X in "${old_pass[@]}"; do echo "${X}"; done|sort)))
last_m=""
echo "[SPATIAL NOTICE] The following apps got messed up: ${diff[@]}"
if [ ! -z "$diff" ]; then 
	echo "debug1"
	for m in ${emails[@]}; do
		echo "debug2"
		if [[ ! "$last_m" = "$m" ]]; then 
			echo "debug3"
			courtesy_email="The following apps on branch ${branch} went from pass to fail: ${diff[@]} when going from commits: $old_commit to $new_commit.  See https://github.com/stanford-ppl/spatial/wiki/${branch}-Regression-Tests-Status"
			echo "Message: ${courtesy_email}" | mail $m -s "[SPATIAL NOTICE] Oops on ${branch}-branch!" 
			# -r AppTsar@spatial-lang.com
		fi
		echo "[EMAIL] Sent ${tmp} to $m"
		last_m=$m
	done
fi

# Update history 
history_file=${SPATIAL_HOME}/spatial.wiki/${branch}_Regression_Test_History.csv
pretty_file=${SPATIAL_HOME}/spatial.wiki/${branch}_Pretty_Regression_Test_History.csv
all_apps=(`cat ${result_file} | grep "^\*\*pass\|^<-\+failed" | sed "s/<-\+//g" | sed "s/^.*[0-9]\+\_//g" | sed "s/\*//g" | sed "s/\[🗠.*//g" | sort`)
last_line=(`tail -n1 < $pretty_file`)
last_line=$(printf " %s" "${last_line[@]}")
if [[ "$last_line" = *"$hash_str / $dhash_str"* ]]; then
	commit_change=false
else
	commit_change=true
fi
for aa in ${all_apps[@]}; do
	if [[ ! "$last_aa" = "$aa" ]]; then
		# Append status to line
		a=(`echo $aa | sed "s/ //g" | sed "s/\[.*//g"`)
		dashes=(`cat ${result_file} | grep "[0-9]\+\_$a\(\ \|\*\|\[\)" | sed "s/\[🗠.*//g" | grep -oh "\-" | wc -l`)
		num=$(($dashes/4))
		if [ $num = 0 ]; then bar=▇; elif [ $num = 1 ]; then bar=▆; elif [ $num = 2 ]; then bar=▅; elif [ $num = 3 ]; then bar=▄; elif [ $num = 4 ]; then bar=▃; elif [ $num = 5 ]; then bar=▂; elif [ $num = 6 ]; then bar=▁; else bar=□; fi

		# Print what the seds are for debug
		# cmd="sed \"/^${a}\ \+,/ s/$/,$num/\" ${history_file}"
		# echo -e "\n\n [SPATIAL NOTICE] sedding for $a"
		# eval "$cmd"
		# cmd="sed \"/^${a}\ \+,/ s/$/$bar/\" ${pretty_file}"
		# eval "$cmd"

		# Actually edit files
		cmd="sed -i \"/^${a}\ \+,/ s/$/,$num/\" ${history_file}"
		eval "$cmd"
		cmd="sed -i \"/^${a}\ \+,/ s/$/$bar/\" ${pretty_file}"
		eval "$cmd"

		# Shave first if too long
		numel=(`cat ${history_file} | grep "^$a\ " | sed "s/^$a \+,//g" | grep -oh "\," | wc -l`)
		if [ $numel -gt $hist ]; then
			cmd="sed -i \"s/^${a}\([[:blank:]]*\),,[0-9]\\+,/${a}\1,,/g\" ${history_file}"
			echo "[SPATIAL NOTICE] shaving $a in history"
			eval "$cmd"
		fi
		# Shave first if too long
		numel=(`cat ${pretty_file} | grep "^$a\ " | grep -oh "." | wc -l`)
		chars_before_bars=(`cat ${pretty_file} | grep "^$a\ " | sed "s/,,.*/,,/g" | grep -oh "." | wc -l`)
		if [ $numel -gt $(($hist+$chars_before_bars)) ]; then 
			cmd="sed -i \"s/^${a}\([[:blank:]]*\),,./${a}\1,,/g\" ${pretty_file}"
			echo "[SPATIAL NOTICE] shaving $a in pretty history"
			eval "$cmd"	
		fi

	fi
	last_aa=$aa

done

# Draw delta-commit arrows
if [ "$commit_change" = "true" ]; then
	arrow="↖"
	varrow="↑"
	marker="↰"
else
	arrow=" "
	varrow=" "
	marker=" "
fi
cmd="sed -i \"/^(commit change)\ \+,/ s/$/$arrow/\" ${pretty_file}"
eval "$cmd"
cmd="sed -i \"/^(commit change)\ \+,/ s/$/$varrow /\" ${history_file}"
eval "$cmd"
numel1=(`cat ${history_file} | grep "^(commit change)\ " | sed "s/(commit change) \+,//g" | grep -oh "." | wc -l`)
numel2=(`cat ${history_file} | grep "^(commit change)\ " | wc -l`)
numel=$(($numel1 / $numel2 / 2))
if [ $numel -gt $hist ]; then
	cmd="sed -i \"s/^(commit change)\([[:blank:]]*\),,../(commit change)\1,,/g\" ${history_file}"
	eval "$cmd"
fi
numel1=(`cat ${pretty_file} | grep "^(commit change)\ " | grep -oh "." | wc -l`)
numel2=(`cat ${pretty_file} | grep "^(commit change)\ " | wc -l`)
numel=$(($numel1 / $numel2))
if [ $numel -gt $(($hist+$chars_before_bars)) ]; then 
	cmd="sed -i \"s/^(commit change)\([[:blank:]]*\),,./(commit change)\1,,/g\" ${pretty_file}"
	eval "$cmd"	
fi

# Delete outdated line and add new one
cd ${SPATIAL_HOME}
lines=(`cat $history_file | wc -l`)
dline=$(($lines-$(($hist-1))))
sed -i -e "${dline}d" $history_file
echo "$hash_str / $dhash_str $marker" >> $history_file
lines=(`cat $pretty_file | wc -l`)
dline=$(($lines-$(($hist-1))))
sed -i -e "${dline}d" $pretty_file
echo "$hash_str / $dhash_str $marker" >> $pretty_file

# git push
cd ${SPATIAL_HOME}/spatial.wiki
git stash
git pull
git stash pop
git add *
git commit -m "automated status update via cron"
git push

