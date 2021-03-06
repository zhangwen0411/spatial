#!/bin/sh

## This file contains all the functions used for regression testing.
##   It is called from the receive.sh, which handles path variables
##   and git checkouts on a server-specific basis

spacing=45
delay=2100
numpieces=30
hist=72

## Function for finding filse with older timestamps.
##   This tester will yield to any active or new tests who are older
coordinate() {
  cd ${REGRESSION_HOME}
  files=(*)
  new_packets=()
  sorted_packets=()
  for f in ${files[@]}; do if [[ $f = *".new"* || $f = *".ack"* || $f = *".lock"* ]]; then new_packets+=($f); fi; done
  sorted_packets=( $(for arr in "${new_packets[@]}"; do echo $arr; done | sort) )
  stringified=$( IFS=$' '; echo "${sorted_packets[*]}" )
  rank=-1
  for i in ${!sorted_packets[@]}; do if [[  "$packet" = *"${sorted_packets[$i]}"* ]]; then rank=${i}; fi; done
  while [ $rank != 0 ]; do
    # Sanity check
    if [ $rank = -1 ]; then 
      logger "CRITICAL ERROR: This packet ${packet} was not found in waiting list ${stringified}"
      exit 1
    fi

    logger "This packet (${packet}) is ${rank}-th in line (${stringified})... Waiting $((delay/numpieces)) seconds..."
    sleep $((delay/numpieces))

    # Update active packets list
    files=(*)
    new_packets=()
    for f in ${files[@]}; do if [[ $f = *".new"* || $f = *".ack"* || $f = *".lock"* ]]; then new_packets+=($f); fi; done
    sorted_packets=()
    sorted_packets=( $(for arr in "${new_packets[@]}"; do echo $arr; done | sort) )
    stringified=$( IFS=$' '; echo "${sorted_packets[*]}" )
    rank=-1
    for i in ${!sorted_packets[@]}; do if [[  "$packet" = *"${sorted_packets[$i]}"* ]]; then rank=${i}; fi; done
  done
  logger "Packet cleared to launch! (${packet}) in list (${stringified})"

  cd -
}

## Function for building spatial
build_spatial() {
  logger "Cleaning old regression directories..."
  cd ${REGRESSION_HOME}
  files=(*)
  testdirs=()
  sorted_testdirs=()
  for f in ${files[@]}; do if [[ $f = *"testdir"* ]]; then testdirs+=($f); fi; done
  sorted_testdirs=( $(for arr in "${testdirs[@]}"; do echo $arr; done | sort) )
  stringified=$( IFS=$' '; echo "${sorted_testdirs[*]}" )
  rank=-5
  for i in ${!sorted_testdirs[@]}; do if [[  "${sorted_testdirs[$i]}" = *"$tim"* ]]; then rank=$((i-1)); fi; done
  # Sanity check
  if [ $rank = -5 ]; then 
    logger "CRITICAL ERROR: This time ${tim} was not found in dirs list ${stringified}"
    exit 1
  fi

  for i in `seq 0 $rank`; do
    logger "Cleaning ${sorted_testdirs[$i]}..."
    cmd="stubborn_delete ${sorted_testdirs[$i]}"
    eval "$cmd"
  done
  logger "Cleanup done!"
  
  logger "Patching the nsc library thing..."
  cd $DELITE_HOME
  find ./ -type f -exec sed -i -e 's/^import tools.nsc/import scala.tools.nsc/g' {} \; # Not sure why we do this suddenly..
  cd $SPATIAL_HOME
  find ./ -type f -exec sed -i -e 's/^import tools.nsc/import scala.tools.nsc/g' {} \; # Not sure why we do this suddenly..
  logger "Patch done!"

  logger "Making spatial..."
  make > /tmp/log 2>&1
  logger "Spatial done!"
  logger "Checking if spatial made correctly..."
  exists "$PUB_HOME" 3
  errs=(`cat /tmp/log | grep error | grep -v errors | wc -l`)
  if [[ $errs -gt 0 ]]; then
  	clean_exit 8 "Detected errors in spatial build (/tmp/log)"
  fi
  logger "Spatial made correctly!"

  # Patch bin/spatial
  logger "Patching bin/spatial..."
  if [[ ${test_to_run} = "maxj" ]]; then
    sed -i 's/parser.add_argument("--maxj", dest="maxj", action="store_true", default=False/parser.add_argument("--maxj", dest="maxj", action="store_true", default=True/g' ${PUB_HOME}/bin/spatial
  elif [[ ${test_to_run} = "scala" ]]; then
    sed -i 's/dest="test", action="store_true", default=False/dest="test", action="store_true", default=True/g' ${PUB_HOME}/bin/spatial
  fi
  sed -i "s/parser.add_argument('--CGRA', dest='cgra', action='store_true', default=True/parser.add_argument('--CGRA', dest='cgra', action='store_true', default=False/g" ${PUB_HOME}/bin/spatial
  logger "Patch done!"
}

## Function for cleaning up iff test was successful
wipe_clean() {
summary=`sed -n '1p' $packet | sed -r "s/\| Status-[^\|]*\|/| Status- Success |/g"`

echo "`date` - $summary (wipe_clean)" >> ${REGRESSION_HOME}/regression_history.log

cd $WIKI_HOME
logger "Pushing wiki..."
git add -A
git commit -m "Automated incron update"
git push

logger "Removing packet ${packet} so those waiting are clear to launch"
rm $packet

# stubborn_delete ${dirname}

ps aux | grep -ie mattfel | grep -v ssh | grep -v bash | grep -iv screen | grep -v receive | awk '{system("kill -9 " $2)}'

exit 1

}

## Function for collecting results and reporting in the markdown
collect_results() {
logger "Removing old wiki directory..."
rm -rf $WIKI_HOME
cd $SPATIAL_HOME
logger "Cloning wiki to avoid conflicts..."
git clone git@github.com:stanford-ppl/spatial.wiki.git  > /dev/null 2>&1
git checkout -f HEAD  > /dev/null 2>&1
logger "Cloning done!"
logger "Cleaning old markdown file..."
rm $wiki_file > /dev/null 2>&1
touch $wiki_file
logger "Putting timestamp in wiki"
echo -e "
Edited at `date`
* <---- indicates relative amount of work needed before app will **pass**" > $wiki_file

for ac in ${types_list[@]}; do
  logger "Collecting results for ${ac} apps, putting in ${wiki_file}"
  cd ${SPATIAL_HOME}/regression_tests/${ac}/results
  echo "

# ${ac}:
" | awk '{print toupper($0)}' >> $wiki_file
  init_travis_ci $ac
  update_log $wiki_file `echo ${spatial_hash} | cut -c1-5` `echo ${delite_hash} | cut -c1-5`
  push_travis_ci $ac
done

echo -e "\n\n***\n\n" >> $wiki_file

# Link to logs
# echo -e "\n## [History log](https://raw.githubusercontent.com/wiki/stanford-ppl/spatial/${branch}_Regression_Test_History.csv) \n" >> $wiki_file
echo -e "\n## [Pretty History log](https://raw.githubusercontent.com/wiki/stanford-ppl/spatial/${pretty_name}) \n" >> $wiki_file
# echo -e "\n## [Performance Results](https://www.dropbox.com/s/a91ra3wvdyr3x5b/Performance_Results.xlsx?dl=0) \n" >> $wiki_file

stamp_app_comments
stamp_commit_msgs
}

stamp_app_comments() {
  cd ${SPATIAL_HOME}/regression_tests
  comments=(`find . -type f -maxdepth 3 -exec grep PASS {} \; | grep "^PASS: \(.*\).*\*" | sed "s/PASS:.*(/* (/g" | sed "s/*//g"`)
  echo -e "\n# COMMENTS:" >> $wiki_file
  echo -e "\n${comments}" >> $wiki_file
}

update_log() {
  perf_hist=72
  echo "" >> $1
  echo "" >> $1
  progress=(`find . -maxdepth 1 -type f | sort -r`)
  for p in ${progress[@]}; do
    pname=(`echo $p | sed "s/.*[0-9]\+_//g"`)
    cute_plot="[🗠](https://raw.githubusercontent.com/wiki/stanford-ppl/spatial/${branch}_$pname.png)"
    if [[ $p == *"pass"* ]]; then
      echo "**$p**${cute_plot}  " | sed "s/\.\///g" | tee -a $1 $tracker > /dev/null
      t=(`cat $p`)
    elif [[ $p == *"failed_execution_validation"* ]]; then
      echo "<----${p}${cute_plot}  " | sed "s/\.\///g" | tee -a $1 $tracker > /dev/null
      t=0
    elif [[ $p == *"failed_execution_nonexistent_validation"* ]]; then
      echo "<--------${p}${cute_plot}  " | sed "s/\.\///g" | tee -a $1 $tracker > /dev/null
      t=0
    elif [[ $p == *"failed_execution_hanging"* ]]; then
      echo "<------------${p}${cute_plot}  " | sed "s/\.\///g" | tee -a $1 $tracker > /dev/null
      t=0
    elif [[ $p == *"failed_compile_backend_hanging"* ]]; then
      echo "<----------------${p}${cute_plot}  " | sed "s/\.\///g" | tee -a $1 $tracker > /dev/null
      t=0
    elif [[ $p == *"failed_compile_backend_crash"* || $p == *"failed_execution_backend_crash"* ]]; then
      echo "<--------------------${p}${cute_plot}  " | sed "s/\.\///g" | tee -a $1 $tracker > /dev/null
      t=0
    elif [[ $p == *"failed_app_spatial_compile"* ]]; then
      echo "<------------------------${p}${cute_plot}  " | sed "s/\.\///g" | tee -a $1 $tracker > /dev/null
      t=0
    elif [[ $p == *"failed_app_not_written"* ]]; then
      echo "<----------------------------${p}${cute_plot}  " | sed "s/\.\///g" | tee -a $1 $tracker > /dev/null
      t=0
    else
      echo "Unknown result: $p  " | sed "s/\.\///g" | tee -a $1 $tracker > /dev/null
      t=0
    fi

    # # Update performance file
    # perf_file="${SPATIAL_HOME}/spatial.wiki/${branch}_${pname}.csv"
    # lines=(`cat $perf_file | wc -l`)
    # dline=$(($lines-$(($perf_hist-1))))
    # last=(`tail -n1 < $perf_file`)
    # last_color=(`echo ${last[@]} | sed "s/;.*//g"`)
    # if [[ "${last[@]}" = *"$2 $3"* ]]; then
    #   color=$last_color
    #   echo "[SPATIAL NOTICE] Using old color $color for app $p and hash $2 $3"
    # else
    #   if [ "$last_color" = "r" ]; then
    #     color="b"
    #     echo "[SPATIAL NOTICE] Using new color $color for app $p and hash $2 $3 from line ${last[@]}"
    #   else
    #     color="r"
    #     echo "[SPATIAL NOTICE] Using new color $color for app $p and hash $2 $3 from line ${last[@]}"
    #   fi
    # fi

    # echo -ne "\n${color};$t;$2 $3" >> $perf_file

    # # Hack to get rid of empty first line for new files
    # first=(`head -n1 < $perf_file`)
    # if [ "$first" = "" ]; then 
    #   sed -i -e "1d" $perf_file
    # fi

    # if [ $dline -gt $perf_hist ]; then
    #   sed -i -e "1d" $perf_file
    # fi

    # cmd="/usr/local/bin/python2.7 ${SPATIAL_HOME}/static/plotter.py ${branch}_${pname} ${SPATIAL_HOME}/spatial.wiki/"
    # eval "$cmd"


  done
}

update_histories() {

# Update history 
# history_file=${SPATIAL_HOME}/spatial.wiki/${branch}_Regression_Test_History.csv

# Get list of apps that have data
all_apps=(`cat ${wiki_file} | grep "^\*\*pass\|^<-\+failed" | sed "s/<-\+//g" | sed "s/^.*[0-9]\+\_//g" | sed "s/\*//g" | sed "s/\[🗠.*//g" | sort`)

# Determine type for each app to build headers list
for aa in ${all_apps[@]}; do
  if [[ ! "$last_aa" = "$aa" ]]; then
    a=(`echo $aa | sed "s/ //g" | sed "s/\[.*//g"`)
    for bb in ${test_list[@]}; do
      if [[ $bb == "$a|"* ]]; then
        type=(`echo $bb | awk -F'|' '{print $2}'`)
        headers=("${headers[@]}" "${type}|${a}")
      fi
    done
  fi
  last_aa=$aa
done

# Inject the new data to the history
key=(`cat ${pretty_file} | grep KEY | wc -l`)
if [[ $key = 0 ]]; then
    echo "00  KEY:
000 █ = Success
000 ▇ = failed_execution_validation  
000 ▆ = failed_execution_nonexistent_validation  
000 ▅ = failed_execution_hanging  
000 ▄ = failed_compile_backend_hanging 
000 ▃ = failed_compile_backend_crash 
000 ▂ = failed_app_spatial_compile 
000 ▁ = failed_app_not_written 
000 □ = unknown
1 
1
1
1" >> ${pretty_file}
fi
for aa in ${headers[@]}; do
  a=(`echo $aa | sed "s/^.*|//g" | sed "s/\[.*//g"`)
  dashes=(`cat ${wiki_file} | grep "[0-9]\+\_$a\(\ \|\*\|\[\)" | sed "s/\[🗠.*//g" | grep -oh "\-" | wc -l`)
  num=$(($dashes/4))
  if [ $num = 0 ]; then bar=█; elif [ $num = 1 ]; then bar=▇; elif [ $num = 2 ]; then bar=▆; elif [ $num = 3 ]; then bar=▅; elif [ $num = 4 ]; then bar=▄; elif [ $num = 5 ]; then bar=▃; elif [ $num = 6 ]; then bar=▂; elif [ $num = 7 ]; then bar=▁; else bar=□; fi

  infile=(`cat ${pretty_file} | grep $aa | wc -l`)
  if [[ $infile -gt 0 ]]; then # This test exists in history
    logger "Updating $aa in pretty history log"
    cmd="sed -i \"/^${aa}\ \+,/ s/$/$bar/\" ${pretty_file}"
    eval "$cmd"
    # Shave first if too long
    numel=(`cat ${pretty_file} | grep "^$aa\ " | grep -oh "." | wc -l`)
    chars_before_bars=(`cat ${pretty_file} | grep "^$aa\ " | sed "s/,,.*/,,/g" | grep -oh "." | wc -l`)
    if [ $numel -gt $(($hist+$chars_before_bars)) ]; then 
      cmd="sed -i \"s/^${a}\([[:blank:]]*\),,./${a}\1,,/g\" ${pretty_file}"
      eval "$cmd" 
      logger "Shaving $aa in pretty history because its history exceeds $hist"
    fi
  else 
    logger "Detected $aa as a new app!  Adding to pretty history log"
    add=(`printf '%-50s' "$aa"`)
    echo "${add},,${bar}" >> ${pretty_file}
  fi
done

for ac in ${types_list[@]}; do
  infile=(`cat ${pretty_file} | grep "${ac}:" | wc -l`)
  if [[ $infile -eq 0 ]]; then # add this category
    echo "${ac}:" >> ${pretty_file}
  fi
done
infile=(`cat ${pretty_file} | grep "Z Latest Update" | wc -l`)
if [[ $infile -gt 0 ]]; then # add stamp
  cmd="sed -i \"s/Z Latest Update: .*/Z Latest Update: ${tim}/g\" ${pretty_file}"
  eval "$cmd"
else
  echo "Z Latest Update: ${tim}" >> ${pretty_file}
fi

# Append which combo this update is:
at=`date +"%Y-%m-%d_%H-%M-%S"`
line="ZZ ${at} - Spatial ${spatial_hash:0:5} | Delite ${delite_hash:0:5} | LMS ${lms_hash:0:5} | forge ${forge_hash:0:5}"
echo "$line" >> ${pretty_file}

# Sort file
sort $pretty_file > ${pretty_file}.tmp
mv ${pretty_file}.tmp ${pretty_file}

}

## $1 - test class (unit, dense, etc)
init_travis_ci() {
  if [[ ${type_todo} = "chisel" ]]; then

    # Pull Tracker repos
    goto=(`pwd`)
    cd ${SPATIAL_HOME}
    cmd="git clone git@github.com:mattfel1/${1}Tracker.git"
    logger "Pulling TRAVIS CI buttons with command: $cmd"
    eval "$cmd"
    if [ -d "${SPATIAL_HOME}/${1}Tracker" ]; then
      logger "Repo ${1}Tracker exists, prepping it..."
      cd ${SPATIAL_HOME}/${1}Tracker
      cmd="git checkout ${branch}"
      logger "Switching to branch ${branch}"
      eval "$cmd"
      tracker="${SPATIAL_HOME}/${1}Tracker/results"
      ls | grep -v travis | grep -v status | grep -v README | grep -v git | xargs rm -rf
      cp $packet ${SPATIAL_HOME}/${1}Tracker/
    else 
      logger "Repo ${1}Tracker does not exist! Skipping Travis..."
    fi
    cd ${goto}
  fi
}

## $1 - test class (unit, dense, etc)
push_travis_ci() {
  if [[ ${type_todo} = "chisel" ]]; then
    # Pull Tracker repos
    goto=(`pwd`)
    if [ -d "${SPATIAL_HOME}/${1}Tracker" ]; then
      logger "Repo ${1}Tracker exists, pushing it..."
      cd ${SPATIAL_HOME}/${1}Tracker
      git add -A
      git commit -m "auto update"
      git push
    else
      logger "Repo ${1}Tracker does not exist, skipping it!"
    fi
    cd ${goto}
  fi
}

# Helper function for creating script for vulture to use
## 1 - filename for this script
## 2 - type of app this is for (dense, sparse, etc.)
## 3 - id for this test (0, 1, 2, etc.)
## 4 - name of this app
## 5 - directory for this script
## 6 - args
create_script() {
  if [[ $6 = "none" ]]; then
  	args=""
  else
  	args=$6
  fi

  if [[ ${type_todo} = "scala" || ${type_todo} = "maxj" || ${type_todo} = "chisel" ]]; then
    echo "ok!" > /tmp/log
  else
    stamp_commit_msgs
    clean_exit 1 "Error! ${type_todo} type of regression test not yet supported."
  fi

  echo "
#!/bin/bash
# Override env vars to point to a separate directory for this regression test
export SPATIAL_HOME=${SPATIAL_HOME}
export PUB_HOME=${SPATIAL_HOME}/published/Spatial
export HYPER_HOME=${HYPER_HOME}
export PATH=/opt/maxcompiler/bin:/opt/maxcompiler2016/bin:/opt/maxeler/bin:/opt/altera/quartus/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games
export FORGE_HOME=${HYPER_HOME}/forge
export DELITE_HOME=${HYPER_HOME}/delite
export LMS_HOME=${HYPER_HOME}/virtualization-lms-core
export PIR_HOME=${HYPER_HOME}/spatial/published/Spatial
export PATH=/opt/maxeler/maxcompiler-2014.1/bin:$PATH
" >> $1

  if [[ ${type_todo} = "scala" ]]; then
    echo "export JAVA_HOME=/usr/
    " >> $1
  fi

  echo "sleep \$((${3}*${spacing})) # Backoff time to prevent those weird file IO errors
  " >> $1

  if [[ ${type_todo} = "scala" ]]; then
    echo "${PUB_HOME}/bin/spatial --test --outdir=${SPATIAL_HOME}/regression_tests/${2}/${3}_${4}/out ${4} ${args} 2>&1 | tee -a ${5}/log
    " >> $1
  else 
    echo "${PUB_HOME}/bin/spatial --outdir=${SPATIAL_HOME}/regression_tests/${2}/${3}_${4}/out ${4} 2>&1 | tee -a ${5}/log
    " >> $1
  fi

  echo "sed -i \"s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g\" ${5}/log
sed -i \"s/error retrieving current directory/Ignoring getcwd e r r o r/g\" ${5}/log
sed -i \"s/error: illegal sharing of mutable object/Ignoring scattergather mutable sharing e r r o r/g\" ${5}/log

wc=\$(cat ${5}/log | grep \"couldn't find DEG file\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
  echo \"PASS: -1 (${4} Spatial Error)\"
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_backend_hanging.${3}_${4} ]; then
      rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_backend_hanging.${3}_${4}
      echo \"[STATUS] Declaring failure app_not_written\"
      touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_app_not_written.${3}_${4}
    fi
  exit 1
fi

wc=\$(cat ${5}/log | grep \"error\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
  echo \"PASS: -2 (${4} Spatial Error)\"
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_backend_hanging.${3}_${4} ]; then
      rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_backend_hanging.${3}_${4}
      cat ${5}/log
      echo \"[STATUS] Declaring failure build_in_spatial\"
      touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_app_spatial_compile.${3}_${4}
    fi
  exit 1
fi
" >> $1

  if [[ ${type_todo} = "scala" ]]; then
    echo "// cd ${5}/out // No out to go to or thing to patch
    " >> $1
  elif [[ ${type_todo} = "maxj" ]]; then
    echo "cd ${5}/out
# Patch makefile, Because ant won't run inside a makefile if I set the symlinks correctly
sed -i \"s/JAVA_HOME = \/usr\/lib\/jvm\/java-7-openjdk-amd64/JAVA_HOME = \/usr/g\" Makefile
sed -i \"s/ant/\/usr\/share\/ant\/bin\/ant/g\" Makefile
sed -i '4i J_HOME=/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.40.x86_64' Makefile
sed -i \"s/-I\\\$(JAVA_HOME)\/include -I\\\$(JAVA_HOME)\/include\/linux/-I\\\$(J_HOME)\/include -I\\\$(J_HOME)\/include\/linux/g\" Makefile
  " >> $1
  elif [[ ${type_todo} = "chisel" ]]; then
    echo "cd ${5}/out
    " >> $1
  fi

  if [[ ${type_todo} = "scala" ]]; then
    pre_compile="failed_compile_backend_hanging"
    post_compile="failed_compile_backend_hanging" 
  elif [[ ${type_todo} = "maxj" ]]; then
    pre_compile="failed_compile_backend_hanging"
    post_compile="failed_execution_hanging"
  elif [[ ${type_todo} = "chisel" ]]; then
    pre_compile="failed_compile_backend_hanging"
    post_compile="failed_execution_hanging"
  fi

  if [[ ${type_todo} = "scala" ]]; then
    echo "// no make process
    " >> $1
  else # maxj and chisel
    echo "make clean sim 2>&1 | tee -a ${5}/log
wc=\$(cat ${5}/log | sed \"s/Error [0-9]\+ (ignored)/ignore e r r o r/g\" | grep \"BUILD FAILED\\|Error 1\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
  echo \"PASS: -3 (${4} Spatial Error)\"
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/${pre_compile}.${3}_${4} ]; then
      rm ${SPATIAL_HOME}/regression_tests/${2}/results/${pre_compile}.${3}_${4}
      echo \"[STATUS] Declaring failure compile_maxj\"
      touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_backend_crash.${3}_${4}
  fi
  exit 1
fi

rm ${SPATIAL_HOME}/regression_tests/${2}/results/${pre_compile}.${3}_${4}
touch ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4}
" >> $1
    if [[ ${type_todo} = "maxj" ]]; then
      echo "bash ${5}/out/run.sh ${args} 2>&1 | tee -a ${5}/log
      " >> $1
    elif [[ ${type_todo} = "chisel" ]]; then
      echo "bash ${5}/out/run.sh \"${args}\" 2>&1 | tee -a ${5}/log
wc=\$(cat ${5}/log | grep \"Error: App\\|Segmentation fault\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
  echo \"PASS: -4 (${4} Spatial Error)\"
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4} ]; then
      rm ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4}
      echo \"[STATUS] Declaring failure compile_maxj\"
      touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_execution_backend_crash.${3}_${4}
  fi
  exit 1
fi
    " >> $1
    fi
  fi

  echo "if grep -q \"PASS: 1\" ${5}/log; then
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4}
    touch ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
  cat ${5}/log | grep \"Kernel done, cycles\" | sed \"s/Kernel done, cycles = //g\" > ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
  fi
elif grep -q \"PASS: true\" ${5}/log; then
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4}
    touch ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
  cat ${5}/log | grep \"Kernel done, cycles\" | sed \"s/Kernel done, cycles = //g\" > ${SPATIAL_HOME}/regression_tests/${2}/results/pass.${3}_${4}
  fi
elif grep -q \"PASS: 0\" ${5}/log; then
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4}
    echo \"[STATUS] Declaring failure validation\"
  touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_execution_validation.${3}_${4}
  fi
elif grep -q \"PASS: false\" ${5}/log; then
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4}
    echo \"[STATUS] Declaring failure validation\"
    touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_execution_validation.${3}_${4}
  fi
else 
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4} ]; then
    rm ${SPATIAL_HOME}/regression_tests/${2}/results/${post_compile}.${3}_${4}
    echo \"[STATUS] Declaring failure no_validation_check\"
  touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_execution_nonexistent_validation.${3}_${4}    
  fi
  fi" >> $1

}


# Helper function for launching regression tests
launch_tests() {
  # # Use magic to free unused semaphores
  # logger "Killing semaphores"
  # cmd="for semid in `ipcs -s | cut -d\" \" -f 2` ; do pid=`ipcs -s -i $semid | tail -n 2 | head -n 1 | awk '{print $5}'`; running=`ps --no-headers -p $pid | wc -l` ; if [ $running -eq 0 ] ; then ipcrm -s $semid; fi ; done"
  # $cmd 2> /dev/null
  # Free old screens
  logger "Killing old screen sessions"
  screen -ls | grep "${branch}_${type_todo}" | cut -d. -f1 | awk '{print $1}' | xargs kill
  screen -wipe
  logger "Killing maxeleros jobs"
  ps aux | grep -ie mattfel | grep -v ssh | grep -v bash | awk '{system("kill -9 " $2)}'

  IFS=$'\n'
  # Collect the regression tests by searching for "// Regression (<type>)" tags
  test_list=(`grep -R "// Regression" ${PUB_HOME}/apps/src | sed 's/^.* with //g' | sed 's/App//g' | sed 's/ \/\/ Regression (/|/g' | sed 's/) \/\/ Args: /|/g' | sed 's/ /-/g'`)

  # Assemble regression types
  for t in ${test_list[@]}; do
    # logger "Processing app: $t"
    tp=(`echo $t | awk -F'|' '{print $2}'`)
    if [[ ! ${types_list[*]} =~ "$tp" ]]; then
      types_list=("${types_list[@]}" $tp)
    fi
  done

  # Make reg test dir
  rm -rf ${SPATIAL_HOME}/regression_tests;mkdir ${SPATIAL_HOME}/regression_tests

  for ac in ${types_list[@]}; do 
    logger "Preparing vulture directory for $ac..."
    # Create vulture dir
    rm -rf ${SPATIAL_HOME}/regression_tests/${ac};mkdir ${SPATIAL_HOME}/regression_tests/${ac}
    mkdir ${SPATIAL_HOME}/regression_tests/${ac}/results
    cd ${SPATIAL_HOME}/regression_tests/${ac}

    # Create the package for each app
    i=0
    for t in ${test_list[@]}; do
      if [[ $t == *"|${ac}|"* && (${tests_todo} == "all" || $t == *"|${tests_todo}|"*) ]]; then
        appname=(`echo $t | sed 's/|.*$//g'`)
        appargs=(`echo $t | sed 's/.*|.*|//g' | sed 's/-/ /g'`)
        # Initialize results
        touch ${SPATIAL_HOME}/regression_tests/${ac}/results/failed_compile_backend_hanging.${i}_${appname}

        # Make dir for this vulture job
        vulture_dir="${SPATIAL_HOME}/regression_tests/${ac}/${i}_${appname}"
        rm -rf $vulture_dir;mkdir $vulture_dir
        cmd_file="${vulture_dir}/cmd"

        # Create script
        logger "Writing script for ${i}_${appname}"
        create_script $cmd_file ${ac} $i ${appname} ${vulture_dir} "$appargs"

        ((i++))
      fi
    done
    # Run vulture
    cd ${SPATIAL_HOME}/regression_tests/${ac}/
    logger "Executing vulture script in ${ac} directory..."
    bash ${SPATIAL_HOME}/static/vulture.sh ${ac}_${branch}_${type_todo}
    logger "Script executed!"

  done


}
