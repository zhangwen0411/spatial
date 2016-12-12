#!/bin/sh

## This file contains all the functions used for regression testing.
##   It is called from the receive.sh, which handles path variables
##   and git checkouts on a server-specific basis


## Function for cleaning up iff test was successful
wipe_clean() {
summary=`sed -n '1p' $packet | sed -r "s/\| Status-[^\|]*\|/| Status- Success |/g"`

echo "`date` - $summary (wipe_clean)" >> ${REGRESSION_HOME}/regression_history.log

cd $WIKI_HOME
logger "Pushing wiki..."
git add -A
git commit -m "Automated incron update"
git push

mv $log ${dirname}/../${tim}.${type_todo}.log
rm $packet
stubborn_delete

ps aux | grep -ie mattfel | grep -v ssh | grep -v bash | awk '{system("kill -9 " $2)}'

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
  logger "Collecting results for ${ac} apps"
  cd ${SPATIAL_HOME}/regression_tests/${ac}/results
  echo "

# ${ac}:
" | awk '{print toupper($0)}' >> $wiki_file
  update_log $wiki_file `echo ${spatial_hash} | cut -c1-5` `echo ${delite_hash} | cut -c1-5`
done

echo -e "\n\n***\n\n" >> $wiki_file

# Link to logs
# echo -e "\n## [History log](https://raw.githubusercontent.com/wiki/stanford-ppl/spatial/${branch}_Regression_Test_History.csv) \n" >> $wiki_file
echo -e "\n## [Prettier History log](https://raw.githubusercontent.com/wiki/stanford-ppl/spatial/${pretty_name}) \n" >> $wiki_file
# echo -e "\n## [Performance Results](https://www.dropbox.com/s/a91ra3wvdyr3x5b/Performance_Results.xlsx?dl=0) \n" >> $wiki_file

stamp_commit_msgs
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
      echo "**$p**${cute_plot}  " | sed "s/\.\///g" >> $1
      t=(`cat $p`)
    elif [[ $p == *"failed_did_not_finish"* ]]; then
      echo "<------------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
      t=0
    elif [[ $p == *"failed_compile_stuck"* ]]; then
      echo "<----------------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
      t=0
    elif [[ $p == *"failed_app_not_written"* ]]; then
      echo "<----------------------------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
      t=0
    elif [[ $p == *"failed_build_in_spatial"* ]]; then
      echo "<------------------------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
      t=0
    elif [[ $p == *"failed_compile_maxj"* ]]; then
      echo "<--------------------${p}${cute_plot}  " | sed "s/\.\///g" >> $1
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


# Helper function for creating script for vulture to use
## 1 - filename for this script
## 2 - type of app this is for (dense, sparse, etc.)
## 3 - id for this test (0, 1, 2, etc.)
## 4 - name of this app
## 5 - directory for this script
## 6 - args
create_script() {
  if [[ ${type_todo} = "maxj" ]]; then
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

sleep \$((${3}*${spacing})) # Backoff time to prevent those weird file IO errors

cd ${PUB_HOME}
${PUB_HOME}/bin/spatial --outdir=${SPATIAL_HOME}/regression_tests/${2}/${3}_${4}/out ${4} 2>&1 | tee -a ${5}/log

sed -i \"s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g\" ${5}/log
sed -i \"s/error retrieving current directory/Ignoring getcwd e r r o r/g\" ${5}/log
sed -i \"s/error: illegal sharing of mutable object/Ignoring scattergather mutable sharing e r r o r/g\" ${5}/log

wc=\$(cat ${5}/log | grep \"couldn't find DEG file\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
  echo \"PASS: -1 (${4} Spatial Error)\"
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4} ]; then
      rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4}
      echo \"[STATUS] Declaring failure app_not_written\"
      touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_app_not_written.${3}_${4}
    fi
  exit 1
fi

wc=\$(cat ${5}/log | grep \"error\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
  echo \"PASS: -2 (${4} Spatial Error)\"
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4} ]; then
      rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4}
      cat ${5}/log
      echo \"[STATUS] Declaring failure build_in_spatial\"
      touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_build_in_spatial.${3}_${4}
    fi
  exit 1
fi

cd ${5}/out
# Patch makefile, Because ant won't run inside a makefile if I set the symlinks correctly
sed -i \"s/JAVA_HOME = \/usr\/lib\/jvm\/java-7-openjdk-amd64/JAVA_HOME = \/usr/g\" Makefile
sed -i \"s/ant/\/usr\/share\/ant\/bin\/ant/g\" Makefile
sed -i '4i J_HOME=/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.40.x86_64' Makefile
sed -i \"s/-I\\\$(JAVA_HOME)\/include -I\\\$(JAVA_HOME)\/include\/linux/-I\\\$(J_HOME)\/include -I\\\$(J_HOME)\/include\/linux/g\" Makefile

make clean sim 2>&1 | tee -a ${5}/log
wc=\$(cat ${5}/log | sed \"s/Error 1 (ignored)/ignore e r r o r/g\" | grep \"BUILD FAILED\\|Error 1\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
  echo \"PASS: -3 (${4} Spatial Error)\"
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4} ]; then
      rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4}
      echo \"[STATUS] Declaring failure compile_maxj\"
      touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_maxj.${3}_${4}
  fi
  exit 1
fi

rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4}
touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
cd out
bash ${5}/out/run.sh $6 2>&1 | tee -a ${5}/log
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
  elif [[ ${type_todo} = "scala" ]]; then
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

sleep \$((${3}*${spacing})) # Backoff time to prevent those weird file IO errors

cd ${PUB_HOME}
${PUB_HOME}/bin/spatial --test --outdir=${SPATIAL_HOME}/regression_tests/${2}/${3}_${4}/out ${4} ${6} 2>&1 | tee -a ${5}/log

sed -i \"s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g\" ${5}/log
sed -i \"s/error retrieving current directory/Ignoring getcwd e r r o r/g\" ${5}/log
sed -i \"s/error: illegal sharing of mutable object/Ignoring scattergather mutable sharing e r r o r/g\" ${5}/log

wc=\$(cat ${5}/log | grep \"couldn't find DEG file\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
  echo \"PASS: -1 (${4} Spatial Error)\"
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4} ]; then
      rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4}
      echo \"[STATUS] Declaring failure app_not_written\"
      touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_app_not_written.${3}_${4}
    fi
  exit 1
fi

wc=\$(cat ${5}/log | grep \"error\" | wc -l)
if [ \"\$wc\" -ne 0 ]; then
  echo \"PASS: -2 (${4} Spatial Error)\"
  if [ -e ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4} ]; then
      rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4}
      cat ${5}/log
      echo \"[STATUS] Declaring failure build_in_spatial\"
      touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_build_in_spatial.${3}_${4}
    fi
  exit 1
fi

rm ${SPATIAL_HOME}/regression_tests/${2}/results/failed_compile_stuck.${3}_${4}
touch ${SPATIAL_HOME}/regression_tests/${2}/results/failed_did_not_finish.${3}_${4}
cd out
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
  else
    echo -e "Error! ${type_todo} type of regression test not yet supported."
    stamp_commit_msgs
    exit 1
  fi
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
    logger "Processing app: $t"
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
        touch ${SPATIAL_HOME}/regression_tests/${ac}/results/failed_compile_stuck.${i}_${appname}

        # Make dir for this vulture job
        vulture_dir="${SPATIAL_HOME}/regression_tests/${ac}/${i}_${appname}"
        rm -rf $vulture_dir;mkdir $vulture_dir
        cmd_file="${vulture_dir}/cmd"

        # Create script
        create_script $cmd_file ${ac} $i ${appname} ${vulture_dir} "$appargs"

        ((i++))
      fi
    done
    # Run vulture
    cd ${SPATIAL_HOME}/regression_tests/${ac}/
    logger "Executing vulture script..."
    bash ${SPATIAL_HOME}/static/vulture.sh ${ac}_${branch}_${type_todo}
    logger "Script executed!"

  done


}
