#!/bin/sh

# Argument 1 = Test type (maxj, scala, chisel, etc...)

REGRESSION_HOME="${HOME}/regression"

## Helper for deleting directories when you still have those nfs files stuck in use
# 1 - directory to delete
stubborn_delete() {
  rm -rf $1 > /tmp/todelete 2>&1
  undeletable=(`cat /tmp/todelete | wc -l`)
  if [[ $undeletable -gt 0 ]]; then
    while read p; do
      f=(`echo $p | sed 's/rm: cannot remove \`//g' | sed "s/': Device or resource busy//g"`)
      fuser -k $f
    done </tmp/todelete
  fi
  rm -rf $1
}

## Helper function for checking if git checkouts were successful
# 1 - repo to check
checkout_success() {
  x=(`cat /tmp/gitstuff | grep error | grep "known to git" | wc -l`)
  if [[ $x -gt 0 ]]; then
    clean_exit "4" "Git error on ${1}"
  fi
}

## Helper function for safe exiting
# 1 - Error code
# 2 - (optional) error message
clean_exit() {
  summary=`sed -n '1p' $packet | sed -r "s/\| Status-[^\|]*\|/| Status- Failed (${1}) |/g"`
  echo "$summary"
  echo "`date` - $summary (clean_exit)" >> ${REGRESSION_HOME}/regression_history.log
  if [[ -z $2 ]]; then
    echo "Unknown error (${1})" >> $log
  else
    echo "Error: ${2} (${1})" >> $log
  fi

  logger "${2}"

  # errfile=`echo $packet | sed 's/ack/error/g'`
  # rm $packet
  exit 1
}


get_packet() {
# Make sure there is only one packet to work on
wc=`ls $REGRESSION_HOME | grep "${test_to_run}.new" | wc -l`
multiple_new=false
if [[ $wc -gt 1 ]]; then
  #echo "Warning: More than one packet detected.  Grabbing newest!" > ${REGRESSION_HOME}/log
  cd ${REGRESSION_HOME}
  files=(*)
  new_packets=()
  for f in ${files[@]}; do if [[ $f = *".new"* ]]; then new_packets+=($f); fi; done
  sorted_packets=( $(for arr in "${new_packets[@]}"; do echo $arr; done | sort) )
  packet=${sorted_packets[-1]}
  multiple_new=true
  cd -
elif [[ $wc = 0 ]]; then
  # echo "`date` - no new packet" >> ${REGRESSION_HOME}/regression_history.log
  exit 1
else
  # Find the new packet 
  packet=`ls $REGRESSION_HOME | grep "${test_to_run}.new"`  
fi

packet="${REGRESSION_HOME}/$packet"

# Acknowledge packet
ackfile=`echo $packet | sed 's/new/ack/g'`
if [ -f $packet ]; then
  mv $packet $ackfile
fi
packet=$ackfile

sleep 2 # Because wft

# Set vars based on this packet
type_todo=`sed -n '4p' $packet`
if [[ ! "${type_todo}" = "${test_to_run}" ]]; then
  echo "Error: packet mislabeled.  Cannot run ${test_to_run} test on ${type_todo} packet!" > ${REGRESSION_HOME}/log
  clean_exit 6
fi
tests_todo=`sed -n '5p' $packet`
tim=`sed -n '2p' $packet`
branch=`sed -n '12p' $packet`
dirname="${REGRESSION_HOME}/testdir-${branch}.${type_todo}.${tests_todo}.${tim}"
HYPER_HOME="$dirname/hyperdsl"
DELITE_HOME="$HYPER_HOME/delite"
FORGE_HOME="$HYPER_HOME/forge"
LMS_HOME="$HYPER_HOME/virtualization-lms-core"
SPATIAL_HOME="$HYPER_HOME/spatial"
PUB_HOME="$SPATIAL_HOME/published/Spatial"
WIKI_HOME="$SPATIAL_HOME/spatial.wiki"
wiki_file="${WIKI_HOME}/${branch}Branch-${type_todo}Test-Regression-Tests-Status.md"
spatial_hash=`sed -n '8p' $packet`
delite_hash=`sed -n '9p' $packet`
lms_hash=`sed -n '10p' $packet`
forge_hash=`sed -n '11p' $packet`
PIR_HOME=$SPATIAL_HOME
pretty_name=${branch}Branch_${type_todo}Test_Pretty_Regression_Test_History.csv
pretty_file=${SPATIAL_HOME}/spatial.wiki/${pretty_name}
log="${REGRESSION_HOME}/${tim}.${branch}.${type_todo}.log"

logger "Got packet.  `sed -n '1p' $packet`"
if [ $multiple_new = "true" ]; then
  logger "Had to get newest because multiple packets found:"
  logger "   ${sorted_packets[@]}"
fi

}

stamp_commit_msgs() {
  logger "Stamping commit messages"
  cd $SPATIAL_HOME
  spatial_msg=`git log --stat --name-status ${spatial_hash}^..${spatial_hash}`
  cd $DELITE_HOME
  delite_msg=`git log --stat --name-status ${delite_hash}^..${delite_hash}`
  cd $LMS_HOME
  lms_msg=`git log --stat --name-status ${lms_hash}^..${lms_hash}`
  cd $FORGE_HOME
  forge_msg=`git log --stat --name-status ${forge_hash}^..${forge_hash}`
  echo "
# Commits
" >> $wiki_file
  echo -e "\nSpatial commit: \n\`\`\`\n${spatial_msg}\n\`\`\`" >> $wiki_file
  echo -e "\nDelite commit: \n\`\`\`\n${delite_msg}\n\`\`\`" >> $wiki_file
  echo -e "\nLMS commit: \n\`\`\`\n${lms_msg}\n\`\`\`" >> $wiki_file
  echo -e "\nForge commit: \n\`\`\`\n${forge_msg}\n\`\`\`" >> $wiki_file
  echo "
# Test summary
" >> $wiki_file
  summary=`sed -n '1p' $packet`
  echo -e "\n\n${summary}" >> $wiki_file
}


# Helper function for ensuring a file or dir exists
## 1 - directory to check
## 2 - error code (for tracing back where failure occured)
exists() {
  # Create error dictionary
  error_dict=(
    "N/A" #0
    "Error!  Hyperdsl did not clone properly" #1
    "Error!  Spatial did not clone properly" #2
    "Error!  Spatial did not build properly. No published" #3
    )

  if [ ! -d "$1" ]; then
    clean_exit $2 ${error_dict[$2]}
  fi
}

# Helper function for logging a message
## 1 - Message
logger() {
  echo "[${phase}] `date` - $1" >> $log
}

# Helper function for doing git stuff, so
#  that I can edit this script without crashing things (bash is odd)
git_things() {

  # Make directory for this regression test
  stubborn_delete $dirname
  mkdir $dirname
  cp $packet $dirname

  # Clone repos in dirname
  cd $dirname
  export dirname=${dirname}
  export HYPER_HOME=${HYPER_HOME}
  export DELITE_HOME=${DELITE_HOME}
  export FORGE_HOME=${FORGE_HOME}
  export LMS_HOME=${LMS_HOME}
  export SPATIAL_HOME=${SPATIAL_HOME}
  export PUB_HOME=${PUB_HOME}
  export WIKI_HOME=${WIKI_HOME}
  export wiki_file=${wiki_file}
  export spatial_hash=${spatial_hash}
  export delite_hash=${delite_hash}
  export lms_hash=${lms_hash}
  export forge_hash=${forge_hash}
  export PIR_HOME=${PIR_HOME}
  export JAVA_HOME=/usr/
  logger "Cloning hyperdsl..."
  git clone git@github.com:stanford-ppl/hyperdsl.git > /dev/null 2>&1
  logger "Cloning done!"
  exists "$HYPER_HOME" 1
  cd $HYPER_HOME
  logger "Switching hyperdsl branch"
  git submodule update --init > /dev/null 2>&1
  git fetch > /dev/null 2>&1
  git checkout -b spatial origin/spatial > /dev/null 2>&1
  cd $DELITE_HOME
  logger "Switching delite commit (${delite_hash})"
  git fetch > /dev/null 2>&1
  git checkout ${delite_hash} > /tmp/gitstuff 2>&1
  checkout_success "Delite"
  cd $FORGE_HOME
  logger "Switching forge commit (${forge_hash})"
  git fetch > /dev/null 2>&1
  git checkout ${forge_hash} > /tmp/gitstuff 2>&1
  checkout_success "Forge"

  # git pull origin spatial > /dev/null 2>&1
  cd $LMS_HOME
  logger "Switching lms commit (${lms_hash})"
  git fetch > /dev/null 2>&1
  git checkout ${lms_hash} > /tmp/gitstuff 2>&1
  checkout_success "LMS"
  # git pull origin spatial > /dev/null 2>&1
  cd $HYPER_HOME
  logger "Cloning spatial..."
  git clone git@github.com:stanford-ppl/spatial.git > /dev/null 2>&1
  logger "Cloning done!"
  exists "$SPATIAL_HOME" 2
  cd $SPATIAL_HOME
  git fetch > /dev/null 2>&1
  logger "Switching to spatial commit (${spatial_hash})"
  cmd="git checkout ${spatial_hash}"
  eval "$cmd" > /tmp/gitstuff 2>&1
  checkout_success "Spatial"
  logger "Cloning wiki..."
  git clone git@github.com:stanford-ppl/spatial.wiki.git  > /dev/null 2>&1
  git checkout -f HEAD  > /dev/null 2>&1
  logger "Cloning done!"
}

# Get test to run
test_to_run=${1}
if [[ "${test_to_run}" = "scala" ]]; then
  # Give maxj headstart
  sleep 20
fi

# Receive and parse packet
phase="INIT"
get_packet

# Do all the cloning and building
sleep 10 # Give it some time to upload to github
phase="GIT"
git_things

# Switch to revision-controlled bash script
phase="FUNCTIONS"
logger "Sourcing functions..."
source ${SPATIAL_HOME}/scripts/regression_functions.sh
if [[ $? -ne 0 ]]; then
  logger "${SPATIAL_HOME}/scripts/regression_functions.sh is nonexistent or has error!"
  clean_exit 7 "${SPATIAL_HOME}/scripts/regression_functions.sh is nonexistent or has error!"
fi
logger "Sourcing successful!"

# Wait for channel to be free
phase="COORDINATION"
logger "Looking for senior files..."
coordinate

# Build spatial
phase="BUILD"
build_spatial

# Lock file
logger "Locking my packet!"
lockfile=`echo $packet | sed 's/ack/lock/g'`
mv $packet $lockfile
packet=$lockfile

# Launch tests
phase="VULTURE"
launch_tests

# Delay while tests run
phase="NAPPING"
for i in `seq 1 $numpieces`; do
  dots="($i / $numpieces x $((delay/numpieces))s)"
  logger "Sleeping $dots"
  sleep $((delay/numpieces))
done

# Update result file
phase="RESULTS"
collect_results

# Update history
phase="HISTORY"
update_histories

# Clean up regression files and push to git
phase="CLEANUP"
wipe_clean









# failed 1
# failed 2
# failed 3
# failed 4
# failed 5
# failed 6
# failed 7
# failed 8
# see clean_exit # or exists

                                   