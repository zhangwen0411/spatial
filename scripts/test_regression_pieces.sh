# Update history 
result_file=${SPATIAL_HOME}/spatial.wiki/maxj-Regression-Tests-Status.md
hist=72
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

################# section testing

history_file=${SPATIAL_HOME}/spatial.wiki/maxj_Regression_Test_History.csv
pretty_file=${SPATIAL_HOME}/spatial.wiki/maxj_Pretty_Regression_Test_History.csv
all_apps=(`cat ${result_file} | grep "^\*\*pass\|^<-\+failed" | sed "s/<-\+//g" | sed "s/^.*[0-9]\+\_//g" | sed "s/\*//g" | sed "s/\[🗠.*//g" | sort`)
last_line=(`tail -n1 < $pretty_file`)
last_line=$(printf " %s" "${last_line[@]}")
if [[ "$last_line" = *"$hash_str / $dhash_str"* ]]; then
	commit_change=false
else
	commit_change=true
fi


## stuff
# ...
###

# Draw delta-commit arrows
if [ "$commit_change" = "true" ]; then
	arrow="↖"
	varrow="↑"
	marker="↰"
else
	arrow=" "
	marker=" "
fi
cmd="sed -i \"/^(commit change)\ \+,/ s/$/$arrow/\" ${pretty_file}"
eval "$cmd"
cmd="sed -i \"/^(commit change)\ \+,/ s/$/$varrow /\" ${history_file}"
eval "$cmd"
numel1=(`cat ${history_file} | grep "^(commit change)\ " | grep -oh "." | wc -l`)
numel2=(`cat ${history_file} | grep "^(commit change)\ " | wc -l`)
numel=$(($numel1 / $numel2))
if [ $numel -gt $hist ]; then
	cmd="sed -i \"s/^(commit change)\([[:blank:]]*\),,../(commit change)\1,,/g\" ${history_file}"
	eval "$cmd"
fi
numel1=(`cat ${pretty_file} | grep "^(commit change)\ " | grep -oh "." | wc -l`)
numel2=(`cat ${pretty_file} | grep "^(commit change)\ " | wc -l`)
numel=$(($numel1 / $numel2))
if [ $numel -gt $(( $hist + $chars_before_bars )) ]; then 
	cmd="sed -i \"s/^(commit change)\([[:blank:]]*\),,./(commit change)\1,,/g\" ${pretty_file}"
	eval "$cmd"	
fi


cd ${SPATIAL_HOME}
stamp=(`date +"%b%d.%H:%M"`)
lines=(`cat $history_file | wc -l`)
commitlines=(`cat $history_file | grep "^[a-zA-Z]\+[0-9]\+\.[0-9]\+:[0-9]\+-" | wc -l`)
if [ $commitlines -ge $hist ]; then
	dline=$(($lines-$(($hist-1))))
	sed -i -e "${dline}d" $history_file
fi
echo "$stamp- $hash_str / $dhash_str $marker" >> $history_file
lines=(`cat $pretty_file | wc -l`)
commitlines=(`cat $pretty_file | grep "^[a-zA-Z]\+[0-9]\+\.[0-9]\+:[0-9]\+-" | wc -l`)
if [ $commitlines -ge $hist ]; then
	dline=$(($lines-$(($hist-1))))
	sed -i -e "${dline}d" $pretty_file
fi
echo "$stamp- $hash_str / $dhash_str $marker" >> $pretty_file
