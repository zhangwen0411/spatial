#!/bin/bash


# This script is for scraping data on the specified channels.
#  ** Remember not to use commas in the lists! **
#  ** Be careful not to move any sims that are still in progress! **

##########
# CONFIG #
##########
test_name="GDA" # Label for data to be scraped
blank_file="maxj2/Summary_GDA_Manual.csv" # Location on remote of blank csv with all runs included (highest # channel)
buildserver="max" # Tag to look for inside file [max / londonvm]
testname_as_prefix="false" # Flag indicating if test_name is a prefix (for printing to csv with multiple tests)
base_dir="./${test_name}" # Move whatever maxj/ you want to scrape to this dir
insertion_file="/home/mattfel/characterization/${test_name}.csv" # Specify file that holds current unpopulated table
server="maxeler" # Specify server to grab data from
# start_channel=1 # Used if mv via script
# stop_channel=30 # Used if mv via script


##########
# SCRIPT #
##########

echo "Make sure /mnt/${server} is mounted"
sleep 2
echo "Results will appear at ${insertion_file}"

# Handle local files
if [ -f /mnt/${server}/${base_dir}/${blank_file} ]; then
	cmd="cp /mnt/${server}/${base_dir}/${blank_file} ${insertion_file}"
else
	echo "ERROR: Could not find blank file at /mnt/${server}/${base_dir}/${blank_file}"
	exit 1
fi
eval "$cmd"
fname="${insertion_file}.populated"
cmd="mv ${fname} ${fname}.displaced"
eval "${cmd}"
cmd="cp ${insertion_file} ${fname}"
eval "${cmd}"

# USE THIS SECTION IF YOU WANT TO MV DATA FROM ~ TO THE base_dir.  COMMENT OUT IF DATA IS ALREADY THERE!
# # Move specified channels
# echo "Moving dirs..."
# for i in `seq $start_channel $stop_channel`; do
# 	echo -n "."
# 	cmd="ssh ${server}.stanford.edu \"mv maxj${i} ${base_dir}\""
# 	eval "$cmd" 2> /dev/null
# done
# echo ""

# Run Raghu scripts
directories=(`ls -F /mnt/${server}/${base_dir}/ | grep / | sed "s/\///g"`)
echo "Running comb scripts..."
# Get summaries
echo -n "0|"
for d in `seq 1 ${#directories[@]}`; do
	echo -n " "
done
echo "|100%"
echo -n "  "

for d in "${directories[@]}"; do
  	echo -n "."
	if [[ "${server}" = "maxeler" ]]; then 
		cmd="ssh -p 3033 mfeldman@portal.maxeler.com \"cd scripts;bash ./res ../${base_dir}/${d}\""
	else
		cmd="ssh ${server}.stanford.edu \"cd scripts;bash ./res ../${base_dir}/${d}\""
	fi
	# echo $cmd
	eval "$cmd" 2> /dev/null
done
echo ""

# Run Matt parser scripts
# Get tag
if [ "${buildserver}" = "maxeler" ]; then
	sserver="max"
else
	sserver=$buildserver
fi
echo "Parsing..."
echo -n "  "
for d in "${directories[@]}"; do
	# Extract fields from summary file
	file="/mnt/${server}/scripts/${base_dir}/${d}/${d}.summary"
	usage=(`cat /mnt/${server}/${test_name}/${d}/Top_MAX4848A_DFE/_build.log | grep "%)"`)
	usage=(`echo ${usage[@]} | sed 's/\// /g'`)		num=(`echo "$d" | sed 's/maxj//g'`)
	if [ "$testname_as_prefix" = "true" ]; then
		if [[ "$buildserver" = "maxeler" ]]; then
			tag="${test_name}\/max.${num}"
		fi
	else
		tag="${sserver}${num}"
	fi

	# echo -e "${usage[@]} \n"
	if [ -f ${file} ]; then 
		text=(`cat $file`)
		if [ "$text" != "" ]; then
			fields=(
			" "
			"logic and registers"
			"LUT logic   "
			"for registers"
			"for memory "
			"Logic Subtotal"
			"Register Subtotal"
			"final placement"
			"recoverable"
			"unavailable"
			"ALMs needed \["
			" "
			"7 input"
			"6 input"
			"5 input"
			"4 input"
			"<=3 input"
			"Combinational ALUT usage for logic"
			"Combinational ALUT usage for route-throughs"
			"64-address"
			"32-address"
			"16-address"
			"Memory ALUT usage"
			"ALUT total"
			" "
			"implementation registers"
			"Routing optimization"
			"Total dedicated regs"
			"DSP"
			"M20K blocks"
			"Difficulty"
			)

			res=()

			# Print useful progress bar
			if [ -f $file ]; then
				echo -n "."	
			else 
				echo -n "x"
			fi

			# Collect data 
			data=","
			for f in "${fields[@]}"
			do
				if [ -f $file ]; then	
					if [ "$f" = " " ]; then
						res+=" "
						val=" "
					elif [ "$f" = "Logic Subtotal" ]; then
						sum=$((${res[1]} + ${res[2]}))
						res+=($sum)
						val=$sum
					elif [ "$f" = "Register Subtotal" ]; then
						sum=$((${res[3]} + ${res[4]}))
						res+=($sum)
						val=$sum
					elif [ "$f" = "ALUT total" ]; then
						sum=$((${res[16]} + ${res[17]} + ${res[21]}))
						res+=($sum)
						val=$sum
					elif [ "$f" = "Total dedicated regs" ]; then
						sum=$((${res[23]} + ${res[24]}))
						res+=($sum)
						val=$sum
					else
						res+=(`cat $file | grep "$f" | awk -F';' '{print $3}' | sed 's/ //g' | sed 's/,//g' | sed 's/\/.*//g'`)
						val=`cat $file | grep "$f" | awk -F';' '{print $3}' | sed 's/ //g' | sed 's/,//g' | sed 's/\/.*//g'`
					fi
				else 
					val='NA'
				fi
				data+="${val},"
			done

			# Pop in data
			cmd="sed -i 's/,${tag},/,${data},,,,,,,,,,,,,,,,${tag},,,,,${usage[@]}/g' ${fname}"
			# echo $cmd
			eval ${cmd}
		else
			cmd="sed -i 's/,${tag},/,${tag},,,${usage[@]}/g' ${fname}"
			eval ${cmd}
			echo -n "x"
		fi
	else 
		cmd="sed -i 's/,${tag},/,${tag},,,${usage[@]}/g' ${fname}"
		eval ${cmd}
		echo -n "x"
	fi

done
echo ""
