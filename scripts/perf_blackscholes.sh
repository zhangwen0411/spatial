#!/bin/bash

##########
# CONFIG #
##########

# **Fixed to use a staging area

testname="BlackScholes"
server="max"
start_channel=1
file=${SPATIAL_HOME}/apps/src/BlackScholes.scala
staging_area=${HOME}/staging/${testname}

innerPar=(2 4 8)
outerPar=(1)
dim0=(192 1536 9600)
startAt=6 # Indicates channel to start at, in case something breaks in the middle

i=$start_channel

rm -rf ${staging_area}
mkdir ${staging_area}
# Create results file
fname="/tmp/Summary_${testname}_Manual.csv"
cmd="mv ${fname} ${fname}.displaced"
eval "${cmd}"
cmd="touch ${fname}"
eval "${cmd}"
echo "${t},,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,=4*M1,=AH1/AL1,=X1/(2*L1),=SUM(T1:W1)/(2*(L1-S1)),=(AA1+AB1)/(2*(K1-Z1)),=(Z1/K1) + ((AA1+AB1)/K1)*AP1,=H1/N1,=(J1*4-2*Y1-4*AG1)/4,,\"=CEILING((1-AO1)*SUM(T1:W1)/2,1)\",\"=CEILING((1-AQ1)*K1/2,1)\",=O1-AV1+AU1,=O1/AW1,\"=CEILING((0.615-AM1)*AF1/2,1)\",=AY1+AV1+AU1,=(O1-AZ1)/O1,,,,=C1*D1*BC1,=BC1*150000" >> $fname
echo "innerPar,outerPar,tileSize,words,channel,ALMs,Register & Logic,Logic Only,Register Only,Mem,Logic Subtotal,Register Subtotal,Placement Total,Recoverable,Unavailable,\"Needed\",LUTs,7 Inputs,6 Inputs,5 Inputs,4 Inputs,<= 3 Inputs,Logic Subtotal,Route-Thru,64 Address Mems,32-Address Mems,16-Address Mems,Mem Subtotal,Total,Registers,Implementation,Routing,Total,DSPs,BRAM,Effort,ALM Regs,% ALM Regs Used,Logic Packed %,Total Logic Pack Ratio (<=6),Mem Packed %,Total Mem Pack Ratio,% ALMs with Reg + Logic,Est. Impl Reg-Only ALMs,Recoverable ALMs,Est. Logic ALM Packing,Est. Mem ALM Packing,Unaccounted,% Unaccounted,Guess,Total,Diff,channel,iters,words,time,cycles" >> $fname

for ip in ${innerPar[@]}; do
  for op in ${outerPar[@]}; do
    for d0 in ${dim0[@]}; do

    	if [ $i -ge $startAt ]; then

	      	echo "Running $ip $op $d0 on channel $i..."

	      	# Edit files
	      	sed -i "s/val innerPar = [0-9]\+/val innerPar = $ip/g" $file
	      	sed -i "s/val outerPar = [0-9]\+/val outerPar = $op/g" $file
	      	sed -i "s/val tileSize = [0-9]\+/val tileSize = $d0/g" $file

	      	# Check for errors
			fastmake="cp -r ${SPATIAL_HOME}/extern/compiler/src/ops/* ${PUB_HOME}/compiler/src/spatial/compiler/ops;cd ${PUB_HOME}/;sbt compile 2>&1"
			eval "$fastmake" > ${PUB_HOME}/log
			wc=$(cat ${PUB_HOME}/log | grep "success" | wc -l)
			if [ "$wc" -ne 1 ]; then
				echo "Remake fucked up fastmake $ip $op $d0 $d1!"
				exit 1
			fi

			# Compile and find errors
			cmd="bin/spatial --outdir=${staging_area}/${i} ${testname} 2>&1 > ${staging_area}/log"
			eval "$cmd"
			sed -i "s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g" ${staging_area}/log

			wc=$(cat ${staging_area}/log | grep "couldn't find DEG file" | wc -l)
			if [ "$wc" -ne 0 ]; then
				echo "Fucked up something"
				exit 1
			fi
			wc=$(cat ${staging_area}/log | grep "error" | wc -l)
			if [ "$wc" -ne 0 ]; then
				echo "Fucked up something"
				exit 1
			fi

			# Copy results file to out, only last one will have all data
	      	echo "$ip,$op,$d0,${server}${i}," >> $fname
			cp $fname ${staging_area}/${i}/

			# Copy to maxeler
			cmd="ssh -p 3033 mfeldman@portal.maxeler.com rm -rf /home/mfeldman/maxj${i};rsync -avz -e \"ssh -p 3033\" --exclude scala/ ${staging_area}/${i}/* mfeldman@portal.maxeler.com:/home/mfeldman/maxj${i}"
			eval "$cmd" 2>&1 > /dev/null
	      	echo "Finished $ip $op $d0 on channel $i..."

		else
			echo "Skipping $ip $op $d0 on channel $i..."
		fi
		
		i=$(($i+1))

    done
  done
done

last=$(($i-1))
echo "Copied on channels ${start_channel} to $last on Maxeler!"
if [ "$server" = "london" ]; then
	echo "Remember to mvrange!"
fi
