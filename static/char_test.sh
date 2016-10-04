#!/bin/bash

##########
# CONFIG #
##########

testname="CharBramTest"
server="max"
start_channel=1
file=${SPATIAL_HOME}/apps/src/CharacterizationUnitTests.scala

innerPar=(1 8)
outerPar=(1 4)
dim0=(192)
dim1=(1920 19200)

i=$start_channel

# Create results file
fname="/tmp/Summary_${testname}_Manual.csv"
cmd="mv ${fname} ${fname}.displaced"
eval "${cmd}"
cmd="touch ${fname}"
eval "${cmd}"
echo "${t},,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,=4*M1,=AH1/AL1,=X1/(2*L1),=SUM(T1:W1)/(2*(L1-S1)),=(AA1+AB1)/(2*(K1-Z1)),=(Z1/K1) + ((AA1+AB1)/K1)*AP1,=H1/N1,=(J1*4-2*Y1-4*AG1)/4,,\"=CEILING((1-AO1)*SUM(T1:W1)/2,1)\",\"=CEILING((1-AQ1)*K1/2,1)\",=O1-AV1+AU1,=O1/AW1,\"=CEILING((0.615-AM1)*AF1/2,1)\",=AY1+AV1+AU1,=(O1-AZ1)/O1,,,,=C1*D1*BC1,=BC1*150000" >> $fname
echo "innerPar,outerPar,rows,cols,words,channel,ALMs,Register & Logic,Logic Only,Register Only,Mem,Logic Subtotal,Register Subtotal,Placement Total,Recoverable,Unavailable,\"Needed\",LUTs,7 Inputs,6 Inputs,5 Inputs,4 Inputs,<= 3 Inputs,Logic Subtotal,Route-Thru,64 Address Mems,32-Address Mems,16-Address Mems,Mem Subtotal,Total,Registers,Implementation,Routing,Total,DSPs,BRAM,Effort,ALM Regs,% ALM Regs Used,Logic Packed %,Total Logic Pack Ratio (<=6),Mem Packed %,Total Mem Pack Ratio,% ALMs with Reg + Logic,Est. Impl Reg-Only ALMs,Recoverable ALMs,Est. Logic ALM Packing,Est. Mem ALM Packing,Unaccounted,% Unaccounted,Guess,Total,Diff,channel,iters,words,time,cycles" >> $fname

for ip in ${innerPar[@]}; do
  for op in ${outerPar[@]}; do
    for d0 in ${dim0[@]}; do
      for d1 in ${dim1[@]}; do
      	echo "Running $ip $op $d0 $d1 on channel $i..."

      	# Edit files
      	sed -i "s/val innerPar = [0-9]\+/val innerPar = $ip/g" $file
      	sed -i "s/val outerPar = [0-9]\+/val outerPar = $op/g" $file
      	sed -i "s/val dim0 = [0-9]\+/val dim0 = $d0/g" $file
      	sed -i "s/val dim1 = [0-9]\+/val dim1 = $d1/g" $file

      	# Check for errors
		fastmake="cp -r ${SPATIAL_HOME}/extern/compiler/src/ops/* ${PUB_HOME}/compiler/src/spatial/compiler/ops;cd ${PUB_HOME}/;sbt compile 2>&1"
		eval "$fastmake" > ${PUB_HOME}/log
		wc=$(cat ${PUB_HOME}/log | grep "success" | wc -l)
		if [ "$wc" -ne 1 ]; then
			echo "Remake fucked up fastmake $ip $op $d0 $d1!"
			exit 1
		fi

		# Compile and find errors
		cmd="bin/spatial ${testname} 2>&1 > log"
		eval "$cmd"
		sed -i "s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g" log

		wc=$(cat log | grep "couldn't find DEG file" | wc -l)
		if [ "$wc" -ne 0 ]; then
			echo "Fucked up something"
			exit 1
		fi
		wc=$(cat log | grep "error" | wc -l)
		if [ "$wc" -ne 0 ]; then
			echo "Fucked up something"
			exit 1
		fi

		# Copy results file to out, only last one will have all data
      	echo "$ip,$op,$d0,$d1,=$d0*$d1,${server}${i}," >> $fname
		cp $fname ./out/

		# Copy to maxeler
		cmd="cp${server}${i}"
		eval "$cmd" 2>&1 > /dev/null
      	echo "Finished $ip $op $d0 $d1 on channel $i..."

		i=$(($i+1))

      done
    done
  done
done

last=$(($i-1))
echo "Copied on channels ${start_channel} to $last on Maxeler!"
if [ "$server" = "london" ]; then
	echo "Remember to mvrange!"
fi
