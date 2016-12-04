#!/bin/bash

##########
# CONFIG #
##########

# **Fixed to use a staging area

testname="Kmeans"
server="max" # max or londonvm
start_channel=7
file=${SPATIAL_HOME}/apps/src/Kmeans.scala
staging_area=${HOME}/staging/${testname}

innerPar=(8) # 32 on max2
outerPar=(1)
dim=(96)
tileSize=(384)
num_cents=(20)
startAt=1 # Indicates channel to start at, in case something breaks in the middle

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
echo "innerPar,midPar,outerPar,p3,p4,p5,words,channel,ALMs,Register & Logic,Logic Only,Register Only,Mem,Logic Subtotal,Register Subtotal,Placement Total,Recoverable,Unavailable,\"Needed\",LUTs,7 Inputs,6 Inputs,5 Inputs,4 Inputs,<= 3 Inputs,Logic Subtotal,Route-Thru,64 Address Mems,32-Address Mems,16-Address Mems,Mem Subtotal,Total,Registers,Implementation,Routing,Total,DSPs,BRAM,Effort,ALM Regs,% ALM Regs Used,Logic Packed %,Total Logic Pack Ratio (<=6),Mem Packed %,Total Mem Pack Ratio,% ALMs with Reg + Logic,Est. Impl Reg-Only ALMs,Recoverable ALMs,Est. Logic ALM Packing,Est. Mem ALM Packing,Unaccounted,% Unaccounted,Guess,Total,Diff,channel,iters,words,time,cycles" >> $fname

for p1 in ${innerPar[@]}; do
  	for p2 in ${outerPar[@]}; do
      for p3 in ${tileSize[@]}; do
        for p4 in ${dim[@]}; do
          for p5 in ${num_cents[@]}; do

    	    if [ $i -ge $startAt ]; then

		      	echo "Running $p1 $p2 $p3 $p4 $p5 on channel $i..."

		      	# Edit files
		      	sed -i "s/val innerPar = [0-9]\+/val innerPar = $p1/g" $file
		      	sed -i "s/val outerPar = [0-9]\+/val outerPar = $p2/g" $file
		      	sed -i "s/val tileSize = [0-9]\+/val tileSize = $p3/g" $file
		      	sed -i "s/val dim = [0-9]\+/val dim = $p4/g" $file
		      	sed -i "s/val num_cents = [0-9]\+/val num_cents = $p5/g" $file

		      	# Check for errors
				fastmake="cp -r ${SPATIAL_HOME}/extern/compiler/src/ops/* ${PUB_HOME}/compiler/src/spatial/compiler/ops;cd ${PUB_HOME}/;sbt compile 2>&1"
				eval "$fastmake" > ${PUB_HOME}/log
				wc=$(cat ${PUB_HOME}/log | grep "success" | wc -l)
				if [ "$wc" -ne 1 ]; then
					echo "Remake fucked up fastmake $p1 $p2 $p3 $p4 $p5!"
					exit 1
				fi

				# Compile and find errors
				cmd="bin/spatial --outdir=${staging_area}/${i} ${testname} 2>&1 > ${staging_area}/log"
				eval "$cmd"
				sed -i "s/^ERROR.*ignored\./Ignoring silly LD_PRELOAD  e r r o r/g" ${staging_area}/log

				wc=$(cat ${staging_area}/log | grep "couldn't find DEG file" | wc -l)
				if [ "$wc" -ne 0 ]; then
					echo "Fucked up something 1"
					exit 1
				fi
				# look for MaxJExecutableGenerator because that should mean it worked
				wc=$(cat ${staging_area}/log | grep "MaxJExecutableGenerator" | wc -l)
				if [ "$wc" -eq 0 ]; then
					echo "Fucked up something 2"
					exit 1
				fi

				# Copy results file to out, only last one will have all data
		      	echo "$p1,$p2,$p3,$p4,$p5,${server}${i}," >> $fname
				cp $fname ${staging_area}/${i}/

				# Copy to server
				if [ "$server" = "max" ]; then
					cmd="ssh -p 3033 mfeldman@portal.maxeler.com rm -rf /home/mfeldman/maxj${i};rsync -avz -e \"ssh -p 3033\" --exclude scala/ ${staging_area}/${i}/* mfeldman@portal.maxeler.com:/home/mfeldman/maxj${i}"
				else
					cmd="ssh mattfel@192.168.122.59 rm -rf /home/mattfel/maxj${i};rsync -avz --exclude scala/ ${staging_area}/${i}/* mattfel@192.168.122.59:/home/mattfel/maxj${i}"
				fi
				eval "$cmd" 2>&1 > /dev/null
		      	echo "Finished $p1 $p2 $p3 $p4 $p5 on channel $i..."

			else
				echo "Skipping $p1 $p2 $p3 $p4 $p5 on channel $i..."
			fi
		
		    i=$(($i+1))

		  done
		done
    done
  done
done

last=$(($i-1))
echo "Copied on channels ${start_channel} to $last on $server!"
if [ "$server" = "londonvm" ]; then
	echo "Remember to mvrange!"
fi
