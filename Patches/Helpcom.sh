#!/bin/sh
#Script to force correct links for help
ls *.comm > tt
sed 's/.comm//' tt > t1
nn=`wc -l tt | sed 's/tt//' `
n=1
echo $nn
rm t2 t3
while [ "$n" -le "$nn" ]
        do
                echo ln -s -f >> t2
        n=`expr $n + 1`
done
paste t2 tt t1 > t3
sh t3
rm t1 tt t2 t3

#Make some additional
 echo List of all current verbs -- see also help commands
 ls -C *.comm | expand | sed 's/.comm//g' >> verbs

#Ensure that 3 letter commands are linked
ls -1 *.comm > tt
sed 's/[A-z]/ln -s &/' tt > t1
cut -c 1-3 tt > t2
paste t1 t2 > Commands.sh
rm t1 t2 tt
sh Commands.sh

echo " " >> help
echo "See also Help Commands, Help Verbs for information" >> help

