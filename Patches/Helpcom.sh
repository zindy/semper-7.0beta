#!/bin/sh
#Script to force correct links for help
ls *.comm > tt
sed 's/.comm//' tt > t1
nn=`wc -l tt | sed 's/tt//' `
n=1
echo Number of commands $nn
rm t2 t3
while [ "$n" -le "$nn" ]
        do
                echo ln -s -f >> t2
        n=`expr $n + 1`
done
paste t2 tt t1 > t3
echo Running Phase 1
sh t3
rm -f t1 tt t2 t3 &> /dev/null

#Make some additional
 echo Listing all current verbs
 rm verbs
 ls -C *.comm | expand | sed 's/.comm//g' >> verbs

#Ensure that 3 letter commands are linked
echo Other links
ls -1 *.comm > tt
sed 's/[a-Z]/ln -s &/' tt > t1
cut -c 1-3 tt > t2
paste t1 t2 > Commands.sh
#rm -f t1 t2 tt &> /dev/null
echo Running Others
sh Commands.sh
echo Done

echo " " >> help
echo "See also Help Commands, Help Verbs for information" >> help

