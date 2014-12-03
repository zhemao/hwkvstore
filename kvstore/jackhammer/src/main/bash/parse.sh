#rm results-cmr.txt
rm results-vcs.txt
rm results-dcs.txt
#dir="hammer-rocket-testcache"
dir="hammer-rocket"

#ls /nscratch/adamiz/$dir/results | while read line;
#do
    #cat "/nscratch/adamiz/$dir/results/$line/cmr_res.OU" >> results-cmr.txt
#done
ls /nscratch/adamiz/$dir/results | while read line;
do
    cat "/nscratch/adamiz/$dir/results/$line/vcs_res.OU" >> results-vcs.txt
done
ls /nscratch/adamiz/$dir/results | while read line;
do
    cat "/nscratch/adamiz/$dir/results/$line/dcs_res.OU" >> results-dcs.txt
done
