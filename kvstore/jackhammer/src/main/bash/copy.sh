for dir in $LOCAL/$ROOT/results/*/
  do
    dir=${dir%*/}
    echo "rsync -au $LOCAL/$ROOT/results/${dir}/$CONFIG* $SHARED/$ROOT/results/${dir}"
    rsync -au $LOCAL/$ROOT/results/${dir}/$CONFIG* $SHARED/$ROOT/results/${dir}
    echo "rsync -au $LOCAL/$ROOT/outputs/${dir}/$CONFIG* $SHARED/$ROOT/outputs/${dir}"
    rsync -au $LOCAL/$ROOT/outputs/${dir}/$CONFIG* $SHARED/$ROOT/outputs/${dir}
  done
