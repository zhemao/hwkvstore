for dir in $LOCAL/$ROOT/outputs/*/
  do
    dir=${dir%*/}
    if [[ -e ${dir}/$CONFIG* ]]; then
      echo "rsync -au ${dir}/$CONFIG* ${SHARED%:*}:${dir}"
      rsync -au ${dir}/$CONFIG* ${SHARED%:*}:${dir}
    fi
  done
for dir in $LOCAL/$ROOT/results/*/
  do
    dir=${dir%*/}
    if [[ -e ${dir}/$CONFIG* ]]; then
      echo "rsync -au ${dir}/$CONFIG* ${SHARED%:*}:${dir}"
      rsync -au ${dir}/$CONFIG* ${SHARED%:*}:${dir}
    fi
  done
