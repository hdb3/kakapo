for k in 5 5p 10
do t="ANCS-800k-${k}" 
   z /var/webdav TOPIC=${t} PLATFORM=bird,frr,bgpd,hbgp RIBSIZE=? --min --pdf --png --exec --title="${t}-selected"
   mv plot.csv "${t}-selected.csv"
   z /var/webdav TOPIC=${t} PLATFORM=, RIBSIZE=? --min --pdf --png --exec --title="${t}-all"
   mv plot.csv "${t}-all.csv"
   z /var/webdav TOPIC=${t} PLATFORM=bird,frr,bgpd,hbgp RIBSIZE=? --pdf --png --exec --eb --title="${t}-selected-errorbars"
   mv plot.csv "${t}-selected-errorbars.csv"
   z /var/webdav TOPIC=${t} PLATFORM=, RIBSIZE=? --pdf --png --exec --eb --title="${t}-all-errorbars"
   mv plot.csv "${t}-all-errorbars.csv"
done
