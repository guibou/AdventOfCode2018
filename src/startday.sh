echo $1

cp DayX.hs Day$1.hs
sed -i "s/DayX/Day$1/" Day$1.hs
