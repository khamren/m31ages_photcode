gs -dNOPAUSE -q -r300 -dBATCH -sDEVICE=ppm -sOutputFile=$argv[1].ppm $argv[1]
ppmtogif $argv[1].ppm > $argv[1].gif
\rm $argv[1].ppm
