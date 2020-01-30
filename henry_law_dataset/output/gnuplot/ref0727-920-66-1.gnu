# load "ref0727-920-66-1.gnu"
# chem = "1,1,1,3,3,3-hexafluoro-2-propanol"

set terminal postscript eps color
set title "ref = 727; chem = 1,1,1,3,3,3-hexafluoro-2-propanol; casrn = 920-66-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2359977     * exp(  -6678.784    *(1/   298.    -1/T))

set label "" at    288.0000    ,   0.5328892     point
set label "" at    298.0000    ,   0.2321361     point
set label "" at    308.0000    ,   0.1126824     point
set label "" at    318.0000    ,   0.5980872E-01 point
set label "" at    298.1500    ,   0.2359977     point ps 2 pt 6

plot [280:320] H(T)
