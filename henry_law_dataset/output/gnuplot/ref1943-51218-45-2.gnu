# load "ref1943-51218-45-2.gnu"
# chem = "metolachlor"

set terminal postscript eps color
set title "ref = 1943; chem = metolachlor; casrn = 51218-45-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    571.9697     * exp(  -14677.33    *(1/   298.    -1/T))

set label "" at    293.2000    ,    1096.581     point
set label "" at    298.2000    ,    710.0167     point
set label "" at    303.2000    ,    259.7166     point
set label "" at    313.2000    ,    49.84461     point
set label "" at    298.1500    ,    571.9697     point ps 2 pt 6

plot [290:320] H(T)
