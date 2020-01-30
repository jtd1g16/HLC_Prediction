# load "ref1146-205-99-2.gnu"
# chem = "benzo[b]fluoranthene"

set terminal postscript eps color
set title "ref = 1146; chem = benzo[b]fluoranthene; casrn = 205-99-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    15.03613     * exp(  -5449.828    *(1/   298.    -1/T))

set label "" at    283.1500    ,    40.00000     point
set label "" at    293.1500    ,    19.60784     point
set label "" at    308.1500    ,    8.403361     point
set label "" at    313.2500    ,    6.622517     point
set label "" at    318.1500    ,    4.807692     point
set label "" at    328.1500    ,    2.702703     point
set label "" at    298.1500    ,    15.03613     point ps 2 pt 6

plot [280:330] H(T)
