# load "ref1146-193-39-5.gnu"
# chem = "indeno[1,2,3-cd]pyrene"

set terminal postscript eps color
set title "ref = 1146; chem = indeno[1,2,3-cd]pyrene; casrn = 193-39-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    28.26635     * exp(  -3639.172    *(1/   298.    -1/T))

set label "" at    283.1500    ,    55.55556     point
set label "" at    293.1500    ,    34.48276     point
set label "" at    308.1500    ,    17.54386     point
set label "" at    313.2500    ,    16.39344     point
set label "" at    318.1500    ,    12.98701     point
set label "" at    328.1500    ,    9.523810     point
set label "" at    298.1500    ,    28.26635     point ps 2 pt 6

plot [280:330] H(T)
