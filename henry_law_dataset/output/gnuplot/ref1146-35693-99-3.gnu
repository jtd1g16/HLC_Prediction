# load "ref1146-35693-99-3.gnu"
# chem = "2,2',5,5'-tetrachlorobiphenyl"

set terminal postscript eps color
set title "ref = 1146; chem = 2,2',5,5'-tetrachlorobiphenyl; casrn = 35693-99-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4175322E-01 * exp(  -6192.566    *(1/   298.    -1/T))

set label "" at    283.5500    ,   0.1162791     point
set label "" at    293.1500    ,   0.6097561E-01 point
set label "" at    303.2500    ,   0.2673797E-01 point
set label "" at    308.0500    ,   0.2577320E-01 point
set label "" at    315.2500    ,   0.1455604E-01 point
set label "" at    321.0500    ,   0.9157509E-02 point
set label "" at    321.5500    ,   0.8291874E-02 point
set label "" at    298.1500    ,   0.4175322E-01 point ps 2 pt 6

plot [280:330] H(T)
