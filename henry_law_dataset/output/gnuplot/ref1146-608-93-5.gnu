# load "ref1146-608-93-5.gnu"
# chem = "pentachlorobenzene"

set terminal postscript eps color
set title "ref = 1146; chem = pentachlorobenzene; casrn = 608-93-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1406246E-01 * exp(  -5185.887    *(1/   298.    -1/T))

set label "" at    287.9500    ,   0.2673797E-01 point
set label "" at    293.2500    ,   0.2024291E-01 point
set label "" at    295.2500    ,   0.1468429E-01 point
set label "" at    297.3500    ,   0.1499250E-01 point
set label "" at    307.9500    ,   0.8058018E-02 point
set label "" at    323.6500    ,   0.3620565E-02 point
set label "" at    298.1500    ,   0.1406246E-01 point ps 2 pt 6

plot [280:330] H(T)
