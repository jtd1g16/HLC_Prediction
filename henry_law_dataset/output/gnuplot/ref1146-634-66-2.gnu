# load "ref1146-634-66-2.gnu"
# chem = "1,2,3,4-tetrachlorobenzene"

set terminal postscript eps color
set title "ref = 1146; chem = 1,2,3,4-tetrachlorobenzene; casrn = 634-66-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1300476E-01 * exp(  -4752.971    *(1/   298.    -1/T))

set label "" at    287.9500    ,   0.2061856E-01 point
set label "" at    293.2500    ,   0.1923077E-01 point
set label "" at    295.2500    ,   0.1468429E-01 point
set label "" at    297.3500    ,   0.1410437E-01 point
set label "" at    307.9500    ,   0.7818608E-02 point
set label "" at    323.6500    ,   0.3620565E-02 point
set label "" at    298.1500    ,   0.1300476E-01 point ps 2 pt 6

plot [280:330] H(T)
