# load "ref1146-7012-37-5.gnu"
# chem = "2,4,4'-trichlorobiphenyl"

set terminal postscript eps color
set title "ref = 1146; chem = 2,4,4'-trichlorobiphenyl; casrn = 7012-37-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3623763E-01 * exp(  -6051.886    *(1/   298.    -1/T))

set label "" at    283.5500    ,   0.1149425     point
set label "" at    293.1500    ,   0.4716981E-01 point
set label "" at    303.2500    ,   0.2109705E-01 point
set label "" at    308.0500    ,   0.1988072E-01 point
set label "" at    315.2500    ,   0.1412429E-01 point
set label "" at    321.0500    ,   0.8291874E-02 point
set label "" at    321.5500    ,   0.8183306E-02 point
set label "" at    298.1500    ,   0.3623763E-01 point ps 2 pt 6

plot [280:330] H(T)
