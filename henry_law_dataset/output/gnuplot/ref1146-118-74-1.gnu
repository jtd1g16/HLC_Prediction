# load "ref1146-118-74-1.gnu"
# chem = "hexachlorobenzene"

set terminal postscript eps color
set title "ref = 1146; chem = hexachlorobenzene; casrn = 118-74-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2044001E-01 * exp(  -5739.246    *(1/   298.    -1/T))

set label "" at    287.9500    ,   0.4237288E-01 point
set label "" at    293.2500    ,   0.3333333E-01 point
set label "" at    295.2500    ,   0.2145923E-01 point
set label "" at    297.3500    ,   0.1904762E-01 point
set label "" at    307.9500    ,   0.1132503E-01 point
set label "" at    323.6500    ,   0.4604052E-02 point
set label "" at    298.1500    ,   0.2044001E-01 point ps 2 pt 6

plot [280:330] H(T)
