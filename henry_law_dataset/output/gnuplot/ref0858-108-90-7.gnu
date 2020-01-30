# load "ref0858-108-90-7.gnu"
# chem = "chlorobenzene"

set terminal postscript eps color
set title "ref = 858; chem = chlorobenzene; casrn = 108-90-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3110102E-02 * exp(  -2936.079    *(1/   298.    -1/T))

set label "" at    318.1600    ,   0.1610386E-02 point
set label "" at    333.1600    ,   0.1158825E-02 point
set label "" at    343.1600    ,   0.8972411E-03 point
set label "" at    353.1600    ,   0.6334426E-03 point
set label "" at    298.1500    ,   0.3110102E-02 point ps 2 pt 6

plot [310:360] H(T)
