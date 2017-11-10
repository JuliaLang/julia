#!/usr/bin/env perl

use strict;
use warnings;

while (<>) {
  chomp;
  @_ = split /,/;
  $_{$_[1]}{$_[0]} = $_[2];
}

our @benchmarks = qw(
  iteration_pi_sum
  recursion_fibonacci
  recursion_quicksort
  parse_integers
  print_to_file
  matrix_statistics
  matrix_multiply
  userfunc_mandelbrot
);

our $c_ver = chomp(`gcc -v 2>&1 | grep "gcc version" | cut -f3 -d" "`);
our $julia_ver = chomp(`../../../julia -v | cut -f3 -d" "`);
our $fortran_ver = chomp(`gfortran -v 2>&1 | grep "gcc version" | cut -f3 -d" "`);
our $python_ver = chomp(`python3 -V 2>&1 | cut -f2 -d" "`);
our $matlab_ver = chomp(`matlab -nodisplay -nojvm -nosplash -r "version -release, quit" | tail -n 3 | head -n 1`);
our $R_ver = chomp(`R --version | grep "R version" | cut -f3 -d" "`);
our $octave_ver = chomp(`octave -v | grep version | cut -f4 -d" "`);
our $go_ver = chomp(`go version | cut -f3 -d" "`);
#our $lua_ver = chomp(`scilua -v 2>&1 | grep Shell | cut -f3 -d" " | cut -f1 -d,`);
our $lua_ver = "scilua v1.0.0-b12"; # scilua has no run-time versioninfo function
our $javascript_ver = chomp(`nodejs -e "console.log(process.versions.v8)"`);
our $mathematica_ver = chomp(`echo quit | math -version | head -n 1 | cut -f2 -d" "`);
#our $stata_ver = chomp(`stata -q -b version && grep version stata.log | cut -f2 -d" " && rm stata.log`);
our $java_ver = chomp(`java -version 2>&1 |grep "version" | cut -f3 -d " " | cut -c 2-9`);

our %systems = (
  "c"          => ["C"                , "gcc $c_ver" ],
  "julia"      => ["Julia"            , $julia_ver  ],
  "lua"        => ["LuaJIT"           , "$lua_ver" ],
  "fortran"    => ["Fortran"          , "gcc $fortran_ver" ],
  "java"       => ["Java"             , $java_ver ],
  "javascript" => ["JavaScript"       , "V8 $javascript_ver" ],
  "matlab"     => ["Matlab"           , "R$matlab_ver" ],
  "python"     => ["Python"           , $python_ver ],
  "mathematica"=> ["Mathe&shy;matica" , $mathematica_ver ],
  "r"          => ["R"                , $R_ver ],
  "octave"     => ["Octave"           , $octave_ver ],
  "go"         => ["Go"               , $go_ver ],
#  "stata"      => ["Stata"            , $stata_ver ],
);

our @systems = qw(c julia lua fortran go java javascript mathematica python matlab r octave);

print qq[<table class="benchmarks">\n];
print qq[\t<colgroup>\n];
print qq[\t\t<col class="name">\n];
printf qq[\t\t<col class="relative" span="%d">\n], scalar(@systems);
print qq[\t</colgroup>\n];
print qq[\t<thead>\n];
print qq[\t\t<tr>\n];
print qq[\t\t\t<th></th>\n];
print qq[\t\t\t<th class="system">$systems{$_}[0]</th>\n] for @systems;
print qq[\t\t</tr>\n];
print qq[\t\t<tr>\n];
print qq[\t\t\t<td></td>\n];
print qq[\t\t\t<td class="version">$systems{$_}[1]</td>\n] for @systems;
print qq[\t\t</tr>\n];
print qq[\t</thead>\n];
print qq[\t<tbody>\n];

for my $benchmark (@benchmarks) {
  print qq[\t\t<tr>\n];
  print qq[\t\t\t<th>$benchmark</th>\n];
  for my $system (@systems) {
    printf qq[\t\t\t<td class="data">%.2f</td>\n], $_{$benchmark}{$system}/$_{$benchmark}{'c'};
  }
  print qq[\t\t</tr>\n];
}
print qq[\t</tbody>\n];
print qq[</table>\n];
