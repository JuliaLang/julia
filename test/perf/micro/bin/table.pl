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

our $c_ver = `gcc -v 2>&1 | grep "gcc version" | cut -f3 -d" "`;
our $julia_ver = `../../../julia -v | cut -f3 -d" "`;
our $fortran_ver = `gfortran -v 2>&1 | grep "gcc version" | cut -f3 -d" "`;
our $python_ver = `python3 -V 2>&1 | cut -f2 -d" "`;
our $matlab_ver = `matlab -nodisplay -nojvm -nosplash -r "version -release, quit" | tail -n 3 | head -n 1`;
our $R_ver = `R --version | grep "R version" | cut -f3 -d" "`;
our $octave_ver = `octave -v | grep version | cut -f4 -d" "`;
our $go_ver = `go version | cut -f3 -d" "`;
#our $lua_ver = `scilua -v 2>&1 | grep Shell | cut -f3 -d" " | cut -f1 -d,`;
our $lua_ver = "scilua v1.0.0-b12"; # scilua has no run-time versioninfo function
our $javascript_ver = `nodejs -e "console.log(process.versions.v8)"`;
our $mathematica_ver = `echo quit | math -version | head -n 1 | cut -f2 -d" "`;
#our $stata_ver = `stata -q -b version && grep version stata.log | cut -f2 -d" " && rm stata.log`;
our $java_ver = `java -version 2>&1 |grep "version" | cut -f 3 -d " " | cut -c 2-9`;

our %systems = (
  "c"          => ["C"           , "gcc $c_ver" ],    
  "julia"      => ["Julia"       , $julia_ver  ],
  "lua"        => ["LuaJIT"      , "$lua_ver" ],
  "fortran"    => ["Fortran"     , "gcc $fortran_ver" ],
  "java"       => ["Java"        , $java_ver ],
  "javascript" => ["JavaScript"  , "V8 $javascript_ver" ],
  "matlab"     => ["Matlab"      , "R$matlab_ver" ],
  "python"     => ["Python"      , $python_ver ],
  "mathematica"=> ["Mathe-matica" , $mathematica_ver ],
  "r"          => ["R"           , $R_ver ],
  "octave"     => ["Octave"      , $octave_ver ],
  "go"         => ["Go"          , $go_ver ],
#  "stata"      => ["Stata"       , $stata_ver ],
);

our @systems = qw(c julia lua fortran go java javascript mathematica python matlab r octave);
#our @systems = qw(lua);

print qq[<table class="benchmarks">\n];
print qq[<colgroup>\n];
print qq[<col class="name">\n];
printf qq[<col class="relative" span="%d">\n], scalar(@systems);
print qq[</colgroup>\n];
print qq[<thead>\n];
print qq[<tr>];
print qq[<td></td>];
print qq[<th class="system">$systems{$_}[0]</th>] for @systems;
print qq[</tr>\n];
print qq[<tr>];
print qq[<td></td>];
print qq[<td class="version">$systems{$_}[1]</td>] for @systems;
print qq[</tr>\n];
print qq[</thead>\n];
print qq[<tbody>\n];

for my $benchmark (@benchmarks) {
  print qq[<tr>];
  print qq[<th>$benchmark</th>];
  for my $system (@systems) {
    printf qq[<td class="data">%.2f</td>], $_{$benchmark}{$system}/$_{$benchmark}{'c'};
  }
  print qq[</tr>\n];
}
print qq[</tbody>\n];
print qq[</table>\n];
