#!/usr/bin/env perl

use strict;
use warnings;

while (<>) {
  chomp;
  @_ = split /,/;
  $_{$_[1]}{$_[0]} = $_[2];
}

our @benchmarks = qw(
  fib
  parse_int
  quicksort
  mandel
  pi_sum
  rand_mat_stat
  rand_mat_mul
);

our $julia_ver = `julia -e 'print(Base.BUILD_INFO.version_string)'`;
our $fortran_ver = `gfortran-4.8 -v 2>&1 | grep "gcc version" | cut -f3 -d" "`;
our $python_ver = `python -V 2>&1 | cut -f2 -d" "`;
our $matlab_ver = `matlab -nodisplay -nojvm -nosplash -r "version -release, quit" | tail -n 3 | head -n 1`;
our $R_ver = `R --version | grep "R version" | cut -f3 -d" "`;
our $octave_ver = `octave -v | grep version | cut -f4 -d" "`;
our $go_ver = `go version | cut -f3 -d" "`;
our $javascript_ver = `node -e "console.log(process.versions.v8)"`;
our $mathematica_ver = `echo quit | math -version | head -n 1 | cut -f2 -d" "`;

our %systems = (
  "fortran"    => ["Fortran" , "GCC $fortran_ver" ],
  "julia"      => ["Julia"       , $julia_ver  ],
  "python"     => ["Python"      , $python_ver ],
  "matlab"     => ["Matlab"      , "R$matlab_ver" ],
  "octave"     => ["Octave"      , $octave_ver ],
  "r"          => ["R"           , $R_ver ],
  "javascript" => ["JavaScript", "V8 $javascript_ver"],
  "go"         => ["Go"          , $go_ver ],
  "mathematica"=> ["Mathematica" , $mathematica_ver ],
);

our @systems = qw(fortran julia python r matlab octave mathematica javascript go);

print qq[<table class="benchmarks">\n];
print qq[<colgroup>\n];
print qq[<col class="name"></col>\n];
printf qq[<col class="relative" span="%d"></col>\n], scalar(@systems);
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
