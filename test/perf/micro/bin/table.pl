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


our $julia_ver       = `../../../julia -e 'print(Base.VERSION)'`;
our $python_ver      = `python -c "import sys; print sys.version.split(' ')[0]"`;
our $rbase_ver       = ( split /\s/,`R --version`)[2];
our $js_ver          = `node -e "'v8 '+console.log(process.versions.v8);"`;
our $fortran_ver     = ( split /\s/,`gfortran --version`)[4];
our $octave_ver      = ( split /\s/,`octave --version`)[3];
our $go_ver          = `go version`;
our $java_ver        = "7";## Java not writing version to std out
our $matlab_ver      = "unknown";
our $mathematica_ver = "unknown";



our %systems = (
  "fortran"    => ["Fortran"     , $fortran_ver     ],
  "julia"      => ["Julia"       , $julia_ver       ],
  "java"       => ["Java"        , $java_ver        ],
  "javascript" => ["JavaScript"  , $js_ver          ],
  "python"     => ["Python"      , $python_ver      ],
  "octave"     => ["Octave"      , $octave_ver      ],
  "r"          => ["R"           , $rbase_ver       ],
  "go"         => ["Go"          , $go_ver          ],
  "matlab"     => ["Matlab"      , $matlab_ver      ],
  "mathematica"=> ["Mathematica" , $mathematica_ver ],
);


 ##go matlab mathematica
our @systems = qw(fortran julia java javascript python octave r);

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
