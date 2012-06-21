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

our $julia_ver = `julia -e 'print(VERSION_COMMIT[1:8])'`;

our %systems = (
  "julia"      => ["Julia"       , $julia_ver   ],
  "python"     => ["Python"      , "2.7.1"      ],
  "matlab"     => ["Matlab"      , "R2011a"     ],
  "octave"     => ["Octave"      , "3.4"        ],
  "r"          => ["R"           , "2.14.2"     ],
  "javascript" => ["JavaScript"  , "V8 3.6.6.11"],
  "fortran"    => ["Fortran"     , ""],
);

our @systems = qw(julia python matlab octave r javascript fortran);

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
