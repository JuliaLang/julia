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

our %systems = (
  "c"          => ["C++ (GCC)"   , "4.2.1*"     ],
  "julia"      => ["Julia"       , "54fc2f70"   ],
  "python"     => ["Python/NumPy", "2.7.1/1.5.1"],
  "matlab"     => ["Matlab"      , "R2011a"     ],
  "octave"     => ["Octave"      , "3.4"        ],
  "r"          => ["R"           , "2.9.0"      ],
  "javascript" => ["JavaScript"  , "V8 3.6.6.11"],
);

our @systems = qw(c julia python matlab octave r javascript);
our @compare = @systems[1..$#systems];

printf "%s\n", ("_" x (34+14*@compare));
print "                 |             |\n";
printf "                 |  %9s  |", $systems{"c"}[0];
for my $system (@compare) {
  printf "%14s", $systems{$system}[0];
}
print "\n";
printf "                 |  %9s  |", $systems{"c"}[1];
for my $system (@compare) {
  printf "%14s", $systems{$system}[1];
}
print "\n";
print "_________________|_____________|";
printf "%s\n", ("_" x (2+14*@compare));
print "                 |             |\n";

for my $benchmark (@benchmarks) {
  printf "  %-14s |   %7.3f   |", $benchmark, $_{$benchmark}{'c'};
  for my $system (@compare) {
    printf "%14.2f", $_{$benchmark}{$system}/$_{$benchmark}{'c'};
  }
  print "\n";
}

print "_________________|_____________|";
printf "%s\n", ("_" x (2+14*@compare));
