#!/usr/bin/env perl

use strict;
use warnings;

our $mode = pop;

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
  "python"     => ["NumPy"       , "1.5.1"      ],
  "matlab"     => ["Matlab"      , "R2011a"     ],
  "octave"     => ["Octave"      , "3.4"        ],
  "r"          => ["R"           , "2.9.0"      ],
  "javascript" => ["JavaScript"  , "V8 3.6.6.11"],
);

our @systems = qw(c julia python matlab octave r javascript);
our @compare = @systems[1..$#systems];

if ($mode eq "txt") {
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
} elsif ($mode eq "html") {
  print qq[<table class="benchmarks">\n];
  print qq[<colgroup>\n];
  print qq[<col class="name"></col>\n];
  print qq[<col class="reference"></col>\n];
  printf qq[<col class="relative" span="%d"></col>\n], scalar(@compare);
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
    printf qq[<td class="data" class="reference">%.3f</td>], $_{$benchmark}{"c"};
    for my $system (@compare) {
      printf qq[<td class="data">%.2f</td>], $_{$benchmark}{$system}/$_{$benchmark}{'c'};
    }
    print qq[</tr>\n];
  }
  print qq[</tbody>\n];
  print qq[</table>\n];
} else {
  die "unknown output mode: $mode";
}
