#!/usr/bin/env perl

while (<>) {
  chomp;
  @_ = split /,/;
  $_{"$_[0],$_[1]"} ||= $_[2] if
    !exists($_{"$_[0],$_[1]"}) or $_[2] < $_{"$_[0],$_[1]"};
}
print "$_,$_{$_}\n" for sort keys %_;
