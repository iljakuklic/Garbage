#!/bin/bash

# Format time compactly to fit in 6 chars.
# $1 is the time in nanoseconds
function fmt_compact_time {
  local maxw r u q ns us ms s m h d ul l
  (( maxw = 6 ))
  case "$1" in
    -w*) (( maxw = "${1#-w}" )); shift ;;
  esac
  (( ns = "$1" ))
  local r=''
  local u=''
  if (( q = 1, ns < 1000 )); then
    printf -vr "%d" $(( ns ))
    u='ns'
  elif (( q *= 1000, ( us = ns / q ) < 1000 )); then
    printf -vr "%d.%03d" $(( us )) $(( ns % q ))
    u='μs'
  elif (( q *= 1000, ( ms = ns / q ) < 1000 )); then
    printf -vr "%d.%06d" $(( ms )) $(( ns % q ))
    u='ms'
  elif (( q *= 1000, (s = ns / q ) < 600 )); then
    printf -vr "%d.%09d" $(( s )) $(( ns % q ))
    u='s'
  elif (( q *= 60, (m = ns / q ) < 60 )); then
    printf -vr "%dm%02d" $(( m )) $(( s % 60 ))
    u='s'
  elif (( q *= 60, (h = ns / q ) < 96 )); then
    printf -vr "%dh%02d" $(( h )) $(( m % 60 ))
    u='m'
  elif (( q *= 24, (d = ns / q ) < 100 )); then
    printf -vr "%dd%02d" $(( d )) $(( h % 24 ))
    u='h'
  else
    printf -vr "%d" $(( d ))
    u='d'
  fi
  local -i ul=${#u}
  (( l = maxw - ul ))
  r=${r:0:$l}
  echo "${r%.}$u"
}

case "$1" in
  [0-9]*)
    for t in "$@"; do
      r="                  $(fmt_compact_time "$t")"
      r="${r: -15}"
      printf "%25s:%s\n" "$t" "$r"
    done
    ;;
  run)
    shift
    t0=`date +10#%s%N`
    "$@"
    t1=`date +10#%s%N`
    echo "Elapsed: $(fmt_compact_time $(( t1 - t0 )) )"
    ;;
esac
