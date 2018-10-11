using Gadfly
using Colors

import Base.Dates

set_default_plot_size(14cm, 40cm)

type Stop
  time :: Int64
  place :: AbstractString
end

type Journey
  mode :: AbstractString  # e.g. Weekdays, Saturdays, etc.
  service :: AbstractString
  stops :: Array{Stop}
end

function Journey{T <: AbstractString}(mode :: AbstractString,
                                      service :: AbstractString,
                                      stops :: Array{Tuple{T, T}})
  Journey(mode, service, [ Stop(totime(s[1]), s[2]) for s = stops ])
end

function totime(s :: AbstractString)
  @assert(length(s) == 4, "Invalid time '$s'")
  60 * parse(Int64, s[1:2]) + parse(Int64, s[3:4])
end

function fmttime(t)
  t = Int64(t)
  (h, m) = divrem(t, 60)
  (d, h) = divrem(h, 24)
  dd = h == 0 && m == 0 ? "(+$(d)d) " : ""
  dd * lpad("$h", 2, "0") * ":" * lpad("$m", 2, "0")
end

dayofweek = Dict([(Dates.english_daysofweekabbr[i], i-1) for i=1:7 ])

tt1 = [
  Journey("Mon", "CalMac", [
    ("1015", "Mallaig"),
    ("1130", "Eigg"),
    ("1145", "Eigg"),
    ("1245", "Rum"),
    ("1255", "Rum"),
    ("1350", "Canna"),
    ("1405", "Canna"),
    ("1500", "Rum"),
    ("1510", "Rum"),
    ("1610", "Eigg"),
    ("1625", "Eigg"),
    ("1740", "Mallaig"),
  ]),
  Journey("Tue", "CalMac", [
    ("1025", "Mallaig"),
    ("1155", "Muck"),
    ("1210", "Muck"),
    ("1245", "Eigg"),
    ("1300", "Eigg"),
    ("1335", "Muck"),
    ("1350", "Muck"),
    ("1530", "Mallaig"),
  ]),
  Journey("Wed", "CalMac", [
    ("1015", "Mallaig"),
    ("1135", "Rum"),
    ("1150", "Rum"),
    ("1245", "Canna"),
    ("1515", "Canna"),
    ("1610", "Rum"),
    ("1620", "Rum"),
    ("1740", "Mallaig"),
  ]),
  Journey("Thu", "CalMac", [
    ("1015", "Mallaig"),
    ("1130", "Eigg"),
    ("1140", "Eigg"),
    ("1215", "Muck"),
    ("1230", "Muck"),
    ("1305", "Eigg"),
    ("1315", "Eigg"),
    ("1430", "Mallaig"),
  ]),
  Journey("Fri", "CalMac", [
    ("0825", "Mallaig"),
    ("1005", "Muck"),
    ("1015", "Muck"),
    ("1050", "Eigg"),
    ("1105", "Eigg"),
    ("1220", "Mallaig"),
    ("1235", "Mallaig"),
    ("1355", "Rum"),
    ("1405", "Rum"),
    ("1500", "Canna"),
    ("1510", "Canna"),
    ("1605", "Rum"),
    ("1620", "Rum"),
    ("1740", "Mallaig"),
  ]),
  Journey("Sat", "CalMac", [
    ("0730", "Mallaig"),
    ("0850", "Rum"),
    ("0905", "Rum"),
    ("1000", "Canna"),
    ("1015", "Canna"),
    ("1150", "Muck"),
    ("1200", "Muck"),
    ("1235", "Eigg"),
    ("1250", "Eigg"),
    ("1405", "Mallaig"),
    ("1425", "Mallaig"),
    ("1540", "Eigg"),
    ("1550", "Eigg"),
    ("1625", "Muck"),
    ("1635", "Muck"),
    ("1810", "Canna"),
    ("1825", "Canna"),
    ("1920", "Rum"),
    ("1930", "Rum"),
    ("2050", "Mallaig"),
  ]),
  Journey("Sun", "CalMac", [
    ("0910", "Mallaig"),
    ("1030", "Rum"),
    ("1045", "Rum"),
    ("1140", "Canna"),
    ("1155", "Canna"),
    ("1400", "Mallaig"),
    ("1420", "Mallaig"),
    ("1535", "Eigg"),
    ("1550", "Eigg"),
    ("1625", "Muck"),
    ("1635", "Muck"),
    ("1815", "Mallaig"),
  ])]

const TimetableInput = Tuple{AbstractString, AbstractString, AbstractString}

ttsmallisles = Array{TimetableInput}[[
    ("Mon", "1015", "Mallaig"),
    ("Mon", "1130", "Eigg"),
    ("Mon", "1145", "Eigg"),
    ("Mon", "1245", "Rum"),
    ("Mon", "1255", "Rum"),
    ("Mon", "1350", "Canna"),
    ("Mon", "1405", "Canna"),
    ("Mon", "1500", "Rum"),
    ("Mon", "1510", "Rum"),
    ("Mon", "1610", "Eigg"),
    ("Mon", "1625", "Eigg"),
    ("Mon", "1740", "Mallaig"),
  ], [
    ("Tue", "1025", "Mallaig"),
    ("Tue", "1155", "Muck"),
    ("Tue", "1210", "Muck"),
    ("Tue", "1245", "Eigg"),
    ("Tue", "1300", "Eigg"),
    ("Tue", "1335", "Muck"),
    ("Tue", "1350", "Muck"),
    ("Tue", "1530", "Mallaig"),
  ], [
    ("Wed", "1015", "Mallaig"),
    ("Wed", "1135", "Rum"),
    ("Wed", "1150", "Rum"),
    ("Wed", "1245", "Canna"),
    ("Wed", "1515", "Canna"),
    ("Wed", "1610", "Rum"),
    ("Wed", "1620", "Rum"),
    ("Wed", "1740", "Mallaig"),
  ], [
    ("Thu", "1015", "Mallaig"),
    ("Thu", "1130", "Eigg"),
    ("Thu", "1140", "Eigg"),
    ("Thu", "1215", "Muck"),
    ("Thu", "1230", "Muck"),
    ("Thu", "1305", "Eigg"),
    ("Thu", "1315", "Eigg"),
    ("Thu", "1430", "Mallaig"),
  ], [
    ("Fri", "0825", "Mallaig"),
    ("Fri", "1005", "Muck"),
    ("Fri", "1015", "Muck"),
    ("Fri", "1050", "Eigg"),
    ("Fri", "1105", "Eigg"),
    ("Fri", "1220", "Mallaig"),
    ("Fri", "1235", "Mallaig"),
    ("Fri", "1355", "Rum"),
    ("Fri", "1405", "Rum"),
    ("Fri", "1500", "Canna"),
    ("Fri", "1510", "Canna"),
    ("Fri", "1605", "Rum"),
    ("Fri", "1620", "Rum"),
    ("Fri", "1740", "Mallaig"),
  ], [
    ("Sat", "0730", "Mallaig"),
    ("Sat", "0850", "Rum"),
    ("Sat", "0905", "Rum"),
    ("Sat", "1000", "Canna"),
    ("Sat", "1015", "Canna"),
    ("Sat", "1150", "Muck"),
    ("Sat", "1200", "Muck"),
    ("Sat", "1235", "Eigg"),
    ("Sat", "1250", "Eigg"),
    ("Sat", "1405", "Mallaig"),
    ("Sat", "1425", "Mallaig"),
    ("Sat", "1540", "Eigg"),
    ("Sat", "1550", "Eigg"),
    ("Sat", "1625", "Muck"),
    ("Sat", "1635", "Muck"),
    ("Sat", "1810", "Canna"),
    ("Sat", "1825", "Canna"),
    ("Sat", "1920", "Rum"),
    ("Sat", "1930", "Rum"),
    ("Sat", "2050", "Mallaig"),
  ], [
    ("Sun", "0910", "Mallaig"),
    ("Sun", "1030", "Rum"),
    ("Sun", "1045", "Rum"),
    ("Sun", "1140", "Canna"),
    ("Sun", "1155", "Canna"),
    ("Sun", "1400", "Mallaig"),
    ("Sun", "1420", "Mallaig"),
    ("Sun", "1535", "Eigg"),
    ("Sun", "1550", "Eigg"),
    ("Sun", "1625", "Muck"),
    ("Sun", "1635", "Muck"),
    ("Sun", "1815", "Mallaig"),
  ]]

function ttjourney(j :: Journey, offset :: Int64 = 0)
  xs = [ s.place for s in j.stops ]
  ys = [ s.time for s in j.stops ]
  lbl = Geom.label(hide_overlaps=false)
  layer(x=xs, y=ys.+offset, label=map(fmttime, ys), Geom.path, Geom.point, lbl)
end

function timetable(js :: Array{Journey})
  starts = [ div(j.stops[1].time - 59, 60) * 60 for j=js ]
  ends = [ div(j.stops[end].time + 119, 60) * 60 for j=js ]
  durations = ends - starts
  offsets = cumsum([0; durations])[1:end-1] - starts
  layers = map(ttjourney, js, offsets)

  scy = Scale.y_continuous(labels=x -> "")
  #scx = Scale.x_discrete(levels=unique(sort(xs)))
  scx = Scale.x_discrete(levels=["Muck", "Eigg", "Mallaig", "Rum", "Canna"])
  cart = Coord.cartesian(yflip=true)
  yticks = Guide.yticks(ticks=collect(0:60:sum(durations)))

  plot(layers..., scx, scy, cart, yticks)
end

function ttparse(e :: TimetableInput)
  (1440 * dayofweek[e[1]] + totime(e[2]), e[3])
end

function timetable(tt :: Array{TimetableInput})
  scy = Scale.y_continuous(labels=fmttime)
  #scx = Scale.x_discrete(levels=unique(sort(xs)))
  scx = Scale.x_discrete(levels=["Muck", "Eigg", "Mallaig", "Rum", "Canna"])
  cart = Coord.cartesian(yflip=true)
  lbl = Geom.label(hide_overlaps=false)
  yticks = Guide.yticks(ticks=collect((9*60):60:(1440*6+20*60)))

  xs = [ e[3] for e = tt ]
  ys = [ 1440 * dayofweek[e[1]] + totime(e[2]) for e = tt ]

  plot(x=xs, y=ys, label=map(fmttime, ys),
       Geom.point, Geom.path, scx, scy, cart, lbl, yticks,
       Guide.ylabel(""))
end

function timetableold(tt :: Array{TimetableInput})

  scy = Scale.y_continuous(labels=fmttime)
  #scx = Scale.x_discrete(levels=unique(sort(xs)))
  scx = Scale.x_discrete(levels=["Muck", "Eigg", "Mallaig", "Rum", "Canna"])
  cart = Coord.cartesian(yflip=true)
  lbl = Geom.label(hide_overlaps=false)
  yticks = Guide.yticks(ticks=collect((9*60):60:(1440*6+20*60)))

  xs = [ e[3] for e = tt ]
  ys = [ 1440 * dayofweek[e[1]] + totime(e[2]) for e = tt ]

  plot(x=xs, y=ys, label=map(fmttime, ys),
       Geom.point, Geom.path, scx, scy, cart, lbl, yticks)
end

function timetableday(tt, xlevels)
  xs = [ e[3] for e = tt ]
  ys = [ totime(e[2]) for e = tt ]
  scy = Scale.y_continuous(labels=fmttime)
  scx = Scale.x_discrete(levels=xlevels)
  coord = Coord.cartesian(yflip=true)
  hourmin = minimum(map(Dates.hour, ys))
  hourmax = maximum(map(t -> Dates.hour(t + Dates.Minute(59)), ys))
  hourmin = 7
  hourmax = 21
  hourly = [datetime(dates.hour(h)) for h=hourmin:hourmax]
  yt = guide.yticks(ticks=hourly)
  glab = Geom.label(hide_overlaps=false)
  plot(x=xs, y=ys, label=map(fmttime, ys), scx, scy,
       glab, Geom.point, Geom.path, yt, coord)
end
