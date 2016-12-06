val s = Set(1,2,3)
val v = Set(4,5)

v ++ v.filter(s.contains(_))