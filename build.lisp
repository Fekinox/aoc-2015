(ql:quickload :aoc-2015/bin)

(sb-ext:save-lisp-and-die "aoc-2015"
  :toplevel 'aoc-bin:main
  :executable t)
