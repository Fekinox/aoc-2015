(defsystem :aoc-2015
  :depends-on (:aoc-2015/utils
               :str
               :ironclad/digest/md5
               :jsown
               :damn-fast-priority-queue)
  :serial t
  :components
  ((:module "src" :serial t
    :components
    ((:file "package")
     (:module "problems" :serial t
      :components ((:file "aoc01")
                   (:file "aoc02")
                   (:file "aoc03")
                   (:file "aoc04")
                   (:file "aoc05")
                   (:file "aoc06")
                   (:file "aoc07")
                   (:file "aoc08")
                   (:file "aoc09")
                   (:file "aoc10")
                   (:file "aoc11")
                   (:file "aoc12")
                   (:file "aoc13")
                   (:file "aoc14")
                   (:file "aoc15")
                   (:file "aoc16")
                   (:file "aoc17")
                   (:file "aoc18")
                   (:file "aoc19")
                   (:file "aoc20")
                   (:file "aoc21")
                   (:file "aoc22")
                   (:file "aoc23")
                   (:file "aoc24")
                   (:file "aoc25")))))))

(defsystem :aoc-2015/utils
  :depends-on (:str)
  :serial t
  :components
  ((:module "src" :serial t
    :components
    ((:module "utils" :serial t
      :components ((:file "package")
                   (:file "conv")
                   (:file "exec")
                   (:file "groups")))))))

(defsystem :aoc-2015/bin
  :depends-on (:aoc-2015)
  :serial t
  :components
  ((:module "src" :serial t
    :components ((:file "main")))))
