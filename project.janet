(declare-project
  :name "noq-janet"
  :description "TODO: Write a cool description")

(declare-archive
  :name "noq-janet"
  :entry "/src/noq-janet")

(declare-binscript
  :main "src/jnoq"
  :is-janet true)
