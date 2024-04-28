(use judge)
(import spork/schema)

(comment

  # Expr
  [:symbol ""]
  [:functor "" @[]]

  # Rule
  {:head :expr
   :body :expr})

(defn rule/apply [rule])

(defn expr/to-string [expr]
  (match expr
    [:symbol sym] sym

    [:functor name & rest]
    (string name "(" (string/join (map expr/to-string rest) ", ") ")")

    (error "`expr/to-string` received an illegal form")))

(defn rule/to-string [rule]
  (def [tag {:head head
             :body body}] rule)
  (assert (= tag :rule) "`rule/to-string` received an illegal form")

  (string (expr/to-string head) " = " (expr/to-string body)))

(defn pattern-match [pattern value]
  (var ret @{})

  (defn pattern-match* [pattern value]
    (match [pattern value]
      [[:symbol sym] _]
      (if (get ret sym)
        (if (deep= (get ret sym) value)
          ret (set ret @{}))
        (put ret sym value))

      [[:functor pname & prest] [:functor vname & vrest]]
      (if (and (= pname vname) (= (length prest) (length vrest)))
        (each pair (map |$& prest vrest)
          (pattern-match* ;pair)
          (if (empty? ret) (break)))
        (set ret @{})))

    ret)

  (pattern-match* pattern value)
  (if (empty? ret) nil ret))

(defn main [& args]

  # swap(pair(a, b)) = pair(b, a)


  (print "Hello, world!"))
