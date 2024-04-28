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

(test (rule/to-string [:rule
                       {:head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
                        :body [:functor "pair" [:symbol "b"] [:symbol "a"]]}])
      "swap(pair(a, b)) = pair(b, a)")

(test-error (rule/to-string {:head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
                             :body [:functor "pair" [:symbol "b"] [:symbol "a"]]})
            "expected string, symbol, keyword, array, tuple, table, struct or buffer, got nil")

(test (rule/to-string [:rule
                       {:head [:functor "swap" [:functor "pair" [:functor "f" [:symbol "c"]] [:functor "g" [:symbol "d"]]]]
                        :body [:functor "pair" [:functor "g" [:symbol "d"]] [:functor "f" [:symbol "c"]]]}])
      "swap(pair(f(c), g(d))) = pair(g(d), f(c))")

(defn pattern-match [pattern value]
  (var ret @{})

  (defn pattern-match* [pattern value]
    (match [pattern value]
      [[:symbol sym] _] (put ret sym value)

      [[:functor pname & prest] [:functor vname & vrest]]
      (if (and (= pname vname) (= (length prest) (length vrest)))
        (each pair (map |$& prest vrest)
          (pattern-match* ;pair)
          (if (empty? ret) (break)))
        (set ret @{})))

    ret)

  (pattern-match* pattern value)
  (if (empty? ret) nil ret))

(test (pattern-match [:symbol "a"]
                     [:symbol "a"])
      @{"a" [:symbol "a"]})

(test (pattern-match [:symbol "a"]
                     [:functor "yes" [:symbol "b"]])
      @{"a" [:functor "yes" [:symbol "b"]]})

(test (pattern-match [:functor "fails" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
                     [:functor "nope" [:symbol "c"]])
      nil)

(test (pattern-match [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
                     [:functor "swap" [:functor "pair" [:functor "f" [:symbol "c"]] [:functor "g" [:symbol "d"]]]])
      @{"a" [:functor "f" [:symbol "c"]]
        "b" [:functor "g" [:symbol "d"]]})

(defn main [& args]

  # swap(pair(a, b)) = pair(b, a)


  (print "Hello, world!"))
