(use judge)
(import spork/schema)

(comment

  # Expr
  [:symbol ""]
  [:functor "" @[]]

  # Rule
  {:head :expr
   :body :expr})

(defmacro . [x]
  (with-syms [$x]
    ~(let [,$x ,x]
       (,$x 1))))

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

(defn sub-bindings [bindings expr]
  (match expr
    [:symbol name]
    (if-let [binding (bindings name)]
      binding
      expr)

    [:functor name & rest]
    (let [new-name (match (bindings name)
                     [:symbol n] n
                     nil name
                     (error "Expected symbol in the place of the functor name"))
          new-args (map |(sub-bindings bindings $) rest)]
      [:functor new-name ;new-args])))

(defn rule/apply-all [rule expr]
  (if-let [bindings (pattern-match ((. rule) :head) expr)]
    (sub-bindings bindings ((. rule) :body))
    (match expr
      [:symbol _] expr
      [:functor name & rest]
      (seq [exp :in rest]
        (rule/apply-all rule exp)))))

(defn main [& args]

  # swap(pair(a, b)) = pair(b, a)
  (def swap [:rule
             {:head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
              :body [:functor "pair" [:symbol "b"] [:symbol "a"]]}])

  (def expr [:functor "foo"
             [:functor "swap"
              [:functor "pair"
               [:symbol "first"]
               [:symbol "second"]]]
             [:functor "swap"
              [:functor "pair"
               [:symbol "first"]
               [:symbol "second"]]]])

  (print (rule/to-string swap))
  (print (expr/to-string expr)))
