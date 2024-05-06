(use judge)

(comment

  # Expressions can have one of two forms
  [:symbol ""]
  [:functor "" @[]]

  # Rule must have this form
  [:rule {:head <Expression>
          :body <Expression>}])

(defmacro . [x] # Equivalent to `second`
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
        (set ret @{}))

      (error "`pattern-match` received an illegal form"))

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
      [:functor name ;(seq [exp :in rest]
                        (rule/apply-all rule exp))]

      (error "`rule/apply-all` received an illegal form"))))

(def lex-peg
  ~{:open-paren (/ (<- "(") [:open-paren "("])
    :close-paren (/ (<- ")") [:close-paren ")"])
    :symbol (/ (<- (some (+ :w :d))) ,|[:symbol $])
    :equals (/ (<- "=") [:equals "="])
    :main (some (+ :open-paren :close-paren :symbol :equals "," :s*))})

(def ast-peg
  ~{:symbol (/ (<- (some (+ :w :d))) ,|[:symbol $])
    :functor (/ (* :s* (<- (some (+ :w :d))) :s* "(" :s* (any (* :expr (? ",") :s*)) :s* ")" :s*)
                ,(fn [name & rest] 
                   [:functor name ;rest]))
    :expr (+ :functor :symbol)
    :main :expr})

(defn expr/from-string [str]
  (let [match (peg/match ast-peg str)]
    (if (empty? match)
      nil
      (first match))))

(def rule-peg
  (merge ast-peg
  ~{:main (* :expr :s* "=" :s* :expr)}))

(defn rule/from-string [str]
  (let [match (peg/match rule-peg str)]
    (if (empty? match)
      nil 
      [:rule {:head (get match 0)
              :body (get match 1)}])))

(defn tokens->ast [tokens &opt state]
  (var state (or state 0))
  (def [this & rest] tokens)
  
  (match this
    [:symbol name] (case (first (first rest))  
                     :open-paren [:functor name (tokens->ast rest state)]
                     :symbol
                     :close-paren)
    [:open-paren "("] (tokens->ast rest (+= state 1)))
    [:close-paren ")"] (tokens->ast rest (-= state 1)))

(comment
  
  (tokens->ast [[:symbol "f"] [:open-paren "("] [:close-paren ")"]])
  )

(defn main [& args]
  (print "Hello, world! Type `.help` for help. Type `.quit` to exit.")
  
  (var quit? false)
  (var input "")

  (while (not quit?)
    (prin "> ")
    (flush)
    (set input (string/trim (getline)))
    (case input 
      ".quit" (do (set quit? true) (break))
      ".exit" (do (set quit? true) (break))
      ".q" (do (set quit? true) (break))
      ":q" (do (set quit? true) (break))
      ":Q" (do (set quit? true) (break))
      ".help" (print "TODO: Some helpful help here"))
    (print "Got: " input)
    (try
      (do (pp (expr/from-string input)))
      ([err fib]
       (print "That didn't work, try again")))))
