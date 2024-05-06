(use judge)
(use /src/noq-janet)

######################
# Conversion to string 
######################

(test (expr/to-string [:symbol "a"])
      "a")

(test (expr/to-string [:functor "test"
                       [:symbol "lol"]])
      "test(lol)")

(test (rule/to-string [:rule
                       {:head [:functor "swap"
                               [:functor "pair"
                                [:symbol "a"]
                                [:symbol "b"]]]
                        :body [:functor "pair"
                               [:symbol "b"]
                               [:symbol "a"]]}])
      "swap(pair(a, b)) = pair(b, a)")

(test-error (rule/to-string {:head [:functor "swap"
                                    [:functor "pair"
                                     [:symbol "a"]
                                     [:symbol "b"]]]
                             :body [:functor "pair"
                                    [:symbol "b"]
                                    [:symbol "a"]]})
            "expected string, symbol, keyword, array, tuple, table, struct or buffer, got nil")

(test (rule/to-string [:rule
                       {:head [:functor "swap"
                               [:functor "pair"
                                [:functor "f"
                                 [:symbol "c"]]
                                [:functor "g"
                                 [:symbol "d"]]]]
                        :body [:functor "pair"
                               [:functor "g"
                                [:symbol "d"]]
                               [:functor "f"
                                [:symbol "c"]]]}])
      "swap(pair(f(c), g(d))) = pair(g(d), f(c))")

##################
# Pattern matching
##################

(test (pattern-match [:symbol "a"]
                     [:symbol "a"])
      @{"a" [:symbol "a"]})

(test (pattern-match [:symbol "a"]
                     [:functor "yes" [:symbol "b"]])
      @{"a" [:functor "yes" [:symbol "b"]]})

(test (pattern-match [:functor "fails"
                      [:functor "pair"
                       [:symbol "a"]
                       [:symbol "b"]]]
                     [:functor "nope"
                      [:symbol "c"]])
      nil)

(test (pattern-match [:functor "swap"
                      [:functor "pair"
                       [:symbol "a"]
                       [:symbol "b"]]]
                     [:functor "swap"
                      [:functor "pair"
                       [:functor "f"
                        [:symbol "c"]]
                       [:functor "g"
                        [:symbol "d"]]]])
      @{"a" [:functor "f" [:symbol "c"]]
        "b" [:functor "g" [:symbol "d"]]})

(test (pattern-match [:functor "good-dup"
                      [:symbol "a"]
                      [:symbol "a"]]
                     [:functor "good-dup"
                      [:symbol "g"]
                      [:symbol "g"]])
      @{"a" [:symbol "g"]})

(test (pattern-match [:functor "bad-dup"
                      [:symbol "a"]
                      [:symbol "a"]]
                     [:functor "bad-dup"
                      [:symbol "g"]
                      [:functor "pair"
                       [:symbol "one"]
                       [:symbol "two"]]])
      nil)

(test (pattern-match [:functor "swap"
                      [:functor "pair"
                       [:symbol "a"]
                       [:symbol "b"]]]
                     [:functor "swap"
                      [:functor "pair"
                       [:symbol "first"]
                       [:symbol "second"]]])
      @{"a" [:symbol "first"]
        "b" [:symbol "second"]})

(test (pattern-match [:functor "swap"
                      [:functor "pair"
                       [:symbol "a"]
                       [:symbol "b"]]]
                     [:functor "foo"
                      [:functor "swap"
                       [:functor "pair"
                        [:symbol "first"]
                        [:symbol "second"]]]])
      nil)

######################
# Binding substitution
######################

(test (sub-bindings {"a" [:symbol "d"]
                     "b" [:symbol "e"]
                     "c" [:functor "test" [:symbol "lol"]]}
                    [:functor "pair"
                     [:symbol "a"]
                     [:symbol "b"]
                     [:symbol "c"]
                     [:symbol "b"]
                     [:symbol "a"]])
      [:functor
       "pair"
       [:symbol "d"]
       [:symbol "e"]
       [:functor "test" [:symbol "lol"]]
       [:symbol "e"]
       [:symbol "d"]])

################
# Applying rules
################

(test (rule/apply-all [:rule
                       {:head [:functor "swap"
                               [:functor "pair"
                                [:symbol "a"]
                                [:symbol "b"]]]
                        :body [:functor "pair"
                               [:symbol "b"]
                               [:symbol "a"]]}]
                      [:functor "swap"
                       [:functor "pair"
                        [:symbol "first"]
                        [:symbol "second"]]])
      [:functor
       "pair"
       [:symbol "second"]
       [:symbol "first"]])

(test (rule/apply-all [:rule
                       {:head [:functor "swap"
                               [:functor "pair"
                                [:symbol "a"]
                                [:symbol "b"]]]
                        :body [:functor "pair"
                               [:symbol "b"]
                               [:symbol "a"]]}]
                      [:functor "foo"
                       [:functor "swap"
                        [:functor "pair"
                         [:symbol "first"]
                         [:symbol "second"]]]
                       [:functor "swap"
                        [:functor "pair"
                         [:symbol "first"]
                         [:symbol "second"]]]])
      [:functor
       "foo"
       [:functor
        "pair"
        [:symbol "second"]
        [:symbol "first"]]
       [:functor
        "pair"
        [:symbol "second"]
        [:symbol "first"]]])

################
# Lexing strings
################

(test (peg/match lex-peg " () ")
      @[[:open-paren "("]
        [:close-paren ")"]])

(test (peg/match lex-peg "f()")
      @[[:symbol "f"]
        [:open-paren "("]
        [:close-paren ")"]])

(test (peg/match lex-peg "swap(pair(a, b)) = pair(b, a)")
      @[[:symbol "swap"]
        [:open-paren "("]
        [:symbol "pair"]
        [:open-paren "("]
        [:symbol "a"]
        [:symbol "b"]
        [:close-paren ")"]
        [:close-paren ")"]
        [:equals "="]
        [:symbol "pair"]
        [:open-paren "("]
        [:symbol "b"]
        [:symbol "a"]
        [:close-paren ")"]])

(test (peg/match ast-peg "pair(a, b)")
      @[[:functor
         "pair"
         [:symbol "a"]
         [:symbol "b"]]])

(test (peg/match ast-peg "swap(pair(a, b))")
      @[[:functor
         "swap"
         [:functor
          "pair"
          [:symbol "a"]
          [:symbol "b"]]]])

(test (peg/match ast-peg "swap(pair(f(a), g(b)))")
      @[[:functor
         "swap"
         [:functor
          "pair"
          [:functor "f" [:symbol "a"]]
          [:functor "g" [:symbol "b"]]]]])

(deftest "round trip from string to expr and back again"
  (def test-string "swap(pair(a, b))")
  (test (= test-string (expr/to-string (expr/from-string test-string)))
        true))

(deftest "test whitespace handling in expr/from-string"
  (def cases ["   swap(pair(a, b))"
              "swap   (pair(a, b))"
              "swap(   pair(a, b))"
              "swap(pair   (a, b))"
              "swap(pair(   a, b))"
              "swap(pair(a,    b))"
              "swap(pair(a, b   ))"])
  (test (map expr/from-string cases)
    @[[:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
      [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
      [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
      [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
      [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
      [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
      [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]]))

(test (rule/from-string "swap(pair(a, b)) = pair(b, a)")
      [:rule
       {:body [:functor
               "pair"
               [:symbol "b"]
               [:symbol "a"]]
        :head [:functor
               "swap"
               [:functor
                "pair"
                [:symbol "a"]
                [:symbol "b"]]]}])

(deftest "test whitespace handling in rule/from-string"
  (def cases ["   swap(pair(a, b)) = pair(b, a)"
              "swap   (pair(a, b)) = pair(b, a)"
              "swap(   pair(a, b)) = pair(b, a)"
              "swap(pair   (a, b)) = pair(b, a)"
              "swap(pair(   a, b)) = pair(b, a)"
              "swap(pair(a,    b)) = pair(b, a)"
              "swap(pair(a, b   )) = pair(b, a)"])
  (test (map rule/from-string cases)
        @[[:rule
           {:body [:functor "pair" [:symbol "b"] [:symbol "a"]]
            :head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]}]
          [:rule
           {:body [:functor "pair" [:symbol "b"] [:symbol "a"]]
            :head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]}]
          [:rule
           {:body [:functor "pair" [:symbol "b"] [:symbol "a"]]
            :head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]}]
          [:rule
           {:body [:functor "pair" [:symbol "b"] [:symbol "a"]]
            :head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]}]
          [:rule
           {:body [:functor "pair" [:symbol "b"] [:symbol "a"]]
            :head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]}]
          [:rule
           {:body [:functor "pair" [:symbol "b"] [:symbol "a"]]
            :head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]}]
          [:rule
           {:body [:functor "pair" [:symbol "b"] [:symbol "a"]]
            :head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]}]]))

(deftest "round trip from string to rule and back again"
  (def test-string "swap(pair(a, b)) = pair(b, a)")
  (test (= test-string (rule/to-string (rule/from-string test-string)))
        true))
