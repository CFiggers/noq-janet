(use judge)
(use /src/noq-janet)


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

(test (pattern-match [:functor "good-dup" [:symbol "a"] [:symbol "a"]]
                     [:functor "good-dup" [:symbol "g"] [:symbol "g"]])
  @{"a" [:symbol "g"]})

(test (pattern-match [:functor "bad-dup" [:symbol "a"] [:symbol "a"]]
                     [:functor "bad-dup" [:symbol "g"] [:functor "pair" [:symbol "one"] [:symbol "two"]]])
      nil)

(test (sub-bindings {"a" [:symbol "d"]
                     "b" [:symbol "e"]}
                    [:functor "pair" [:symbol "d"]
                     [:symbol "e"]])
      [:functor
       "pair"
       [:symbol "d"]
       [:symbol "e"]])

(test (pattern-match [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
                     [:functor "swap"
                      [:functor "pair"
                       [:symbol "first"]
                       [:symbol "second"]]])
  @{"a" [:symbol "first"]
    "b" [:symbol "second"]})

(test (pattern-match [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
                     [:functor "foo"
                      [:functor "swap"
                       [:functor "pair"
                        [:symbol "first"]
                        [:symbol "second"]]]])
      nil)

(test (rule/apply-all [:rule
                       {:head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
                        :body [:functor "pair" [:symbol "b"] [:symbol "a"]]}]
                      [:functor "swap"
                       [:functor "pair"
                        [:symbol "first"]
                        [:symbol "second"]]])
  [:functor
   "pair"
   [:symbol "second"]
   [:symbol "first"]])

(test (rule/apply-all [:rule
                       {:head [:functor "swap" [:functor "pair" [:symbol "a"] [:symbol "b"]]]
                        :body [:functor "pair" [:symbol "b"] [:symbol "a"]]}]
                      [:functor "foo"
                       [:functor "swap"
                        [:functor "pair"
                         [:symbol "first"]
                         [:symbol "second"]]]
                       [:functor "swap"
                        [:functor "pair"
                         [:symbol "first"]
                         [:symbol "second"]]]])
  @[[:functor
     "pair"
     [:symbol "second"]
     [:symbol "first"]]
    [:functor
     "pair"
     [:symbol "second"]
     [:symbol "first"]]])
