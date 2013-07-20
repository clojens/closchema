(ns closchema.test.core
  (:use [closchema.core :only [validate]]
        [clojure.test]))

(def base-schema {:type "object"
                  :properties {:id {:type "number"}
                               :name {:type "string"}
                               :description {:optional true :type "string"}}})

(deftest validate-properties
  (let [s base-schema]
    (is (validate s {:id 1 :name "shoe"})
        "should accept object with only required properties")
    (is (validate s {:id 1 :name "shoe" :description "style"})
        "should accept object with optional properties")
    (is (not (validate s {:id 1}))
        "should not accept when required property is not present")
    (is (not (validate s {:id "1" :name "bag"}))
        "should not accept when property type is incorrect")
    (is (validate s {:id 1 :name "test" :description nil})
        "should accept an optional property with value null")))

(deftest validate-false-additional-properties
  (let [s (merge base-schema {:additionalProperties false})]
    (is (not (validate s {:id 1 :name "wine" :age "13 years"}))
        "should not allow any properties that are not defined in schema")
    (is (validate s {:id 1 :name "wine" :description "good"})
        "should allow any properties that are defined in schema as optional")))

(deftest validate-additional-properties-schema
  (let [s (merge base-schema {:additionalProperties {:type "string"}})]
    (is (and (validate s
                       {:id 1 :name "wine" :age "13 years" :country "france"})
             (not (validate s {:id 1 :name "wine" :age 13})))
        "should enforce that all extra properties conform to the schema")))

(deftest validate-additional-properties-fields
  (let [s (assoc base-schema
            :properties (merge (base-schema :properties)
                               {:address  {:type "string" :optional true}
                                :address2 {:type "string"
                                           :requires "address"
                                           :optional true}}))]
    (is (validate s {:id 1 :name "john" :address "street"})
        "should validate with non-required")
    (is (validate s {:id 1 :name "john" :address "street" :address2 "country"})
        "should validate when both are present" )
    (is (not (validate s {:id 1 :name "john" :address2 "country"}))
        "should not validate nem required is not present")))

(deftest validate-items-any
  (is (and (validate {:type "array"} [ 1 2 3])
           (validate {:type "array" :items []} [1 "2" 3]))
      "should allow any items"))

(deftest validate-items-with-object-schema
  (let [s1 {:type "array" :items {:type "string"}}
        s2 {:type "array" :items {:type "object"
                                  :properties {:name {:type "string"}}}}]
    (is (validate s1 []) "should accept empty array")
    (is (and (validate s1 ["a" "b" "c"])
             (validate s2 [{:name "half"} {:name "life"}]))
        "should accept homogenous array")
    (is (not (validate s1 ["a" "b" 3])) "should not accept heterogenous array")
    (is (not (validate s2 [{:name "quake"} {:id 1}]))
        "should not accept if inner item does not follow item schema")))

(deftest validate-items-with-schema-array
  (let [s {:type "array" :items [{:type "string"} {:type "string"}]}]
    (is (and (validate s ["a" "b"])
             (not (validate s ["a"]))
             (not (validate s ["a" 1])))
        "should enforce tuple typing")
    (is (and (validate s ["a" "b" "c"]) (validate s ["a" "b" 1 2 3 {}]))
        "should allow additional properties to be defined by default")))

(deftest validate-items-with-additional-properties
  (let [s {:type "array" :items [{:type "number"}]
           :additionalProperties {:type "string"}}]
    (is (and (validate s [1 "a" "b" "c"]) (not (validate s [1 2 "a"])))
        "should ensure type schema for additional properties")
    (is (not (validate s ["a" 1])) "should still enforce tuple typing")))

(deftest validate-items-with-additional-properties-disabled
  (let [s {:type "array" :items [{:type "number"}]
           :additionalProperties false}]
    (is (validate s [1]) "should be strict about tuple typing")
    (is (not (validate s [1 "a"])) "should be strict about tuple typing")))

(deftest validate-items-with-unique-items-set
  (let [s {:type "array" :uniqueItems true}]
    (is (validate s [1 1]) "of type numbers")
    (is (validate s ["a" "a" "a"]) "of type strings")
    (is (validate s [{:a 1} {:a 1} {:a 1}]) "of type objects")
    (is (not (validate s [1 2 3])) "of type numbers")
    (is (not (validate s ["a" "b"])) "of type strings")
    (is (not (validate s [{:a 1} {:b 1} {:a 1}])) "of type objects")))

(deftest validate-common-string
  (let [s {:type "string"}]
    (is (validate s "foobar") "should validate with string")))

(deftest validate-minimum-string
  (let [s {:type "string" :minLength 3}]
    (is (validate s "foobar") "validate if within the lengths")
    (is (not (validate s "fo")) "not validate if not within the lengths")))

(deftest validate-maximum-string
  (let [s {:type "string" :maxLength 5}]
    (is (validate s "fooba") "validate if within the lengths")
    (is (not (validate s "foobar")) "not validate if not within the lengths")))

(deftest validate-min-max-string
  (let [s {:type "string" :maxLength 5 :minLength 3}]
    (is (and (validate s "foo") (validate s "fooba"))
        "validate if within the lengths" )
    (is (and (not (validate s "fo")) (not (validate s "foobar")))
        "not validate if not within the lengths")))

(deftest validate-string-pattern
  (let [s {:type "string" :pattern "^[a-zA-Z0-9]+$"}]
    (is (and (validate s "fooBar")
             (validate s "fooBar123")
             (validate s "123fooBar"))
        "pattern matches")
    (is (and (not  (validate s "foo-Bar")) (not (validate s "foo_bar")))
        "pattern doesn't match")))

(deftest validate-common-numbers
  (let [s {:type "number"}]
    (is (and (validate s 1) (validate s -2) (validate s 3.5)) "is a number")
    (is (not (validate s "2")) "not a number")))

(deftest validate-max-number
  (let [s {:type '"number" :maximum 5}]
    (is (validate s 4) "lower than maximum")
    (is (and (not (validate s 6)) (not (validate s 5)))
        "greater than maximum")))

(deftest validate-min-number
  (let [s {:type '"number" :minimum 2}]
    (is (validate s 3) "above minimum")
    (is (and (not (validate s 1)) (not (validate s 2)))
        "not above or equal minimum")))

(deftest validate-max-equal-number
  (let [s {:type '"number" :maximumCanEqual 5}]
    (is (and (validate s 5) (validate s 4))
        "should validate if is lower or equal maximum")
    (is (not (validate s 6))
        "should not validate if not lower nor equal maximum")))

(deftest validate-min-equal-number
  (let [s {:type '"number" :minimumCanEqual 2}]
    (is (and (validate s 3) (validate s 2))
        "should validate if is above or equal minimum")
    (is (not (validate s 1))
        "should not validate if not above nor equal minimum")))

(deftest validate-divisible-number
  (let [s {:type "number" :divisibleBy 2}]
    (is (validate s 4) "number is divisible by 2")
    (is (not (validate s 5)) "if number it not divisible by 2")))