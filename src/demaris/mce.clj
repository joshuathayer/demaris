(ns damaris.mce
  (:require
   [clojure.tools.trace :as trace]))


(declare evl)

;; ===== env ======

(def primitive-procedures
  (list '+ +
        '- -
        'first first
        'second second
        'third #(nth % 2)
        'get get
        'print println
        'cons cons
        ))

(defn make-primitive-procedure-object [p]
  {:proc-type :primitive
   :proc      p})

(defn make-env []
  {:symbols {}
   :enclosing nil})

(defn add-to-env [env sym val]
  (assoc-in env [:symbols sym] val))

(defn enclose-env [env enclosing]
  (assoc env :enclosing enclosing))

(defn lookup-symbol [exp env]
  (if-let [val (get (:symbols env) exp)]
    val
    (if-let [enclosing (:enclosing env)]
      (lookup-symbol exp enclosing)
      nil)))

(defn setup-env []
  (reduce (fn [e [s v]] (add-to-env e s (make-primitive-procedure-object v)))
          (make-env)
          (partition 2 primitive-procedures)))

;; ===== preds =====

(defn tagged-list? [exp tag]
  (and (list? exp) (= (first exp) tag)))

(defn self-eval? [exp]
  (cond (number? exp) true
        (string? exp) true
        (boolean? exp) true
        (keyword? exp) true
        :else false))

(defn lambda? [exp] (tagged-list? exp 'lambda))
(defn let? [exp] (tagged-list? exp 'let))

(defn application? [exp] (list? exp))

;; =====

(defn make-procedure [parameters body env]
  {:proc-type :compound
   :params    parameters
   :body      body
   :env       env})

(defn handle-let [let-forms body env]
  (let [ext-env  (reduce (fn [e [let-sym let-body]]
                           (add-to-env e let-sym (evl let-body e)))
                         (enclose-env (make-env) env)
                         (partition 2 let-forms))]
    (evl body ext-env)))

;; ===== apply =====
(defn app [procedure args]
  (case (:proc-type procedure)
    :primitive (apply (:proc procedure) args)
    :compound  (first (evl (:body procedure)
                           (enclose-env
                            (reduce (fn [e [s v]] (add-to-env e s v))
                                    (make-env)
                                    (map vector (:params procedure) args))
                            (:env procedure))))
    nil))

;; ===== eval =====

(defn evl [exp env]
  (cond
    (self-eval? exp) (list exp env)
    (symbol? exp)    (list (lookup-symbol exp env) env)
    (lambda? exp)    (list (make-procedure (nth exp 1) (nth exp 2) env) env)
    (let? exp)       (list (handle-let (nth exp 1) (nth exp 2) env) env)
    (vector? exp)    (list (vec (map #(evl % env) exp)) env)
    (map? exp)       (list (reduce-kv (fn [m k v] (assoc m (evl k env) (evl v env))) {} exp)
                           env)

    ;; is this otherwise a list?
    (application? exp) (app (first (evl (first exp) env))
                            (map #(evl % env) (rest exp)))

    ))



(comment
  (first (evl 1 (setup-env)))

  (first (evl '+ (setup-env)))

  (evl '(lambda [x] x) (setup-env))
  (evl '((lambda [x] x) 1) (setup-env))


  (evl '(cons :a '()) (setup-env))
  (evl '(print "hello") (setup-env))

  (evl '((get {:hi (lambda [who] (print "hello" who))} :hi) "world") (setup-env))

  (evl '(let [x 1 b 2] (+ x b)) (setup-env))

  (evl '(let [x 1 b 2] x) (setup-env))


  (evl '(let [x 1 b (+ x 9)] (+ x b)) (setup-env))

  (evl '(let [a {:a (+ 2 9)}] (get a :a)) (setup-env))

  (evl '[1 2 3 :a (+ 3 4)] (setup-env))

  (evl '{:a 1 :b (+ 2 3)} (setup-env))

  (evl '(get {:a 1 :b (+ 2 3)} :b) (setup-env))

  (evl '(third [:a :b :c]) (setup-env))

  (evl '(lambda [x] (+ x y)) (setup-env))

  (evl '((lambda [x y] (+ x y)) 1 3) (setup-env))

  (evl '((lambda [x y] (+ x y)) (+ 1 2) (- 4 5)) (setup-env))

  (evl :a (setup-env))

  (app (evl '+ (setup-env)) [1 2])

  (evl '(+ 1 2) (setup-env))

  (let [inner (-> (make-env) (add-to-env 'a 1))
        outer (-> (make-env) (enclose-env inner))]
    (evl 'a outer))

  )
