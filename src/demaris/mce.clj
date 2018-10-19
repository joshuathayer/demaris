(ns damaris.mce
  (:require
   [clojure.tools.trace :as trace]
   [clojure.java.io :as io]
   [clojure.edn :as edn]))


(declare evl evl-seq handle-map app)

;; ===== env ======

(def primitive-procedures
  (list '+ +
        '- -
        '/ /
        '* *
        '= =
        'first first
        'second second
        'third #(nth % 2)
        'get get
        'print println
        'cons cons))

(defn make-primitive-procedure-object [p]
  {:proc-type :primitive
   :proc      p})

(defn make-env []
  {:symbols   {}
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
        (nil? exp) true
        (boolean? exp) true
        :else false))

(defn lambda? [exp] (tagged-list? exp 'lambda))
(defn let? [exp] (tagged-list? exp 'let))
(defn define? [exp] (tagged-list? exp 'define))
(defn map-meta? [exp] (tagged-list? exp 'map))
(defn reduce? [exp] (tagged-list? exp 'reduce))
(defn do? [exp] (tagged-list? exp 'do))
(defn if? [exp] (tagged-list? exp 'if))
(defn application? [exp] (list? exp))

;; =====

(defn make-procedure [parameters body]
  {:proc-type :compound
   :params    parameters
   :body      body})

(defn handle-let [let-forms exps env]
  (let [ext-env  (reduce (fn [e [let-sym let-body]]
                           (add-to-env e let-sym (first (evl let-body e))))
                         (enclose-env (make-env) env)
                         (partition 2 let-forms))]
    (first (evl-seq exps ext-env))))

;; map f over items in s, using environment env. i don't think
;; environment modifications in earlier applications should affect
;; later applications
(defn handle-map
  [f s env]
  (if (empty? s) '()
      (cons (app f [(first s)] env)
            (handle-map f (rest s) env))))

(defn handle-reduce
  [f i s env]
  (if (empty? s)
    i
    (handle-reduce
     f
     (app f [i (first s)] env)
     (rest s)
     env)))

(defn handle-define [param body env]
  (let [[res new-env] (evl body env)]
    (add-to-env new-env param res)))

(defn handle-if [[test t f] env]
  (if (first (evl test env))
    (evl t env)
    (evl f env)))

;; ===== apply =====
(defn app [procedure args env]
  (case (:proc-type procedure)
    :primitive (apply (:proc procedure) args)
    :compound  (first (evl (:body procedure)
                           (enclose-env
                            (reduce (fn [e [s v]] (add-to-env e s v))
                                    (make-env)
                                    (map vector (:params procedure) args))
                            env)))
    nil))

;; ===== eval =====

(defn evl-seq [exps env]
  (reduce
   (fn [[res current-env] exp]
     (evl exp current-env))
   [nil env]
   exps))

(defn evl [exp env]
  (cond
    (self-eval? exp) (list exp env)
    (symbol?    exp) (list (lookup-symbol exp env) env)
    (lambda?    exp) (list (make-procedure (nth exp 1) (nth exp 2)) env)
    (let?       exp) (list (handle-let (nth exp 1) (rest (rest exp)) env) env)
    (vector?    exp) (list (vec (map #(first (evl % env)) exp)) env)
    (define?    exp) (list nil (handle-define (nth exp 1) (nth exp 2) env))
    (map?       exp) (list (reduce-kv (fn [m k v] (assoc m (first (evl k env))
                                                         (first (evl v env))))
                                      {} exp)
                           env)
    (do?        exp) (evl-seq exp env)
    (if?        exp) (handle-if (rest exp) env)

    ;; (map ...)
    (map-meta?  exp) (list (handle-map (first (evl (nth exp 1) env))
                                       (first (evl (nth exp 2) env))
                                       env)
                           env)

    (reduce? exp)    (list (handle-reduce (first (evl (nth exp 1) env))
                                          (first (evl (nth exp 2) env))
                                          (first (evl (nth exp 3) env))
                                          env)
                           env)

    ;; is this otherwise a list?
    (application? exp) (list (app (first (evl (first exp) env))
                                  (map #(first (evl % env)) (rest exp))
                                  env)
                             env)

    ))


(defn evl-file [fn]
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader fn))]
    (let [edn-seq (repeatedly (partial edn/read {:eof :eof} in))]
      (evl-seq (take-while (partial not= :eof) edn-seq) (setup-env)))))


(comment
  (evl '(if false 1 2) (setup-env))

  (evl '(map
         (lambda [x]
                 (do (print "hi") (+ x 2)))
         [1 2 3])
       (setup-env))

  (evl '(reduce
         (lambda [acc x]
                 (+ acc x))
         0
         [1 2 3])
       (setup-env))

  (evl-file "/home/user/projects/dem/src/demaris/feh.dsc")

  (first (evl-seq '((define x 1) (define y 2) (+ x y)) (setup-env)))
  (first (evl '(do (define x 1) (define y 2) (+ x y)) (setup-env)))

  (evl-seq
   '((define plus
       (lambda [x] (+ x 2)))

     (define minus
       (lambda [x] (- x 8)))

     (minus (plus 16)))
   (setup-env))

  (evl-seq
   '((define fact
       (lambda [x]
               (if (= x 0)
                 1
                 (* x (fact (- x 1))))))
     (fact 12))
   (setup-env))

  (evl 1 (setup-env))

  (evl '+ (setup-env))

  (def new-env (second (evl '(define x 99) (setup-env))))

  (evl 'x new-env)

  (evl '(+ x 3) new-env)

  (evl '(lambda [x] x) (setup-env))
  (evl '((lambda [x] x) 1) (setup-env))

  (evl '(cons :a '()) (setup-env))
  (evl '(print "hello") (setup-env))

  (evl '((get {:hi (lambda [who] (print "hello" who))} :hi) "world") (setup-env))


  (evl '(let [x 1 b 2] (+ x b)) (setup-env))

  (evl '(let [x 1 b 2] x) (setup-env))

  (evl '[1 2 3] (setup-env))

  (evl '[1 2 3 :a (+ 3 4)] (setup-env))

  (first (evl '(let [x 1 b (+ x 9)] (+ x b) (- x b)) (setup-env)))
  (first (evl '(let [a {:a (+ 2 9)}] (get a :a)) (setup-env)))

  (evl '{:a 1 :b (+ 2 3)} (setup-env))
  (first (evl '{:a 1 :b (+ 2 3)} (setup-env)))

  (evl '(get {:a 1 :b (+ 2 3)} :b) (setup-env))

  (evl '(third [:a :b :c]) (setup-env))

  (evl '(lambda [x] (+ x y)) (setup-env))

  (evl '((lambda [x y] (+ x y)) 1 3) (setup-env))

  (evl '((lambda [x y] (+ x y)) (+ 1 2) (- 4 5)) (setup-env))

  (evl :a (setup-env))

  ;; (app (evl '+ (setup-env)) [1 2] (setup-env))

  (evl '(+ 1 2) (setup-env))

  (let [inner (-> (make-env) (add-to-env 'a 1))
        outer (-> (make-env) (enclose-env inner))]
    (evl 'a outer))

  )
