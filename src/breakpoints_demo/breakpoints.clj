(ns breakpoints-demo.breakpoints)

(def ^:private foo "foo")

(defmacro print-env [] (print &env))

(defmacro local-context []
  (let [local-symbols (keys &env)]
    (zipmap
      (map (fn [sym] `'~sym) local-symbols)
      local-symbols)))

(declare ^:dynamic *locals*)

(defn contextual-eval
  [locals expr]
  (binding [*locals* locals]
    (eval
      `(let
         ~(vec (mapcat #(list % `(*locals* '~%)) (keys *locals*)))
         ~expr))))

(defn better-contextual-eval
  [locals ns expr]
  (binding [*locals* locals
            *ns* ns]
    (eval
      `(let
         ~(vec (mapcat #(list % `(*locals* '~%)) (keys *locals*)))
         ~expr))))

(defn my-read
  [request-prompt request-exit]
  (let [input (clojure.main/repl-read request-prompt request-exit)]
    (if (= input :exit)
      request-exit
      input)))

(defmacro break []
  `(clojure.main/repl
     :prompt #(print "debug=> ")
     :read my-read
     :eval (partial better-contextual-eval (local-context) ~*ns*)))

(defn test-break [arg]
  (let [x 4 y 5]
    (break)))