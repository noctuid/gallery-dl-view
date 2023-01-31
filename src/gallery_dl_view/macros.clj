(ns gallery-dl-view.macros)

(defmacro mp
  "Run an mp function with args."
  [fun & args]
  `(. js/mp ~fun ~@args))

(defmacro utils
  "Run an mp.utils function with args."
  [fun & args]
  `(. (. js/mp -utils) ~fun ~@args))

(defmacro msg
  "Run an mp.msg with level and message."
  [level message]
  `(. (. js/mp -msg) ~level ~message))

(defmacro info
  "Run mp.msg.info with message."
  [& args]
  `(msg ~'info (str ~@args)))

(defmacro show-text
  "Print text on mpv GUI."
  [& args]
  `(mp ~'commandv "show-text" (str ~@args)))

(defmacro subprocess
  "Run subprocess command with given args and options.
  Arguments and options will automatically be converted to js objects, and the
  result will be returned as a clojure object."
  [args & options]
  `(cljs.core/js->clj
    (mp ~'command_native
        (cljs.core/clj->js {:name     "subprocess"
                            :args     ~args
                            ~@options ~@[]}))))

(defmacro subprocess-capture
  "Same as subprocess but capture stdout.
  Set the playback_only option to false and capture_stdout to true."
  [args & options]
  `(subprocess ~args
               :playback_only false
               :capture_stdout true
               ~@options))

(defmacro subprocess-detached
  "Same as subprocess but run detached.
  Set the playback_only option to false and detach to true."
  [args & options]
  `(subprocess ~args
               :playback_only false
               :detach true
               ~@options))
