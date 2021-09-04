(ns gallery-dl-view.core
  (:require [clojure.string :refer [replace starts-with?]])
  (:require-macros [gallery-dl-view.macros
                    :refer [info mp utils subprocess-capture]]))

(defn maybe-open-gallery
  "Open the current filename using gallery-dl if it starts with gallery-dl://."
  []
  (let [url (mp get_property "stream-open-filename")
        gdl-prefix "gallery-dl://"]
    (when (starts-with? url gdl-prefix)
      (let [real-url (replace url gdl-prefix "")
            res (subprocess-capture ["gallery-dl" "-g" real-url])]
        (when (and (= (res "status") 0) (res "stdout"))
          (info (str "Opening gallery " real-url))
          (mp commandv "loadlist" (str "memory://" (res "stdout"))))))))

(defn init
  "Entrypoint.
  Add hook to load files that begin with gallery-dl:// as galleries."
  []
  (mp add_hook "on_load" 50 maybe-open-gallery))
