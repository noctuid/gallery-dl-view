(ns gallery-dl-view.core
  (:require [clojure.string :refer [replace starts-with?]])
  (:require-macros [gallery-dl-view.macros
                    :refer [info mp utils subprocess-capture]]))

;; TODO give download command as a array?
(def default-settings
  (clj->js {:out_dir "."
            :download_command "gallery-dl"
            ;; using shortopt because -d for both aria and gallery-dl
            :dir_flag "-d"
            ;; TODO
            ;; :download-cmd-opts []
            }))

(. (. js/mp -options) read-options default-settings "gallery-dl-view")

(defn settings
  "Return the setting value for keyword prop."
  [prop]
  (let [clj-settings (js->clj default-settings)]
    (clj-settings (name prop))))

(defn gallery-dl-download-url
  "Download the current gallery image to settings outdir."
  []
  (let [url (mp get_property "path")]
    (mp commandv
        "run"
        (settings :download_command)
        (settings :dir_flag)
        (settings :out_dir)
        url)))

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
  (mp add_key_binding nil "gallery-dl-download-url" gallery-dl-download-url)
  (mp add_hook "on_load" 50 maybe-open-gallery))
