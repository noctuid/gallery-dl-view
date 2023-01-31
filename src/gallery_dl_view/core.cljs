;; major issues
;; - past certain range causes problems (no longer loads in correct order)
;; - need sliding range to go past fetch_count

;; * Namespace
(ns gallery-dl-view.core
  (:require [clojure.string :refer [blank? join replace split-lines starts-with?]])
  (:require [clojure.set :refer [difference union]])
  (:require [cognitect.transit :as transit])
  (:require [flatland.ordered.map :refer [ordered-map]])
  (:require-macros [gallery-dl-view.macros
                    :refer [info mp utils show-text subprocess-capture
                            subprocess-detached]]))

;; * Settings
(def default-settings
  (clj->js {:prefix            "gallery-dl://"
            :require_prefix    false
            :fetch_count       500
            :gallery_url_file  ""
            :visited_urls_file ""
            :ignore_visited    false

            :download_command "gallery-dl"
            ;; using shortopt because -d works for both aria and gallery-dl
            :download_args    ["-d" "."]
            :save_key         ""}))

(. (. js/mp -options) read-options default-settings "gallery-dl-view")

(defn settings
  "Return the setting value for keyword prop."
  [prop]
  (let [clj-settings (js->clj default-settings)]
    (clj-settings (name prop))))

;; * Constants
(def ^:const gdl-prefix (settings :prefix))

;; * State
(def start-index
  "The starting index of the current range of images to fetch."
  0)

(def gallery-url
  "The url of the opened gallery if any."
  nil)

(def gallery-metadata
  "An ordered map of image url to image metadata for the gallery.
  The map is ordered by insertion to allow downloading all opened images in
  gallery order.  Using a separate array with conj causes a huge delay in
  opening the gallery."
  (ordered-map))

(def store-urls
  "A set of urls to write to the visited urls file on exit."
  #{})

;; * Visited Urls Helpers
(defn read-file
  "Read text from a file."
  [fname]
  (utils read_file fname))

(def visited-urls
  "Set of previously visited gallery urls.
  Initialize from the visited urls file if it exists."
  (try (set (split-lines (read-file (settings :visited_urls_file))))
       (catch :default e
         ;; file doesn't exist
         #{})))

(defn write-file
  "Write text to a file."
  [file text]
  (let [fname (str "file://" file)]
    (utils write_file fname text)))

(defn append-file
  "Append text to a file."
  [file text]
  (let [fname (str "file://" file)]
    (utils append_file fname text)))

(defn url-previously-visited?
  "Return whether the current file has been previously visited.
  If the ignore_visited setting is false or the current image is not from a
  gallery, return false."
  []
  (let [url (mp get_property "path")]
    (and (settings :ignore_visited)
         (gallery-metadata url)
         (contains? visited-urls url))))

(defn write-visited-urls
  "Write store-urls to disk if the visited urls file option is set."
  []
  (when (and (not (blank? (settings :visited_urls_file)))
             (not (empty? store-urls)))
    (append-file (settings :visited_urls_file)
                 (str (join "\n" store-urls) "\n"))))

(defn maybe-mark-url-visited
  "If the current file is a gallery file, store it to be saved on exit.
  If the visited_urls_file setting is not set, no urls will be stored."
  []
  (let [url (mp get_property "path")]
    (when (gallery-metadata url)
      (set! store-urls (conj store-urls url)))))

(defn parse-gallery-dl-json
  "Parse a gallery-dl json dump.
  Return a list of urls and a map of urls to metadata."
  [json-dump]
  (let [entries
        (filter #(and (seq %)
                      ;; check that there are 3 items and 2nd is string/url
                      ;; ignore other items
                      (= (count %) 3)
                      (string? (% 1)))
                (transit/read (transit/reader :json) json-dump))
        urls     (map #(% 1) entries)
        metadata (into (ordered-map)
                       (map
                        ;; url to metadata
                        #(vector (% 1) (% 2))
                        entries))]
    [urls metadata]))

(defn current-range
  "Return the current range string to pass to gallery-dl."
  ([start]
   (let [end (+ start (settings :fetch_count))]
     (str start "-" end)))
  ([]
   (current-range start-index)))

;; * Keybinding Functions
(defn gallery-dl-download-url
  "Download the current gallery image."
  []
  (when gallery-url
    (let [url  (mp get_property "path")
          args (concat [(settings :download_command)]
                       (settings :download_args)
                       [url])]
      (subprocess-detached args)
      (show-text "Downloaded " url))))

(defn make-keybindings
  "Make keybindings based on user settings."
  []
  (info "Setting this key to save media: " (settings :save_key))
  (when-not (blank? (settings :save_key))
    (mp add_forced_key_binding
        (settings :save_key)
        "gallery-dl-download-url"
        gallery-dl-download-url)))

;; * Gallery Opening/Main
(defn add-images
  "Add images to the playlist."
  [urls metadata]
  (set! gallery-metadata (into gallery-metadata metadata))
  ;; should be empty already; add first image that will be immediately displayed
  (set! store-urls (conj store-urls (first urls)))
  (mp commandv "loadlist"
      (str "memory://" (join "\n" urls))))

(defn load-next-range
  "Load the next range of media files for the current gallery."
  ([gallery-data]
   (let [status          (gallery-data "status")
         stdout          (gallery-data "stdout")
         [urls metadata] (parse-gallery-dl-json stdout)]

     (cond (not= status 0)
           (info "Failed to retrieve image urls with error "
                 (gallery-data "stderr"))

           ;; --range past max will just return nothing
           (empty? urls)
           ;; TODO store that shouldn't keep trying (should be reset on gallery
           ;; load; make helper function for resetting this, start index, etc.)
           (info "No more images in gallery to load")

           :else
           (do
             (let [new-urls     (remove visited-urls urls)
                   new-metadata (apply dissoc metadata visited-urls)
                   range        (current-range)]
               (set! start-index (+ (settings :fetch_count) 1))
               (if (seq new-urls)
                 (do
                   (info "Loading new gallery urls in range " range)
                   (add-images new-urls new-metadata))
                 (do
                   ;; TODO what happens if this is the first time? should make
                   ;; synchronous so won't exit?
                   (info "No new urls in gallery range " range)
                   (load-next-range))))))))
  ([]
   ;; TODO use async callback
   (load-next-range (subprocess-capture
                     ["gallery-dl"
                      "--range" (current-range)
                      "--dump-json" gallery-url]))))

(defn open-gallery
  "Open url as a gallery-dl supported gallery, removing gallery-dl:// prefix.
  If load-failed? is true, try to open url as-is if possible."
  [url load-failed?]
  (let [real-url (if load-failed?
                   url
                   (replace url gdl-prefix ""))
        res      (subprocess-capture
                  ["gallery-dl"
                   "--range" (current-range 0)
                   "--dump-json" real-url])]
    (if (= (res "status") 0)
      (do
        (set! gallery-url url)
        (set! start-index 0)
        (info "Opening gallery " real-url)
        (make-keybindings)
        (when-not (blank? (settings :gallery_url_file))
          (write-file (settings :gallery_url_file) real-url))
        (load-next-range res))
      (info "Failed to open gallery " real-url
            " with error " (res "stderr")))))

(defn maybe-open-gallery
  "Open the current filename using gallery-dl if it starts with gallery-dl://
  This is meant to be used with the on_load hook."
  [_hook]
  (let [url (mp get_property "stream-open-filename")]
    (cond (starts-with? url gdl-prefix)
          (open-gallery url false)

          (url-previously-visited?)
          (mp commandv "playlist-remove" (mp get_property "playlist-pos"))

          :else
          (maybe-mark-url-visited))))

(defn maybe-open-gallery-on-failure
  "Attempt to open the current filename with gallery-dl.
  This is meant to be used with the on_load_fail hook."
  [_hook]
  (let [url (mp get_property "stream-open-filename")]
    (info "Mpv failed to recognize " url)
    (info "Attempting to open with gallery-dl")
    (open-gallery (mp get_property "stream-open-filename") true)))

(defn init
  "Entrypoint.
  Add hook to load files that begin with gallery-dl:// as galleries."
  []
  (mp add_hook "on_load" 50 maybe-open-gallery)
  (when-not (settings :require_prefix)
    (mp add_hook "on_load_fail" 50 maybe-open-gallery-on-failure))
  (mp register_event "shutdown" write-visited-urls))
