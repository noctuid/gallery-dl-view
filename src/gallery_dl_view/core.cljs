;; * Namespace
(ns gallery-dl-view.core
  (:require [clojure.string :refer [blank? join replace split-lines starts-with?]])
  (:require [cognitect.transit :as transit])
  (:require [flatland.ordered.map :refer [ordered-map]])
  (:require-macros [gallery-dl-view.macros
                    :refer [info mp utils show-text subprocess-capture
                            subprocess-detached]]))

;; * Settings
(def default-settings
  (clj->js {:prefix            "gallery-dl://"
            :require_prefix    false
            :fetch_count       200
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
  1)

(def previous-playlist-pos
  "The previous playlist position.
  Used to determine whether to go to the next range or the prior range (when
  looping around to last playlist index from first)."
  -1)

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
       (catch :default _
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
             (seq store-urls))
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
     (str start "-" (- end 1))))
  ([]
   (current-range start-index)))

(defn update-range
  "Increment or decrement the start index for the current gallery range."
  ([decrement?]
   (set! start-index
         (if decrement?
           (max 1 (- start-index (settings :fetch_count)))
           (+ start-index (settings :fetch_count)))))
  ([]
   (update-range false)))

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

(defn load-range
  "Load the current range of media files for the current gallery."
  ([gallery-data]
   ;; using loop to prevent stack overflow (which happens in under 10 recursions
   ;; it looks like); also want to be synchronous
   (loop [gallery-data gallery-data]
     (if (not= (gallery-data "status") 0)
       (info "Failed to retrieve image urls with error "
             (gallery-data "stderr"))
       (let [[urls metadata] (parse-gallery-dl-json (gallery-data "stdout"))]
         ;; --range past max will just return nothing
         (if (empty? urls)
           (info "No more images in gallery to load")
           (let [new-urls     (if (settings :ignore_visited)
                                (remove visited-urls urls)
                                urls)
                 new-metadata (apply dissoc metadata visited-urls)
                 range        (current-range)]
             (if (seq new-urls)
               (do
                 (info "Loading new gallery urls in range " range)
                 (add-images new-urls new-metadata))
               (do
                 (info "No new urls in gallery range " range)
                 (update-range)
                 ;; TODO maybe add max index setting (would need to be a very
                 ;; large gallery to be a problem)
                 (recur (subprocess-capture
                         ["gallery-dl"
                          "--range" (current-range)
                          "--dump-json" gallery-url]))))))))))
  ([]
   (load-range (subprocess-capture
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
                   "--range" (current-range 1)
                   "--dump-json" real-url])]
    (if (= (res "status") 0)
      (do
        (set! gallery-url real-url)
        (set! start-index 1)
        (info "Opening gallery " real-url)
        (make-keybindings)
        (when-not (blank? (settings :gallery_url_file))
          (write-file (settings :gallery_url_file) real-url))
        (load-range res))
      (info "Failed to open gallery " real-url
            " with error " (res "stderr")))))

(defn get-max-playlist-pos
  "Return the maximum playlist position."
  []
  (- (js/parseInt (mp get_property "playlist-count")) 1))

(defn maybe-open-gallery
  "Open the current filename using gallery-dl if it starts with gallery-dl://
  This is meant to be used with the on_load hook."
  [_hook]
  (let [url (mp get_property "stream-open-filename")]
    (cond (starts-with? url gdl-prefix)
          (open-gallery url false)

          ;; TODO confirm this case no longer happens (filtering in
          ;; load-next-range)
          (url-previously-visited?)
          (mp commandv "playlist-remove" (mp get_property "playlist-pos"))

          :else
          (do
            (maybe-mark-url-visited)
            (let [pos     (js/parseInt (mp get_property "playlist-pos"))
                  max-pos (get-max-playlist-pos)]
              ;; update range when going beyond first or last image
              ;; TODO fix edge case when there are only 2 images in the range
              ;; and you go right (will go to previous range)
              (cond (and (= pos max-pos)
                         (= previous-playlist-pos 0)
                         (not (= start-index 1)))
                    (do (update-range true)
                        (load-range)
                        (mp set_property "playlist-pos"
                            (str (get-max-playlist-pos))))

                    (and (= pos 0)
                         (= previous-playlist-pos max-pos))
                    (do (update-range)
                        (load-range)))
              (set! previous-playlist-pos pos))))))

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
