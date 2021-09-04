(local utils (require "mp.utils"))

(fn maybe-open-gallery []
  (var url (mp.get_property "stream-open-filename"))
  (when (= (url:find "gallery://") 1)
    (set url (string.gsub url "gallery://" ""))
    (let [res (utils.subprocess {:args ["gallery-dl" "-g" url]})]
      (when (and (= res.status 0) res.stdout)
        (mp.commandv "loadlist" (.. "memory://" res.stdout))))))

(mp.add_hook "on_load" 15 maybe-open-gallery)
