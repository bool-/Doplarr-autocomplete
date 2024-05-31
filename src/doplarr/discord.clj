(ns doplarr.discord
  (:require
   [com.rpl.specter :as s]
   [discljord.messaging :as m]
   [fmnoise.flow :as flow :refer [else]]
   [taoensso.timbre :refer [fatal]]))

(defn request-command [media-types]
  {:name "request"
   :description "Request media"
   :options
   (into [] (for [media media-types]
              (let [base-options [{:type 4 ;; 3 is string, 4 is number
                                   :name (name media)
                                   :description "Query"
                                   :required true
                                   :autocomplete true}]]
                {:type 1
                 :name (name media)
                 :description (str "Request " (name media))
                 :options (if (= (name media) "series")
                            (conj base-options {:type 4 ;; 3 is string, 4 is number
                                                :name "season"
                                                :description "Query"
                                                :required true
                                                :autocomplete true})
                            base-options)})))})

(defn content-response [content]
  {:content content
   :flags 64
   :embeds []
   :components []})

(def interaction-types {1 :ping
                        2 :application-command
                        3 :message-component
                        4 :application-command-autocomplete})

(def component-types {1 :action-row
                      2 :button
                      3 :select-menu})

(def MAX-OPTIONS 25)
(def MAX-CHARACTERS 100)

(def request-thumbnail
  {:series "https://thetvdb.com/images/logo.png"
   :movie "https://i.imgur.com/44ueTES.png"})

(defn application-command-interaction-option-data [app-com-int-opt]
  [(keyword (:name app-com-int-opt))
   (into {} (map (juxt (comp keyword :name) :value)) (:options app-com-int-opt))])

(defn interaction-data [interaction]
  {:id (:id interaction)
   :type (interaction-types (:type interaction))
   :token (:token interaction)
   :user-id (s/select-one [:member :user :id] interaction)
   :channel-id (:channel-id interaction)
   :payload
   {:component-type (component-types (get-in interaction [:data :component-type]))
    :component-id (s/select-one [:data :custom-id] interaction)
    :name (s/select-one [:data :name] interaction)
    :values (s/select-one [:data :values] interaction)
    :options (into {} (map application-command-interaction-option-data) (get-in interaction [:data :options]))}})


(defn autocomplete-option [result]
  {:name (apply str (take MAX-CHARACTERS (str (or (:title result) (:name result)) " (" (:year result) ")")))
   :value (:id result)})

(defn search-response-autocomplete [results] 
  (let [choices (map autocomplete-option results)]
    {:choices choices}))

(defn request-embed [{:keys [media-type title overview poster season quality-profile language-profile rootfolder]}]
  {:title title
   :description overview
   :image {:url poster}
   :thumbnail {:url (media-type request-thumbnail)}
   :fields (filterv
            identity
            ; Some overrides to make things pretty
            [(when quality-profile
               {:name "Profile"
                :value quality-profile})
             (when language-profile
               {:name "Language Profile"
                :value language-profile})
             (when season
               {:name "Season"
                :value (if (= season -1) "All" season)})
             (when rootfolder
               {:name "Root Folder"
                :value rootfolder})])})

(defn request-performed-plain [payload media-type user-id]
  {:content
   (str "<@" user-id "> your request for the "
        (name media-type) " `" (:title payload)
        "` has been received!")})

(defn request-performed-embed [embed-data user-id]
  {:content (str "<@" user-id "> has requested:")
   :embeds [(request-embed embed-data)]})

;; Discljord Utilities
(defn register-commands [media-types bot-id messaging guild-id]
  (->> @(m/bulk-overwrite-guild-application-commands!
         messaging bot-id guild-id
         [(request-command media-types)])
       (else #(fatal % "Error in registering commands"))))
