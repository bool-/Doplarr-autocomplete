(ns doplarr.interaction-state-machine
  (:require
   [clojure.core.async :as a]
   [clojure.string :as str]
   [com.rpl.specter :as s]
   [discljord.messaging :as m]
   [doplarr.discord :as discord]
   [doplarr.state :as state]
   [doplarr.utils :as utils :refer [log-on-error]]
   [fmnoise.flow :refer [else then]]
   [taoensso.timbre :refer [fatal info]]))

(defn- create-response! [messaging id token response-data]
  (->> @(m/create-interaction-response! messaging id token 8 :data response-data)
       (else #(fatal % "Error in creating search responses"))))

(defn- handle-request! [interaction query season media-type]
  (a/go
    (let [id (:id interaction)
          token (:token interaction)
          {:keys [messaging]} @state/discord]
      (cond
        (and (or (not (string? query)) (not (str/blank? query))) (nil? season)) ; this is fuckin disgusting
        (let [results (->> (log-on-error
                            (a/<! ((utils/media-fn media-type "search") query media-type))
                            "Exception from search")
                           (then #(->> (take (:max-results @state/config discord/MAX-OPTIONS) %)
                                       (into []))))]
          (create-response! messaging id token (discord/search-response-autocomplete results)))

        (not (nil? season))
        (let [add-opts (log-on-error
                        (a/<! ((utils/media-fn media-type "additional-options") {:id query} media-type))
                        "Exception thrown from additional-options")
              seasons (:seasons add-opts)
              options-list (->> (map-indexed (fn [idx s]
                                               (if (zero? idx)
                                                 {:name (:name s) :value (:id s)}
                                                 {:name (str "Season " (:name s)) :value (:id s)})) seasons)
                                (filter #(str/includes? (str/lower-case (:name %)) (str/lower-case season)))
                                (then #(->> (take (:max-results @state/config discord/MAX-OPTIONS) %)
                                            (into []))))]
          (create-response! messaging id token {:choices options-list}))))))

(defn handle-application-command-autocomplete [interaction]
  (a/go
    (let [payload-opts (:options (:payload interaction))
          payload-name (:name (:payload interaction))
          media-type (first (keys payload-opts))
          query (s/select-one [media-type media-type] payload-opts)
          season (s/select-one [media-type :season] payload-opts)]
      (info "Performing search for" (name media-type) query)
      (when (= payload-name "request")
        (a/<! (handle-request! interaction query season media-type))))))

(defn handle-application-command [interaction]
  (a/go
    (let [uuid (str (java.util.UUID/randomUUID))
          id (:id interaction)
          token (:token interaction)
          payload-opts (:options (:payload interaction))
          media-type (first (keys payload-opts))
          query (s/select-one [media-type media-type] payload-opts)
          season (s/select-one [media-type :season] payload-opts)
          user-id (:user-id interaction)
          channel-id (:channel-id interaction)
          request-embed-payload {:id query :season season}
          embed (log-on-error
                 (a/<! ((utils/media-fn media-type "request-embed") request-embed-payload media-type))
                 "Exception from request-embed")
          add-opts (log-on-error
                    (a/<! ((utils/media-fn media-type "additional-options") {:id query} media-type)) ; TODO cache this
                    "Exception thrown from additional-options")
          payload (merge request-embed-payload add-opts)
          {:keys [messaging bot-id]} @state/discord]

                                        ; Send the ack for delayed response
      (->> @(m/create-interaction-response! messaging id token 5 :data {:flags 64})
           (else #(fatal % "Error in interaction ack")))
                                        ; Search for results
      (info "Performing request for" (name media-type) payload)

      (letfn [(msg-resp [msg] (->> @(m/edit-original-interaction-response! messaging bot-id token (discord/content-response msg))
                                   (else #(fatal % "Error in message response"))))]
        (->>  (log-on-error
               (a/<!! ((utils/media-fn media-type "request")
                       (assoc payload :discord-id user-id)
                       media-type))
               "Exception from request")
              (then (fn [status]
                      (case status
                        :unauthorized (msg-resp "You are unauthorized to perform this request in the configured backend")
                        :pending (msg-resp "This has already been requested and the request is pending")
                        :processing (msg-resp "This is currently processing and should be available soon!")
                        :available (msg-resp "This selection is already available!")
                        (do
                          (info "Performing request for " query)
                          (msg-resp "Request performed!")
                          (case (:discord/requested-msg-style @state/config)
                            :none nil
                            :embed (m/create-message! messaging channel-id (discord/request-performed-embed embed user-id))
                            (m/create-message! messaging channel-id (discord/request-performed-plain payload media-type user-id)))))))
              (else (fn [e]
                      (let [{:keys [status body] :as data} (ex-data e)]
                        (if (= status 403)
                          (->> @(m/edit-original-interaction-response! messaging bot-id token (discord/content-response (body "message")))
                               (else #(fatal % "Error in sending request failure response")))
                          (->> @(m/edit-original-interaction-response! messaging bot-id token (discord/content-response "Unspecified error on request, check logs"))
                               (then #(fatal "Non 403 error on request" % data))
                               (else #(fatal % "Error in sending error response"))))))))))))
