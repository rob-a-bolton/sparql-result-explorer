(ns sparql-result-explorer.core
  (:require [cheshire.core :refer [parse-stream]]
            [cljfx.api :as fx]
            [cljfx.ext.list-view :as fx-ext-list]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [java-time :as time])
  (:import [javafx.stage FileChooser]
           [javafx.scene Node])
  (:gen-class))


(def xmls-string "http://www.w3.org/2001/XMLSchema#string")
(def xmls-integer "http://www.w3.org/2001/XMLSchema#integer")
(def xmls-boolean "http://www.w3.org/2001/XMLSchema#boolean")
(def xmls-decimal "http://www.w3.org/2001/XMLSchema#decimal")
(def xmls-float "http://www.w3.org/2001/XMLSchema#float")
(def xmls-double "http://www.w3.org/2001/XMLSchema#double")
(def xmls-duration "http://www.w3.org/2001/XMLSchema#duration")
(def xmls-dateTime "http://www.w3.org/2001/XMLSchema#dateTime")
(def xmls-time "http://www.w3.org/2001/XMLSchema#time")
(def xmls-date "http://www.w3.org/2001/XMLSchema#date")

(def dataset-ops
  {:union set/union
   :difference set/difference
   :intersection set/intersection})

(s/def :binding/datatype
  #{xmls-string
    xmls-integer
    xmls-boolean
    xmls-decimal
    xmls-float
    xmls-double
    xmls-duration
    xmls-dateTime
    xmls-time
    xmls-date})
(s/def :binding/type #{"literal" "uri"})
(s/def :binding/value string?)
(s/def :data/binding (s/keys :req-un [:binding/type :binding/value]
                             :opt-un [:binding/datatype]))
(s/def :data/row (s/map-of keyword? :data/binding))
(s/def :data/bindings (s/coll-of :data/row))
(s/def :data/cols (s/coll-of keyword? :kind set?))
(s/def :data/dataview (s/keys :req-un [:data/bindings :data/cols]))
(s/def :data/operation (set (keys dataset-ops)))

(def *state
  (atom {:datasets {}
         :dataset-selection []
         :data-operation :union
         :dataview nil}))

(defn read-json
  [f]
  (with-open [r (io/reader f)]
    (parse-stream r true)))

(defn read-dataset
  [file]
  {:post [(s/valid? :data/dataview %)]}
  (let [{{cols :vars} :head {data :bindings} :results} (read-json file)]
    {:name (.getName file)
     :file file
     :cols (set (map keyword cols))
     :bindings data}))

(defn load-dataset!
  [file]
  (let [dataset (read-dataset file)]
    (swap! *state assoc-in [:datasets (:name dataset)] dataset)))

(defn datasets->dataview
  [datasets operation]
  {:pre [(s/valid? (s/coll-of :data/dataview) datasets)
         (s/valid? :data/operation operation)]
   :post [(s/valid? :data/dataview %)]}
  (if (and (seq datasets) (apply = (map :cols datasets)))
    {:cols (:cols (first datasets))
     :bindings (apply (operation dataset-ops)
                      (map (comp set :bindings) datasets))}
    {:cols #{}
     :bindings []}))


;; TODO: Try/Catch and show err dialog on failure to combine data
(defn rebuild-dataview!
  []
  (let [op (:data-operation @*state)
        datasets (:dataset-selection @*state)]
    (if-let [dataview (datasets->dataview datasets op)]
      (swap! *state assoc :dataview dataview)
      (println "Bad data"))))

(defmulti event-handler :event/type)

(defmethod event-handler ::load-file [e]
  (let [window (.getWindow (.getScene ^Node (.getTarget (:fx/event e))))
        chooser (doto (FileChooser.)
                  (.setTitle "Open Flile"))]
    (when-let [file (.showOpenDialog chooser window)]
      (load-dataset! file))))

(defmethod event-handler ::radio-event [e]
  (swap! *state assoc :data-operation (:option e))
  (rebuild-dataview!))

(defmethod event-handler ::select-multiple-datasets [e]
  (swap! *state assoc :dataset-selection (:fx/event e))
  (rebuild-dataview!))

;; Define render functions

(defn coerce-bind-type
  [{:keys [type datatype value]}]
  (cond
    (= "uri" type) [:uri value]
    (= datatype xmls-integer) [:int (Integer/valueOf value)]
    (= datatype xmls-boolean) [:bool (Boolean/valueOf value)]
    (= datatype xmls-decimal) [:double (Double/valueOf value)]
    (= datatype xmls-float) [:float (Float/valueOf value)]
    (= datatype xmls-double) [:double (Double/valueOf value)]
    (= datatype xmls-duration) [:string (time/duration value)]
    (= datatype xmls-dateTime) [:string (time/local-date-time value)]
    (= datatype xmls-time) [:string (time/local-time value)]
    (= datatype xmls-date) [:string (time/local-date value)]
    :else [:string value]))

(defn render-binding
  [data]
  (let [[data-type value] (coerce-bind-type data)]
    (case data-type
      {:text (str value)})))

;; TODO: Generate table once upon loading dataset via list
;; TODO: Set column type for better sorting
;; TODO: Styling based on row, col, cell
;; TODO: profile mk-table
;; TODO: Per-cell selection detection for clipboard projection

(defn dataview-table
  [{:keys [dataview]}]
  {:fx/type :table-view
   :row-factory
   {:fx/cell-type :table-row
    ;; full row of bindings received,
    ;; style accordingly perhaps
    :describe (fn [_] {:style {}})}
   :columns
   (or (map (fn [v]
              {:fx/type :table-column
               :text (str v)
               :cell-value-factory #(get % v)
               :cell-factory {:fx/cell-type :table-cell
                              :describe render-binding}})
            (:cols dataview))
       [])
   :items (or (:bindings dataview) [])})

(defn data-list-view [{:keys [items]}]
  {:fx/type fx-ext-list/with-selection-props
   :props {:selection-mode :multiple
           :on-selected-items-changed {:event/type ::select-multiple-datasets}}
   :desc {:fx/type :list-view
          :cell-factory {:fx/cell-type :list-cell
                         :describe (fn [ds] {:text (str (:file ds))})}
          :items items}})

(defn operation-radio-group [{:keys [options value]}]
  {:fx/type fx/ext-let-refs
   :refs {::toggle-group {:fx/type :toggle-group}}
   :desc {:fx/type :h-box
          :padding 20
          :spacing 10
          :children (for [option options]
                      {:fx/type :radio-button
                       :toggle-group {:fx/type fx/ext-get-ref
                                      :ref ::toggle-group}
                       :selected (= option value)
                       :text (str option)
                       :on-action {:event/type ::radio-event
                                   :option option}})}})

(defn dataset-manager [{:keys [datasets dataview]}]
  {:fx/type :h-box
   :children
   [{:fx/type data-list-view
     :h-box/hgrow :always
     :items (map second (seq datasets))}
    {:fx/type :v-box
     :children
     [{:fx/type :label
       :text "Operation"}
      {:fx/type operation-radio-group
       :options (keys dataset-ops)
       :value :union}
      {:fx/type :button
       :text "Open..."
       :on-action {:event/type ::load-file}}
      {:fx/type :label
       :text (format "%d records" (count (:bindings dataview)))}]}]})

(defn root [{:keys [datasets dataview]}]
  {:fx/type :stage
   :showing true
   :title "SPARQL Result Explorer"
   :on-hidden (fn [_] (System/exit 0))
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :children [{:fx/type dataset-manager
                              :datasets datasets
                              :dataview dataview}
                             {:fx/type dataview-table
                              :dataview dataview}]}}})

;; Create renderer with middleware that maps incoming data - description -
;; to component description that can be used to render JavaFX state.
;; Here description is just passed as an argument to function component.

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler event-handler}))

(defn -main
  [& _args]
  (fx/mount-renderer *state renderer))