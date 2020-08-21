(ns wb-file-generator.core
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.string :as string]
   [wb-file-generator.db.main :refer [datomic-conn]]
   [mount.core :as mount]
   [datomic.api :as d]
   [wb-file-generator.intermine :as intermine]))

(defn usage [options-summary]
  (->> ["This is for generating files from WormBase's Datomic database."
        ""
        "Usage: wb-file-generator [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  intermine    Generate Intermine Files"
        ""
        "Please refer to the manual page for more information."]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(def cli-options
  ;; An option with a required argument
  [["-v" nil "Verbosity level"
    :id :verbosity
    :default 0
    :update-fn inc] ; Prior to 0.4.1, you would have to use:
                   ;; :assoc-fn (fn [m k _] (update-in m [k] inc))
   ;; A boolean option defaulting to nil
   [nil "--dir DIR" "Output Directory"
    :id :dir
    :default "output"]
   ["-h" "--help"]])

(defn exit [status msg]
 (println msg)
 (System/exit status))

(defn validate-args [args]
 (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
  (cond
   (:help options) ; help => exit OK with usage summary
   {:exit-message (usage summary) :ok? true}

   errors ; errors => exit with description of errors
   {:exit-message (error-msg errors)}

   (and (= 1 (count arguments))
           (#{"intermine"} (first arguments)))
      {:action (first arguments) :options options}

   :else ; failed custom validation => exit with usage summary
   {:exit-message (usage summary)})))

;(defn datomic-uri []
; (environ/env :wb-db-uri))

(defn -main [& args]
 (let [output-filepath "output"]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
   (if exit-message
    (exit (if ok? 0 1) exit-message)
    (let [db (do (mount/start)
                 (d/db datomic-conn))]
     (do
      (case action
       "intermine" (intermine/generate-files options db))
      (mount/stop)
      (java.lang.System/exit 0)))))))
