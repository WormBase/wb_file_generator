(ns wb-file-generator.intermine
  (:require
   [clojure.string :as string]
   [clojure.data.xml :as xml]
   [datomic.api :as d]
   [clojure.java.io :as io]))

(def q-anatomy-term
  '[:find  [?at ...]
    :in $
    :where [?at :anatomy-term/id]])

(def q-rnai
  '[:find  [?rnai ...]
    :in $
    :where [?rnai :rnai/id]])

(def q-cds
  '[:find  [?cds ...]
    :in $
    :where [?cds :cds/id]])

(def q-expression-cluster
  '[:find  [?expression-cluster ...]
    :in $
    :where [?expression-cluster :expression-cluster/id]])

(def q-expression-pattern
  '[:find  [?expr-pattern ...]
    :in $
    :where [?expr-pattern :expr-pattern/id]])

(def q-laboratory
  '[:find  [?lab ...]
    :in $
    :where [?lab :laboratory/id]])

(def q-life-stage
  '[:find  [?ls ...]
    :in $
    :where [?ls :life-stage/id]])

(def q-phenotype
  '[:find  [?ls ...]
    :in $
    :where [?ls :phenotype/id]])

(def q-gene
  '[:find  [?gene ...]
    :in $
    :where [?gene :gene/id]])

(def q-gene-class
  '[:find  [?gc ...]
    :in $
    :where [?gc :gene-class/id]])

(def q-protein
  '[:find  [?p ...]
    :in $
    :where [?p :protein/id]])

(def q-species
  '[:find  [?s ...]
    :in $
    :where [?s :species/id]])

(def q-strain
  '[:find  [?s ...]
    :in $
    :where [?s :strain/id]])

(def q-transcript
  '[:find  [?t ...]
    :in $
    :where [?t :transcript/id]])

(def q-variation
  '[:find  [?v ...]
    :in $
    :where [?v :variation/id]])

(defn ppxml [xml]
  (let [in (javax.xml.transform.stream.StreamSource.
            (java.io.StringReader. xml))
        writer (java.io.StringWriter.)
        out (javax.xml.transform.stream.StreamResult. writer)
        transformer (.newTransformer 
                     (javax.xml.transform.TransformerFactory/newInstance))]
    (.setOutputProperty transformer 
                        javax.xml.transform.OutputKeys/INDENT "yes")
    (.setOutputProperty transformer 
                        "{http://xml.apache.org/xslt}indent-amount" "2")
    (.setOutputProperty transformer 
                        javax.xml.transform.OutputKeys/METHOD "xml")
    (.transform transformer in out)
    (-> out .getWriter .toString)))


(defn generate-anatomy-term-file [options db]
 (do
   (println "Generating Anatomy Term file")
   (let [filepath (str (:dir options) "/anatomy_term.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-anatomy-term db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:anatomyTerm
			    {:primaryIdentifier (:anatomy-term/id obj)}
			    [:name {} (->> obj :anatomy-term/term :anatomy-term.term/text)]
			    [:definition {} (->> obj :anatomy-term/definition :anatomy-term.definition/text)]
			    [:synonyms (some->> (:anatomy-term/synonym obj)
			   		        (map :anatomy-term.synonym/text)
                                                (map (fn [s] [:synonym {} s])))]
			    [:parent (when-let [parent-id (->> obj :anatomy-term/parent-term :anatomy-term/id)]
                                           {:primaryIdentifier parent-id})]
			    [:children (some->> (:anatomy-term/_parent-term obj)
			 		        (map :anatomy-term/id)
                                                (map (fn [id] [:child {:primaryIdentifier id}])))]])))
		     (seq))
               
             xml-data
             (xml/sexp-as-element [:anatomyTerms data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-transcript-file [options db]
 (do
   (println "Generating Transcript file")
   (let [filepath (str (:dir options) "/transcript.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-transcript db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:transcript
                             {:primaryIdentifier (str "Transcript:" (:transcript/id obj))}
                             [:symbol {} (:transcript/id obj)]
                             [:method {} (->> obj :locatable/method :method/id)]
                             [:organism {:name (->> obj :transcript/species :species/id)}]
			     [:gene {:primaryIdentifier (some->> (:gene.corresponding-transcript/_transcript obj)
			                                         (map :gene/_corresponding-transcript)
				                                 (map :gene/id)
                                                                 (first))}]
                             [:CDS {:primaryIdentifier (->> obj
                                                 :transcript/corresponding-cds
                                                 :transcript.corresponding-cds/cds
                                                 :cds/id)}]])))
		     (seq))
               
             xml-data
             (xml/sexp-as-element [:transcripts data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-strain-file [options db]
 (do
   (println "Generating Strain file")
   (let [filepath (str (:dir options) "/strain.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-strain db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:strain
                             {:primaryIdentifier (:strain/id obj)}
                             [:genotype {} (:strain/genotype obj)]
                             [:otherName {} (some->> (:strain/other-name obj)
                                                     (map :strain.other-name/text))]
                             [:gene (when-let [id (some->> (:gene/_strain obj)
                                                           (map :gene/id)
                                                           (first))]
                                           {:primaryKey id})]
                             [:inbreedingState {} (when-let [state (:strain/inbreeding-state obj)]
                                                   (name state))]
                             [:outcrossed {} (:strain/outcrossed obj)]
                             [:mutagen {} (:strain/mutagen obj)]
                             [:strainHistory {} (:strain/strain-history obj)]
                             [:dateFirstFrozen {} (:strain/date-first-frozen obj)]
                             [:CGCReceived {} (when-let [date-str (:strain/cgc-received obj)]
                                               (first date-str))]
                             [:laboratories {} (some->> (:strain/location obj)
                                                        (map :strain.location/laboratory)
                                                        (map :laboratory/id)
                                                        (map (fn [id]
                                                              [:laboratory {:primaryIdentifer id}])))]
                             [:madeBy {} (some->> (:strain/made-by obj)
                                               (map :person/id)
                                               (map (fn [id]
                                                     [:person {:primaryIdentifier id}])))]
                             [:remarks {} (some->> (:strain/remark obj)
                                                   (map :strain.remark/text)
                                                   (map (fn [txt]
                                                         [:remark {} txt])))]
                             [:species {:primaryIdentifer(->> obj :strain/species :species/id)}]
                             [:ncbiTaxonomyID {} (->> obj :strain/species :species/ncbi-taxonomy)]])))

                        		     (seq))
               
             xml-data
             (xml/sexp-as-element [:strains data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-rnai-file [options db]
 (do
   (println "Generating Transcript file")
   (let [filepath (str (:dir options) "/rnai.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-rnai db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:rnai
                             {:primaryIdentifier (:rnai/id obj)}
                             [:secondaryIdentifier {} (:rnai/history-name obj)]
                             [:DNA {} (some->> (:rnai/dna-text obj)
                                               (map :rnai.dna-text/name))]
                             [:if.uniquelyMapped {} (if (:rnai/uniquely-mapped obj)
                                        true
                                        false)]
                             [:phenotypeRemarks {} (some->> (:rnai/phenotype obj)
                                                            (map :phenotype-info/remark)
                                                            (map (fn [pi]
                                                            (some->> pi
                                                                     (map :phenotype-info.remark/text))))
                                                            (remove nil?)
                                                            (flatten)
                                                            (map (fn [txt]
                                                                  [:remark {} txt])))]
                             [:remarks (some->> (:rnai/remark obj)
                                               (map :rnai.remark/text)
                                               (map (fn [txt]
                                                     [:remark {} txt])))]
                             [:inhibitsGenes (some->> (:rnai/gene obj)
                                                      (map :rnai.gene/gene)
                                                      (map :gene/id)
                                                      (map (fn [id]
                                                            [:gene {:primaryKey id}])))]
                             [:inhibitsPredictedGene {} (some->> (:rnai/predicted-gene obj)
                                                                 (map :rnai.predicted-gene/cds)
                                                                 (map :cds/id)
                                                                 (map (fn [id]
                                                                       [:cds {:primaryKey id}])))]
                             [:organism (when-let [id (->> obj :rnai/species :species/id)]
                                         {:primaryKey id})]
                             [:phenotypes {} (some->> (:rnai/phenotype obj)
                                                      (map :rnai.phenotype/phenotype)
                                                      (map :phenotype/id)
                                                      (map (fn [id]
                                                            [:phenotype {:primaryIdentifier id}])))]
                             [:phenotype_not_observed {} (some->> (:rnai/phenotype-not-observed obj)
                                                                  (map :rnai.phenotype-not-observed/phenotype)
                                                                  (map :phenotype/id)
                                                                  (map (fn [id]
                                                                        [:phenotype {:primaryIdentifier id}])))]
                             [:laboratories {} (some->> (:rnai/laboratory obj)
                                                        (map :laboratory/id)
                                                        (map (fn [id]
                                                              [:laboratory {:primaryIdentifier id}])))]
                             [:stain (when-let [id (->> obj :rnai/strain :strain/id)]
                                      {:primaryIdentifier id})]
                             [:lifeStage (when-let [id (->> obj :rnai/life-stage :life-stage/id)] ; This is different from what is in the XML query
                                          {:primaryIdentifier id})]
                             [:reference (when-let [id (->> obj :rnai/reference :rnai.reference/paper :paper/id)]
                                          {:primaryIdentifier id})]])))
		     (seq))

             xml-data
            (xml/sexp-as-element [:rnais data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))

(defn generate-gene-file [options db]
 (do
   (println "Generating Gene file")
   (let [filepath (str (:dir options) "/gene.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-gene db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:gene
                             {:primaryIdentifier (:gene/id obj)}
                             [:secondaryIdentifier {} (:gene/sequence-name obj)]
                             [:symbol {} (:gene/public-name obj)]
                             [:name {} (:gene/public-name obj)]
                             [:operons {} (some->> (:operon.contains-gene/_gene obj)
                                                   (map :operon/_contains-gene)
                                                   (map :operon/id)
                                                   (map (fn [id]
                                                         [:operon {:primaryIdentifier id}])))]
                             [:biotype {} (when-let [id (->> obj :gene/biotype :so-term/id)]
                                           [:so-term {:primaryIdentifier id}])]
                             [:lastUpdated {} (->> obj ; this is never populated as far as I can tell
                                                   :gene/evidence
                                                   :evidence/date-last-updated)]
                             [:briefDescription {} (some->> (:gene/concise-description obj)
                                                            (map :gene.concise-description/text)
                                                            (first))]
                             [:descriptions {} (some->> (:gene/automated-description obj) ;e.g. WBGene00105325
                                                        (map :gene.automated-description/text)
                                                        (map (fn [txt]
                                                              [:description {} txt])))]
                             [:organism_name {} (->> obj
                                                     :gene/species
                                                     :species/id)]
                             [:transcripts {} (some->> (:gene/corresponding-transcript obj)
                                                       (map :gene.corresponding-transcript/transcript)
                                                       (map :transcript/id)
                                                       (map (fn [id]
                                                             [:transcript {:primaryIdentifier id}])))]
                             [:variations {} (some->> (:gene/reference-allele obj) ;e.g. WBGene00002363
                                                       (map :gene.reference-allele/variation)
                                                       (map :variation/id)
                                                       (map (fn [id]
                                                             [:variation {:primaryIdentifier id}])))]
                             [:CDSs {} (some->> (:gene/corresponding-cds obj)
                                                (map :gene.corresponding-cds/cds)
                                                (map :cds/id)
                                                (map (fn [id]
                                                      [:cds {:primaryIdentifier id}])))]
                             [:strains {} (some->> (:gene/strain obj)
                                                   (map :strain/id)
                                                   (map (fn [id]
                                                         [:strain {:primaryIdentifier id}])))]])))
		     (seq))
               
             xml-data
             (xml/sexp-as-element [:genes data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-gene-class-file [options db]
 (do
   (println "Generating Gene Class file")
   (let [filepath (str (:dir options) "/gene-class.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-gene-class db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:geneClass
                             {:primaryIdentifier (:gene-class/id obj)}
                             [:description {} (:gene-class/description obj)]
                             [:designatingLaboratory (when-let [id (some->> (:gene-class/designating-laboratory obj) ;e.g. WBGene00002363
                                                                   (:laboratory/id))]
                                                      {:primaryIdentifier id})]
                             [:formerDesignatingLaboratory (when-let [id (some->> (:gene-class/former-designating-laboratory obj) ;e.g. WBGene00040036
                                                                                  (sort-by :gene-class.former-designating-laboratory/until)
                                                                                  (first)
                                                                                  :gene-class.former-designating-laboratory/laboratory
                                                                                  :laboratory/id)]
                                                            {:primaryIdentifier id})]
                             [:variations {} (some->> (:variation/_gene-class obj)
                                                      (map :variation/id)
                                                      (map (fn [id]
                                                            [:variation {:primaryKey id}])))]
                             [:genes {} (some->> (:gene/_gene-class obj)
                                                 (map :gene/id)
                                                 (map (fn [id]
                                                       [:gene {:primaryIdentifier id}])))]
                             [:formerGenes {} (some->> (:gene-class/old-member obj)
                                                       (map (fn [id]
                                                        [:gene {:primaryIdentifier id}])))]
                             [:mainName {} (some->> (:gene-class/main-name obj)
                                                  (first)
                                                  (:gene-class/id))]
                             [:otherName {} (some->> (:gene-class/other-name obj) ; I don't see this field in the database
                                                     (first)
                                                     (:gene-class/id))]
                             [:remark {} (some->> (:gene-class/remark obj)
                                                  (map :gene-class.remark/text)
                                                  (first))]])))
		     (seq))
               
             xml-data
             (xml/sexp-as-element [:geneClasses data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-cds-file [options db]
 (do
   (println "Generating CDS file")
   (let [filepath (str (:dir options) "/cds.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-cds db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:cds
                             {:primaryIdentifier (str "CDS:" (:cds/id obj))}
                             [:symbol {} (:cds/id obj)]
                             [:organism.name {} (->> obj :cds/species :species/id)]
                             [:genes {} (some->> (:gene.corresponding-cds/_cds obj)
                                                    (map :gene/_corresponding-cds)
                                                    (map :gene/id)
                                                    (map (fn [id]
                                                          [:gene {:primaryIdenfier id}])))]
                              [:protein (when-let [id (->> obj
                                                           :cds/corresponding-protein
                                                           :cds.corresponding-protein/protein
                                                           :protein/id)]
                                          {:primaryIdentifier id})]
                              [:transcripts {} (some->> (:gene.corresponding-cds/_cds obj)
                                                           (map :gene/_corresponding-cds)
                                                           (map (fn [g]
                                                                  (some->> (:gene/corresponding-transcript g)
                                                                           (map :gene.corresponding-transcript/transcript)
                                                                           (map :transcript/id))))
                                                           (into [])
                                                           (distinct)
                                                           (remove nil?)
                                                           (map (fn [id]
                                                                 {:primaryIdentifer id})))]])))
		     (seq))
               
             xml-data
             (xml/sexp-as-element [:cdss data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))

(defn generate-expression-cluster-file [options db]
 (do
   (println "Generating Expression Cluster file")
   (let [filepath (str (:dir options) "/expression-cluster.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-expression-cluster db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:expression-cluster
                            {:primaryIdentifier (:expression-cluster/id obj)}
                            [:description {} (first (:expression-cluster/description obj))]
                            [:algorithm {} (first (:expression-cluster/algorithm obj))]
                            [:regulatedByTreatment {} (:expression-cluster/regulated-by-treatment obj)]
                            [:genes {} (some->> (:expression-cluster/gene obj)
                                                (map :expression-cluster.gene/gene)
                                                (map :gene/id)
                                                (map (fn [id]
                                                      [:gene {:primaryIdentifier id}])))]
                            [:regulatedByGenes {} (some->> (:expression-cluster/regulated-by-gene obj)
                                                           (map :gene/id)
                                                           (map (fn [id]
                                                                 [:gene {:primaryIdentifier id}])))]
                            [:regulatedByMolucules (some->> (:expression-cluster/regulated-by-molecule obj)
                                                            (map :molecule/id)
                                                            (map (fn [id]
                                                                  [:molecule {:primaryIdentifier id}])))]
                            [:lifeStages {} (some->> (:expression-cluster/life-stage obj)
                                                     (map :life-stage/id)
                                                     (map (fn [id]
                                                           [:life-stage {:primaryIdentifier id}])))]
                            [:anatomyTerms {} (some->> (:expression-cluster/anatomy-term obj)
                                                       (map :expression-cluster.anatomy-term/anatomy-term)
                                                       (map :anatomy-term/id)
                                                       (map (fn [id]
                                                             [:anatomy-term {:primaryIdentifier id}])))]
                            [:processes {} (some->> (:wbprocess.expression-cluster/_expression-cluster obj)
                                                    (map :wbprocess/_expression-cluster)
                                                    (map :wbprocess/id)
                                                    (map (fn [id]
                                                          [:process {:primaryIdentifier id}])))]])))
		     (seq))
               
             xml-data
             (xml/sexp-as-element [:expression-clusters data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))

(defn generate-expression-pattern-file [options db]
 (do
   (println "Generating Expression Pattern file")
   (let [filepath (str (:dir options) "/expression-pattern.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-expression-pattern db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:expression-pattern
                             {:primaryIdentifier (:expr-pattern/id obj)}
                             [:subcellularLocalization {} (->> obj :expr-pattern/subcellular-localization first)]
                             [:pattern {} (first (:expr-pattern/pattern obj))]
                             [:remarks {} (some->> (:expr-pattern/remark obj)
                                                (map :expr-pattern.remark/text)
                                                (map (fn [txt]
                                                      [:remark {} txt])))]
                             [:reporterGene {} (->> obj :expr-pattern/reporter-gene first)]
                             [:genes {} (some->> (:expr-pattern/reflects-endogenous-expression-of obj)
                                                 (map :gene/id)
                                                 (map (fn [id]
                                                       [:gene {:primaryIdentifier id}])))]
                             [:anatomyTerms {} (some->> (:expr-pattern/anatomy-term obj)
                                                        (map :expr-pattern.anatomy-term/anatomy-term)
                                                        (map :anatomy-term/id)
                                                        (map (fn [id]
                                                              [:anatomy-term {:primaryIdentifier id}])))]
                             [:lifeStages {} (some->> (:expr-pattern/life-stage obj)
                                                      (map :expr-pattern.life-stage/life-stage)
                                                      (map :life-stage/id)
                                                      (map (fn [id]
                                                            [:life-stage {:primaryIdentfier id}])))]
                             [:GOTerms {} (some->> (:expr-pattern/go-term obj)
                                                   (map :expr-pattern.go-term/go-term)
                                                   (map :go-term/id)
                                                   (map (fn [id]
                                                         [:go-term {:PrimaryIdentifier id}])))]])))
		     (seq))
               
             xml-data
             (xml/sexp-as-element [:expression-patterns data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-laboratory-file [options db]
 (do
   (println "Generating Laboratory file")
   (let [filepath (str (:dir options) "/laboratory.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-laboratory db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:laboratory
                             {:primaryIdentifier (:laboratory/id obj)}])))
	             (seq))
               
             xml-data
             (xml/sexp-as-element [:laboratories data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-life-stage-file [options db]
 (do
   (println "Generating Life Stage file")
   (let [filepath (str (:dir options) "/life-stage.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-life-stage db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:life-stage
                             {:primaryIdentifier (:life-stage/id obj)}
                             [:definition {} (first (:life-stage/definition obj))]
                             [:publicName {} (:life-stage/public-name obj)]
                             [:remarks {} (some->> (:life-stage/remark obj)
                                                   (map :life-stage.remark/text)
                                                   (map (fn [txt]
                                                         [:remark {} txt])))]
                             [:otherName {} nil]
                             [:containedIn {} nil] ; primaryIdentifier
                             [:precededBy {} (some->> (:life-stage/preceded-by obj)
                                                          (map :life-stage/id)
                                                          (map (fn [id]
                                                                [:life-stage {:primaryIdentifier id}])))]
                             [:followedBy {} (some->> (:life-stage/_preceded-by obj)
                                                      (map :life-stage/id)
                                                      (map (fn [id]
                                                            [:life-stage {:primaryIdentifier id}])))]
                             [:subStages {} (some->> (:life-stage/_contained-in obj)
                                                     (map :life-stage/id)
                                                     (map (fn [id]
                                                           [:life-stage {:primaryIdentifier id}])))]
                             [:anatomyTerms {} (some->> (:life-stage/anatomy-term obj)
                                                        (map :anatomy-term/id)
                                                        (map (fn [id]
                                                              [:anatomy-term {:primaryIdentifier id}])))]
                             [:expressionPatterns {} (some->> (:expr-pattern.life-stage/_life-stage obj)
                                                              (map :expr-pattern/_life-stage)
                                                              (map :expr-pattern/id)
                                                              (map (fn [id]
                                                                    [:expr-pattern {:primaryIdentifier id}])))]])))
	             (seq))
               
             xml-data
             (xml/sexp-as-element [:life-stages data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-phenotype-file [options db]
 (do
   (println "Generating Phenotype file")
   (let [filepath (str (:dir options) "/phenotype.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-phenotype db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:phenotype
                             {:primaryIdentifier (:phenotype/id obj)}
                             [:synonyms {} (some->> (:phenotype/synonym obj)
                                                    (map :phenotype.synonym/text)
                                                    (map (fn [txt]
                                                          [:synonym {} txt])))]
                             [:parents {} (some->> (:phenotype/specialisation-of obj)
                                                   (map :phenotype/id)
                                                   (map (fn [id]
                                                         [:phenotype {:primaryIdentifier id}])))]
                             [:children {} (some->> (:phenotype/_specialisation-of obj)
                                                    (map :phenotype/id)
                                                    (map (fn [id]
                                                          [:phenotype {:primaryIdentifier id}])))]])))
	             (seq))
               
             xml-data
             (xml/sexp-as-element [:phenotypes data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-protein-file [options db]
 (do
   (println "Generating Protein file")
   (let [filepath (str (:dir options) "/protein.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-protein db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:protein
                             {:primaryIdentifier (:protein/id obj)}
                             [:symbol {} (->> obj :protein/gene-name first)]
                             [:molecularWeight {} (->> obj :protein/molecular-weight :protein.molecular-weight/float)]
                             [:organism.name {} (->> obj :protein/species :species/id)]
                             [:CDSs (some->> (:cds.corresponding-protein/_protein obj)
                                             (map :cds/_corresponding-protein)
                                             (map :cds/id)
                                             (map (fn [id]
                                                   [:cds {:primaryIdentifier id}])))]
                             [:motifs nil]]))) ;requires homology db
	             (seq))
               
             xml-data
             (xml/sexp-as-element [:proteins data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-species-file [options db]
 (do
   (println "Generating Species file")
   (let [filepath (str (:dir options) "/species.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-species db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:species
                             {}
                             [:name (:species/id obj)]
                             [:taxonId (:species/ncbi-taxonomy obj)]])))
	             (seq))
               
             xml-data
             (xml/sexp-as-element [:species data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-strain-file [options db]
 (do
   (println "Generating Strain file")
   (let [filepath (str (:dir options) "/strain.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-strain db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:strain
                             {:primaryIdentifier (:strain/id obj)}
                             [:genotype {} (:strain/genotype obj)]
                             [:otherNames {} (some->> (:strain/other-name obj)
                                                      (map :strain.other-name/text)
                                                      (map (fn [txt]
                                                            [:otherName {} txt])))]
                             [:genes {} (some->> (:gene/_strain obj)
                                                 (map :gene/id)
                                                 (map (fn [id]
                                                       [:gene {:primaryIdentifier id}])))]
                             [:inbreedingState {} (when-let [state (:strain/inbreeding-state obj)]
                                                   (name state))]
                             [:outcrossed {} (:strain/outcrossed obj)]
                             [:mutagen {} (first (:strain/mutagen obj))]
                             [:strainHistory {} (:strain/strain-history obj)]
                             [:dateFirstFrozen {} (:strain/date-first-frozen obj)]
                             [:CGCReceived {} (first (:strain/cgc-received obj))]
                             [:laboratories (some->> (:strain/location obj)
                                                     (map :strain.location/laboratory)
                                                     (map :laboratory/id)
                                                     (map (fn [id]
                                                           [:laboratory {:primaryIdentifier id}])))]
                             [:madeBy {} (some->> (:strain/made-by obj)
                                                  (map :person/id)
                                                  (map (fn [id]
                                                        [:person {:primaryIdentifier id}])))]
                             [:remarks {} (some->> (:strain/remark obj)
                                                   (map :strain.remark/text)
                                                   (map (fn [txt]
                                                         [:strain {:primaryIdentifier id}])))]
                             [:species (when-let [id (->> obj :strain/species :species/id)]
                                         {:primaryIdentifier id})]
                             [:ncbiTaxonomyID {} (->> obj :strain/species :species/ncbi-taxonomy)]])))
	             (seq))
               
             xml-data
             (xml/sexp-as-element [:strains data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-transcript-file [options db]
 (do
   (println "Generating Transcript file")
   (let [filepath (str (:dir options) "/transcript.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-transcript db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:transcript
                             {:primaryIdentifier (str "Transcript:" (:transcript/id obj))}
                             [:symbol {} (:transcript/id obj)]
                             [:method {} (->> obj :locatable/method :method/id)]
                             [:organism.name {} (->> obj :transcript/species :species/id)]
                             [:genes {} (some->> (:gene.corresponding-transcript/_transcript obj)
                                                 (map :gene/_corresponding-transcript)
                                                 (map :gene/id)
                                                 (map (fn [id]
                                                       [:gene {:primaryIdentifier id}])))]
                             [:cds (when-let [id (->> obj
                                                     :transcript/corresponding-cds
                                                     :transcript.corresponding-cds/cds
                                                     :cds/id)]
                                     {:primaryIdentifier id})]])))
	             (seq))
               
             xml-data
             (xml/sexp-as-element [:transcripts data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))


(defn generate-variantions-file [options db]
 (do
   (println "Generating Variations file")
   (let [filepath (str (:dir options) "/variations.xml")
         p (println (str "Filepath: " filepath))]
    (with-open [out-file (java.io.OutputStreamWriter.
                         (java.io.FileOutputStream. filepath) "UTF-8")]
      (let [data
            (some->> (d/q q-variation db)
	             (map (fn [id]
			   (let [obj (d/entity db id)]
                            [:variation
                             {:primaryIdentifier (:variation/id obj)}
                             [:symbol {} (:variation/public-name obj)]
                             [:otherName {} (:variation/other-name obj)]
                             [:engineeredAllele_type {} (if (:variation/engineered-allele obj) "true" "false")]
                             [:allele_type {} (if (:variation/allele obj) "true" "false")]
                             [:snp_type {} (if (:variation/snp obj) "true" "false")]
                             [:confirmed_snp_type {} (if (:variation/confirmed-snp obj) "true" "false")]
                             [:predicted_snp_type {} (if (:variation/predicted-snp obj) "true" "false")]
                             [:rflp_type nil] ; field exists on locus but is not populated
                             [:transposonInsertion_type {} (some->> (:variation/transposon-insertion obj)
                                                                    (map :transposon-family/id)
                                                                    (map (fn [id]
                                                                          [:transposon-family {:primaryIdentifier id}])))]
                             [:naturalVariant_type {} (if (:variation/natural-variant obj) "true" "false")]
                             [:substitution_typeOfMutation {} (when-let [s (:variation/substitution obj)]
                                                               {:ref (:variation.substitution/ref s)
                                                                :alt (:variation.substitution/alt s)})]
                             [:deletion_typeOfMutation {} (if (contains? obj :variation/deletion) "true" "false")]
                             [:tandemDuplication_typeOfMutation {} (if (contains? obj :variation/tandem-duplication) "true" "false")]
                             [:sequenceStatus {} (when (contains? obj :variation/seqstatus)
                                                  (name (:variation/seqstatus obj)))]
                             [:linkedTo {} (some->> (:variation/linked-to obj)
                                                    (map :variation/id))]
                             [:species {} (->> obj :variation/species :species/id)]
                             [:productionMethod {} (when (contains? obj :variation/production-method)
                                                    (name (:variation/production-method obj)))]
                             [:if.KOConsoriumAllele {} (if (:variation/ko-consortium-allele obj) "true" "false")]
                             [:if.NBPAllele {} (if (:variation/nbp-allele obj) "true" "false")]
                             [:if.NemaGENETAGConsortiumAllele {} (if (:variation/nemagenetag-consortium-allele obj) "true" "false")]
                             [:detectionMethod {} (:variation/detection-method obj)]
                             [:live_status {} (when-let [s (->> obj
                                                                :variation/status
                                                                :variation.status/value)]
                                               (name s))]
                             [:genes {} (some->> (:variation/gene obj)
                                                 (map :variation.gene/gene)
                                                 (map :gene/id)
                                                 (map (fn [id]
                                                       [:gene {:primaryIdentifier id}])))]
                             [:mutagen {} (->> obj :variation/mutagen :variation.mutagen/text)]
                             [:geneClasses {} (some->> (:variation/gene-class obj)
                                                       (map :gene-class/id)
                                                       (map (fn [id]
                                                             [:gene-class {:primaryIdentifier id}])))]
                             [:nonsense.molecularChange {} (some->> (:variation/predicted-cds obj)
                                                                    (map :molecular-change/nonsense)
                                                                    (remove nil?)
                                                                    (map (fn [h]
                                                                          [:text {} (:molecular-change.nonsense/text h)]
                                                                          [:value {} (when-let [t (:molecular-change.nonsense/value h)]
                                                                                      (name t))])))]
                             [:missenseChange {} (some->> (:variation/predicted-cds obj)
                                                          (map :molecular-change/missense)
                                                          (map (fn [m]
                                                                (some-> m
                                                                        (first)
                                                                        (:molecular-change.missense/text))))
                                                          (map (fn [txt]
                                                                [:change {} txt])))]
                             [:phenotypes {} (some->> (:variation/phenotype obj)
                                                      (map :variation.phenotype/phenotype)
                                                      (map :phenotype/id)
                                                      (map (fn [id]
                                                            [:phenotype {:primaryIdentifier id}])))]
                             [:phenotypesNotObserved {} (some->> (:variation/phenotype-not-observed obj)
                                                                 (map :variation.phenotype-not-observed/phenotype)
                                                                 (map :phenotype/id)
                                                                 (map (fn [id]
                                                                       [:phenotype {:primaryIdentifier id}])))]])))
	             (seq))
               
             xml-data
             (xml/sexp-as-element [:variations data])]
         (.write out-file (ppxml (xml/emit-str xml-data))))))))



(defn generate-files [options db]
 (do
  (println options)
  (if (.isDirectory (io/file (:dir options)))
   (do
    (generate-anatomy-term-file options db)
    (generate-transcript-file options db)
    (generate-strain-file options db)
    (generate-rnai-file options db)
    (generate-gene-file options db)
    (generate-gene-class-file options db)
    (generate-cds-file options db) ;requires lots of memeory
    (generate-expression-cluster-file options db)
    (generate-expression-pattern-file options db)
    (generate-laboratory-file options db)
    (generate-life-stage-file options db)
    (generate-phenotype-file options db)
    (generate-protein-file options db)
    (generate-species-file options db)
    (generate-strain-file options db)
    (generate-transcript-file options db)
    (generate-variantions-file options db) ;requires lots of memory
      )
   (println (str "provided folder is not valid: " (:dir options))))))
