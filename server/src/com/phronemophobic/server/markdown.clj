(ns com.phronemophobic.server.markdown
  (:require [clojure.zip :as z]
            [clojure.java.io :as io]
            [hiccup.core :refer [html]
             :as hiccup])
  (:import com.vladsch.flexmark.util.ast.Node
           com.vladsch.flexmark.html.HtmlRenderer
           com.vladsch.flexmark.parser.Parser
           com.vladsch.flexmark.ext.attributes.AttributesExtension
           com.vladsch.flexmark.ext.xwiki.macros.MacroExtension
           com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
           com.vladsch.flexmark.util.data.MutableDataSet
           com.phronemophobic.server.HiccupNode))


(defn doc->tree-seq [doc]
  (tree-seq #(.hasChildren %)
            #(seq (.getChildren %))
            doc))

(defn doc->zip [doc]
  (z/zipper #(.hasChildren %)
            #(seq (.getChildren %))
            (fn [node children]
                (.removeChildren node)
                (doseq [child children]
                  (.appendChild node child))
              node)
            doc))

(defn children [doc]
  (seq (.getChildren doc)))

(def ^:dynamic *parse-state*)

(defn zip-walk
  "Depth first walk of zip. edit each loc with f"
  [zip f]
  (loop [zip zip]
    (if (z/end? zip)
      (z/root zip)
      (recur (-> (z/edit zip f)
                 z/next)))))

(declare hiccup-node)

(defmulti macroexpand1-doc
  (fn [m]
    (if (instance? com.vladsch.flexmark.ext.xwiki.macros.Macro m)
      (.getName m)
      (if (instance? com.vladsch.flexmark.ext.xwiki.macros.MacroBlock m)
        (.getName (.getMacroNode m))
        (type m)))))

(defmethod macroexpand1-doc :default [node]
  node)

(defn inc-footnote-count []
  (let [k [::footnote-macro ::footnote-index]]
    (set! *parse-state*
          (update-in *parse-state* k (fnil inc 0)))
    (get-in *parse-state* k)))
(defn add-footnote [hiccup]
  (set! *parse-state*
        (update-in *parse-state*
                   [::footnote-macro ::footnotes] (fnil conj []) hiccup)))

(defprotocol IRecipeHtml
  (recipe-html [this]))

(defmethod macroexpand1-doc "footnote" [macro]
  (let [idx (inc-footnote-count)
        childs (->> (children macro)
                    (remove #(instance? com.vladsch.flexmark.ext.xwiki.macros.MacroClose %))
                    (remove #(instance? com.vladsch.flexmark.ext.xwiki.macros.Macro %)))]
    (add-footnote (map recipe-html childs))
    (hiccup-node
     [:sup
      [:a {:href (str "#footnote-" idx)
           :name (str "footnote-ref-" idx)
           :title (clojure.string/join (map #(.getChars %) childs))} idx]])))

(defn get-footnotes []
  (get-in *parse-state* [::footnote-macro ::footnotes]))

(defmethod macroexpand1-doc "footnotes" [macro]
  (hiccup-node
   [:div.footnotes
    (for [[i footnote] (map-indexed vector (get-footnotes))
          :let [idx (inc i)]]
      [:div
       [:a {:name (str "footnote-" idx)
            :href (str "#footnote-ref-" idx)} idx]
       ". " footnote])]))



(defn preprocess-footnotes [doc]
  (binding [*parse-state* nil]
    (zip-walk (doc->zip doc)
              macroexpand1-doc)))

(defn header->tag-name [header]
  (let [tag-chars (clojure.string/join
                     (map #(.getChars %) (children header)))
        tag-name (-> tag-chars
                       (clojure.string/replace #"[ ]" "-")
                       (clojure.string/replace #"[^A-Z\-a-z0-9]" ""))]
    tag-name))

(defn make-toc []
  (z/zipper (constantly true)
            #(get % :children [])
            (fn [node children]
              (assoc node :children children))
            {}))


(defn add-section [toc header]

  (loop [toc toc]
    (let [p (z/up toc)
          level (.getLevel header)]
      (if (and p (>= (get (z/node toc) :level)
                     level))
        (recur p)
        (let [res (-> toc
                      (z/append-child {:title (clojure.string/join
                                               (map #(.getChars %) (children header)))
                                       :name (header->tag-name header)
                                       :level level})
                      z/down
                      z/rightmost)]
          res)))))


(defn gen-table-of-contents [doc]
  (loop [zip (doc->zip doc)
         toc (make-toc)]
    (if (z/end? zip)
      (z/root toc)
      (let [node (z/node zip)]
        (if (instance? com.vladsch.flexmark.ast.Heading node)
          (recur (z/next zip) (add-section toc node))
          (recur (z/next zip) toc))))))




(defn gen-toc-html [toc]
  (let [html ()
        html (if-let [childs (:children toc)]
               (cons [:ul (map gen-toc-html childs)]
                     html)
               html)
        html (if-let [title (:title toc)]
               (cons [:li [:a {:href (str "#" (:name toc))} title]]
                     html)
               html)]
    html))


(defn preprocess-table-of-contents [doc]
  (let [toc (gen-table-of-contents doc)
        toc-html (gen-toc-html toc)]
    (zip-walk (doc->zip doc)
              (fn [node]
                (if (instance? com.vladsch.flexmark.ext.xwiki.macros.MacroBlock node)
                  (if (= (.getName (.getMacroNode node)) "table-of-contents")
                    (hiccup-node toc-html)
                    node)
                  node)))))

(defn parse [s]
  (let [options (doto (MutableDataSet.)
                  (.set Parser/EXTENSIONS [(AttributesExtension/create)
                                           (MacroExtension/create)
                                           (TaskListExtension/create)
                                           ]))
        parser (-> (Parser/builder options)
                   (.build))
        doc (.parse parser s)
        doc (-> doc
                (preprocess-footnotes)
                (preprocess-table-of-contents))]

    doc))

 

(extend-type Object
  IRecipeHtml
  (recipe-html [this]
    (println "No implementation of method :recipe-html found for class: " (type this))
    (println "line: " (.getLineNumber this))
    (let [s (str (.getChars this))]
      (println (subs s 0 (min 30 (count s)))))
    (throw (Exception. ""))))

(defn hiccup-node [content]
  (HiccupNode. content))

#_(defn doall* [s]
    (dorun (tree-seq #(do
                        (prn %)
                        (seqable? %)) seq s)) s)
(extend-type HiccupNode
  IRecipeHtml
  (recipe-html [this]
    (.hiccup this)))

(extend-type com.vladsch.flexmark.util.ast.Document
  IRecipeHtml
  (recipe-html [this]
    [:div {}
     (map recipe-html (children this))]))

(extend-type com.vladsch.flexmark.ast.Paragraph
  IRecipeHtml
  (recipe-html [this]
    [:p {}
     (map recipe-html (children this))
     ]))

(extend-type com.vladsch.flexmark.ast.Heading
  IRecipeHtml
  (recipe-html [this]
    (let [tag (keyword (str "h" (.getLevel this)))]
      [tag {:id (header->tag-name this)}
       (map recipe-html (children this))])))

(extend-type com.vladsch.flexmark.ast.StrongEmphasis
  IRecipeHtml
  (recipe-html [this]
    [:strong
     (map recipe-html (children this))]))

(extend-type com.vladsch.flexmark.ast.Emphasis
  IRecipeHtml
  (recipe-html [this]
    [:em
     (map recipe-html (children this))]))


(extend-type com.vladsch.flexmark.ast.BlockQuote
  IRecipeHtml
  (recipe-html [this]
    [:blockquote.blockquote
     (map recipe-html (children this))]))

(extend-type com.vladsch.flexmark.ast.FencedCodeBlock
  IRecipeHtml
  (recipe-html [this]


    (case (.getInfo this)

      ;; "clojure"
      ;; (let [source (clojure.string/join (map #(.getChars %) (children this)))]
      ;;   (glow/highlight-html source))

      ;; else
      [:pre
       [:code
        (map recipe-html (children this))]]
      )

    ))


(extend-type com.vladsch.flexmark.ast.Code
  IRecipeHtml
  (recipe-html [this]
    [:code (map recipe-html (children this))]))

(extend-type com.vladsch.flexmark.ast.Text
  IRecipeHtml
  (recipe-html [this]
    (hiccup/h (str (.getChars this)))))

(extend-type com.vladsch.flexmark.ast.Link
  IRecipeHtml
  (recipe-html [this]
    [:a {:href (-> this .getUrl str)}
     (-> this (.getText) str)]))

(extend-type com.vladsch.flexmark.ast.AutoLink
  IRecipeHtml
  (recipe-html [this]
    [:a {:href (-> this .getUrl str)}
     (-> this (.getText) str)]))

(extend-type com.vladsch.flexmark.ast.Image
  IRecipeHtml
  (recipe-html [this]
    [:img {:src (-> this .getUrl str)
           :alt (-> this (.getText) str)
           :style "max-width: 90vw;height:auto"}]))


(extend-type com.vladsch.flexmark.ast.SoftLineBreak
  IRecipeHtml
  (recipe-html [this]
    [:br]))

(extend-type com.vladsch.flexmark.ast.OrderedList
  IRecipeHtml
  (recipe-html [this]
    [:ol (map recipe-html (children this))]))

(extend-type com.vladsch.flexmark.ast.OrderedListItem
  IRecipeHtml
  (recipe-html [this]
    [:li (map recipe-html (children this))]))


(extend-type com.vladsch.flexmark.ast.BulletList
  IRecipeHtml
  (recipe-html [this]
    [:ul (map recipe-html (children this))]))

(extend-type com.vladsch.flexmark.ast.BulletListItem
  IRecipeHtml
  (recipe-html [this]
    [:li (map recipe-html (children this))]))

(extend-type com.vladsch.flexmark.ast.HtmlCommentBlock
  IRecipeHtml
  (recipe-html [this]
    nil))

(extend-type com.vladsch.flexmark.ast.HtmlInlineComment
  IRecipeHtml
  (recipe-html [this]
    nil))


;; (extend-type com.vladsch.flexmark.ast.OrderedList
;;   IRecipeHtml
;;   (recipe-html [this]
;;     [:ol.list-group (map recipe-html (children this))]))
;; com.vladsch.flexmark.ast.BulletList

(defmulti markdown-macro (fn [macro]
                           (.getName macro)))


(defmethod markdown-macro "tableflip" [macro]
  [:p {:title "table flip"} "(╯°□°）╯︵ ┻━┻"])

(defmethod markdown-macro "blockquote-footer" [macro]
  [:footer.blockquote-footer
   (map recipe-html (drop-last (children macro)))])

(defmethod markdown-macro "contemplation-break" [macro]
  [:div {:style "width: 90%;height: 500px;border-top: #c4c4c4 1px solid;border-bottom: #c4c4c4 1px solid;margin-bottom:30px;"
         :title "This space intentionally left blank for contemplation."}])


(defmethod markdown-macro "shoot-in-the-foots" [macro]
  [:span {:title "everybody is each shooting just one foot in this metaphor"}
   "foots"])

(defmethod markdown-macro "square-bracket-left" [macro]
  "[")

(defmethod markdown-macro "square-bracket-right" [macro]
  "]")

(defmethod markdown-macro "quote" [macro]
  (let [childs (->> (children macro)
                    (remove #(instance? com.vladsch.flexmark.ext.xwiki.macros.MacroClose %))
                    (remove #(instance? com.vladsch.flexmark.ext.xwiki.macros.Macro %)))]
    (hiccup/h (clojure.string/join (map #(.getChars %) childs)))))


(extend-type com.vladsch.flexmark.ext.xwiki.macros.Macro
  IRecipeHtml
  (recipe-html [this]
    (markdown-macro this)))

(extend-type com.vladsch.flexmark.ext.xwiki.macros.MacroBlock
  IRecipeHtml
  (recipe-html [this]
    (markdown-macro (.getMacroNode this))))

(extend-type com.vladsch.flexmark.ext.gfm.tasklist.TaskListItem
  IRecipeHtml
  (recipe-html [this]
;; <input type="checkbox" id="subscribeNews" name="subscribe" value="newsletter">
    ;;     <label for="subscribeNews">Subscribe to newsletter?</label>
    (let [checkbox-id (name (gensym))]
      [:div
       [:input {:type "checkbox"
                :id checkbox-id}]
       [:label {:for checkbox-id}
        (map recipe-html (children this))]])
    ))


(extend-type com.vladsch.flexmark.ast.ThematicBreak
  IRecipeHtml
  (recipe-html [this]
    [:hr]))


(defn parse-recipe [fname]
  (-> (slurp fname)
      parse
      recipe-html))

(defn recipe-page [title
                   body]
  (let [asset-prefix ""]
    [:html {:lang "en"}
     [:head

      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}] 
      ;; [:meta {:name "description" :content ""}]
      ;; [:meta {:name "author" :content "Adrian Smith"}]

      ;; <link rel="icon" href="../../favicon.ico">
      #_[:link {:rel "icon"
              :href (str asset-prefix "favicon.ico")}]
      [:title title]

      [:link {:href (str asset-prefix "bootstrap.min.css")
              :rel "stylesheet"}]
      [:link {:href (str asset-prefix "recipe.css")
              :rel "stylesheet"}]
]

     [:body
      [:div {:class "recipe-masthead"}
       [:nav.nav.recipe-nav
        [:a.nav-link {:href "index.html"}
         "Kitchen"]
        ]]

      [:div.recipe-header
       [:div.container
        [:h1.recipe-title title]
        #_[:p.lead.recipe-description subheading]]]


      [:div.container
       [:div.row
        [:div.col-sm-8.recipe-main
         [:div.recipe-post
          body]]]]


      ]])
  )



(defn project-root []
  (-> (io/file ".")
      (.getCanonicalFile)
      (.getParentFile)))

(defn output-folder []
  (io/file (project-root)
           ;; only available option from github
           "server"
           "resources"
           "public"))

(defn recipes-dir []
  (io/file (project-root)
           "recipes"))



(defn all-recipes []
  (->> (tree-seq #(.isDirectory %)
                 #(.listFiles %)
                 (recipes-dir))
       (filter #(clojure.string/ends-with? (.getName %) ".md")))
  )

(defn recipe->category [recipe-file]
  (let [rd (recipes-dir)]
    (loop [categories '()
           f (.getParentFile recipe-file)]
      (cond
        (nil? f) (throw (Exception. "Recipe not in recipe dir"))
        (= f rd) categories
        :else (recur (conj categories (.getName f))
                     (.getParentFile f))))))

(defn render-recipe! [f]
  (let [
        doc (parse (slurp f))
        body (recipe-html doc)
        title (first (keep (fn [x]
                             (when (instance? com.vladsch.flexmark.ast.Heading x)
                               (str (.getText x))))
                           (doc->tree-seq doc)))

        page-html (recipe-page title body)
        html-str (html page-html)
        html-filename (clojure.string/replace (.getName f) #".md" ".html")]
    (spit (io/file (output-folder) html-filename) html-str)))

(defn render-index []
  (let [toc (->> (all-recipes)
                 (map (juxt recipe->category #(.getName %)))
                 (partition-by first) )
        page-html (recipe-page
                   "Dinner"
                   [:div
                    (for [category toc
                          :let [category-name (-> category
                                                  first
                                                  first
                                                  first
                                                  clojure.string/capitalize)]]
                      (list
                       [:h2 category-name]
                       (for [[_ post] category
                             :let [html (clojure.string/replace post #".md" ".html")]]
                         [:div
                          [:a {:href html}
                           html]])))])
        html-str (html page-html)]
    (spit (io/file (output-folder) "index.html") html-str)))

(defn render-all! []
  (render-index)
  (run! render-recipe! (all-recipes)))

(defn -main [ & args]
  (when (not (.exists (output-folder)))
    (.mkdir (output-folder)))
  (render-all!))


(comment

  (require '[com.phronemophobic.membrane.spreadsheet :as ss]
           '[com.phronemophobic.membrane.inspector :as iv])


  (do
    (alter-var-root #'ss/eval-ns (constantly *ns*))
    (ss/-main))
  

  )
