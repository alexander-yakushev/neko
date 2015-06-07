(ns neko.activity
  "Utilities to aid in working with an activity."
  (:require [clojure.string :as s]
            neko.init
            [neko.ui :refer [make-ui]]
            [neko.debug :refer [all-activities safe-for-ui]]
            [neko.-utils :as u])
  (:import android.app.Activity
           [android.view View Window]
           android.app.Fragment
           neko.ActivityWithState))

(defn ^View get-decor-view
  "Returns the root view of the given activity."
  [^Activity activity]
  (.. activity getWindow getDecorView))

(defn set-content-view!
  "Sets the content for the activity.  The view may be one of:

  + neko.ui tree
  + A view object, which will be used directly
  + An integer presumed to be a valid layout ID."
  [^Activity activity, view]
  {:pre [(instance? Activity activity)]}
  (cond
   (instance? View view)
   (.setContentView activity ^View view)

   (integer? view)
   (.setContentView activity ^Integer view)

   :else
   (let [dv (get-decor-view activity)]
     (.setTag dv (java.util.HashMap.))
     (.setContentView activity
                      ^View (neko.ui/make-ui-element activity view
                                                     {:id-holder dv})))))

(defmacro request-window-features!
  "Requests the given features for the activity. The features should be keywords
  such as :no-title or :indeterminate-progress corresponding FEATURE_NO_TITLE
  and FEATURE_INDETERMINATE_PROGRESS, respectively. Returns a sequence of
  booleans whether for each feature that indicates if the feature is supported
  and now enabled.

  This macro should be called before set-content-view!."
  [^Activity activity & features]
  {:pre [(every? keyword? features)]}
  `[~@(for [feat features]
        `(.requestWindowFeature
          ~activity ~(symbol (str (.getName Window) "/FEATURE_"
                                  (u/keyword->static-field (name feat))))))])

(defmacro old-defactivity
  "Creates an activity with the given full package-qualified name.
  Optional arguments should be provided in a key-value fashion.

  Available optional arguments:

  :extends, :implements, :prefix - same as for `gen-class`.

  :features - window features to be requested for the activity.
  Relevant only if :create is used.

  :on-create - takes a two-argument function. Generates a handler for
  activity's `onCreate` event which automatically calls the
  superOnCreate method and creates a var with the name denoted by
  `:def` (or activity's lower-cased name by default) to store the
  activity object. Then calls the provided function onto the
  Application object.

  :on-start, :on-restart, :on-resume, :on-pause, :on-stop, :on-destroy
  - same as :on-create but require a one-argument function."
  [name & {:keys [extends implements prefix on-create on-create-options-menu
                  on-options-item-selected on-activity-result
                  on-new-intent def state key features]
           :as options}]
  (let [options (or options {}) ;; Handle no-options case
        sname (u/simple-name name)
        prefix (or prefix (str sname "-"))
        state (or state `(atom {}))]
    (when def
      (println "WARNING: :def attribute in defactivity is deprecated.
Use (*a) to get the current activity."))
    `(do
       (gen-class
        :name ~name
        :main false
        :prefix ~prefix
        ~@(when state
            '(:init "init" :state "state"))
        :extends ~(or extends Activity)
        :implements ~implements
        :exposes-methods {~'onCreate ~'superOnCreate
                          ~'onStart ~'superOnStart
                          ~'onRestart ~'superOnRestart
                          ~'onResume ~'superOnResume
                          ~'onPause ~'superOnPause
                          ~'onStop ~'superOnStop
                          ~'onCreateContextMenu ~'superOnCreateContextMenu
                          ~'onContextItemSelected ~'superOnContextItemSelected
                          ~'onCreateOptionsMenu ~'superOnCreateOptionsMenu
                          ~'onOptionsItemSelected ~'superOnOptionsItemSelected
                          ~'onActivityResult ~'superOnActivityResult
                          ~'onNewIntent ~'superOnNewIntent
                          ~'onDestroy ~'superOnDestroy})
       ~(when state
          `(defn ~(symbol (str prefix "init"))
             [] [[] ~state]))
       ~(when on-create
          `(defn ~(symbol (str prefix "onCreate"))
             [~(vary-meta 'this assoc :tag name),
              ^android.os.Bundle ~'savedInstanceState]
             (.superOnCreate ~'this ~'savedInstanceState)
             ~(when (and (not (:neko.init/release-build *compiler-options*))
                         def)
                `(def ~(vary-meta def assoc :tag name) ~'this))
             (.put all-activities '~(.name *ns*) ~'this)
             ~(when key
                `(.put all-activities ~key ~'this))
             (neko.init/init (.getApplicationContext ~'this))
             ~(when features
                `(request-window-features! ~'this ~@features))
             (~on-create ~'this ~'savedInstanceState)))
       ~(when on-create-options-menu
          `(defn ~(symbol (str prefix "onCreateOptionsMenu"))
             [~(vary-meta 'this assoc :tag name),
              ^android.view.Menu ~'menu]
             (.superOnCreateOptionsMenu ~'this ~'menu)
             (~on-create-options-menu ~'this ~'menu)
             true))
       ~(when on-options-item-selected
          `(defn ~(symbol (str prefix "onOptionsItemSelected"))
             [~(vary-meta 'this assoc :tag name),
              ^android.view.MenuItem ~'item]
             (~on-options-item-selected ~'this ~'item)
             true))
       ~(when on-activity-result
          `(defn ~(symbol (str prefix "onActivityResult"))
             [~(vary-meta 'this assoc :tag name),
              ^int ~'requestCode,
              ^int ~'resultCode,
              ^android.content.Intent ~'intent]
             (.superOnActivityResult ~'this ~'requestCode ~'resultCode ~'intent)
             (~on-activity-result ~'this ~'requestCode ~'resultCode ~'intent)))
       ~(when on-new-intent
          `(defn ~(symbol (str prefix "onNewIntent"))
             [~(vary-meta 'this assoc :tag name),
              ^android.content.Intent ~'intent]
             (.superOnNewIntent ~'this ~'intent)
             (~on-new-intent ~'this ~'intent)))
       ~@(map #(let [func (options %)
                     event-name (u/keyword->camelcase %)]
                 (when func
                   `(defn ~(symbol (str prefix event-name))
                      [~(vary-meta 'this assoc :tag name)]
                      (~(symbol (str ".super" (u/capitalize event-name))) ~'this)
                      (~func ~'this))))
              [:on-start :on-restart :on-resume
               :on-pause :on-stop :on-destroy]))))

(defmacro ^{:forms '[name & options & methods]} defactivity
  "Creates an activity with the given full package-qualified name.
  Optional arguments should be provided in a key-value fashion.

  Available optional arguments:

  :extends, :implements, :prefix - same as for `gen-class`.

  :features - window features to be requested for the activity.
  Relevant only if :create is used.

  :on-create - takes a two-argument function. Generates a handler for
  activity's `onCreate` event which automatically calls the
  superOnCreate method and creates a var with the name denoted by
  `:def` (or activity's lower-cased name by default) to store the
  activity object. Then calls the provided function onto the
  Application object.

  :on-start, :on-restart, :on-resume, :on-pause, :on-stop, :on-destroy
  - same as :on-create but require a one-argument function."
  [name & args]
  (if (some #{:on-create} args)
    `(old-defactivity name ~@args)
    (let [[{:keys [extends implements prefix state key features]} methods]
          (loop [args args, options {}, methods {}]
            (cond (empty? args) [options methods]

                  (keyword? (first args))
                  (recur (drop 2 args)
                         (assoc options (first args) (second args))
                         methods)

                  :else
                  (recur (rest args) options
                         (assoc methods (ffirst args) (first args)))))

          sname (u/simple-name name)
          prefix (or prefix (str sname "-"))
          state (or state `(atom {}))]
      `(do
         (gen-class
          :name ~name
          :main false
          :prefix ~prefix
          :init "init"
          :state "state"
          :extends ~(or extends Activity)
          :implements ~(conj implements neko.ActivityWithState)
          :overrides-methods ~(keys methods)
          :exposes-methods {~'onCreate ~'superOnCreate
                            ~'onStart ~'superOnStart
                            ~'onRestart ~'superOnRestart
                            ~'onResume ~'superOnResume
                            ~'onPause ~'superOnPause
                            ~'onStop ~'superOnStop
                            ~'onCreateContextMenu ~'superOnCreateContextMenu
                            ~'onContextItemSelected ~'superOnContextItemSelected
                            ~'onCreateOptionsMenu ~'superOnCreateOptionsMenu
                            ~'onOptionsItemSelected ~'superOnOptionsItemSelected
                            ~'onActivityResult ~'superOnActivityResult
                            ~'onNewIntent ~'superOnNewIntent
                            ~'onDestroy ~'superOnDestroy})
         ~`(defn ~(symbol (str prefix "init"))
             [] [[] ~state])
         ~`(defn ~(symbol (str prefix "getState"))
             [~(vary-meta 'this assoc :tag name)]
             (.state ~'this))
         ~(when-let [[mname args & body] (get methods 'onCreate)]
            (let [[super-call body] (if (= (ffirst body) '.superOnCreate)
                                      [(first body) (rest body)]
                                      [nil body])]
              `(defn ~(symbol (str prefix mname))
                 [~(vary-meta (first args) assoc :tag name)
                  ~(vary-meta (second args) assoc :tag android.os.Bundle)]
                 ~super-call
                 (.put all-activities '~(.name *ns*) ~'this)
                 ~(when key
                    `(.put all-activities ~key ~'this))
                 (neko.init/init (.getApplicationContext ~'this))
                 ~(when features
                    `(request-window-features! ~'this ~@features))
                 (safe-for-ui ~@body))))
         ~@(for [[_ [mname args & body]] (dissoc methods 'onCreate)]
             `(defn ~(symbol (str prefix mname))
                [~(vary-meta (first args) assoc :tag name)
                 ~@(rest args)]
                ~@body
                true))))))

(defn get-state [^ActivityWithState activity]
  (.getState activity))

(defn simple-fragment
  "Creates a fragment which contains the specified view. If a UI tree
  was provided, it is inflated and then set as fragment's view."
  ([context tree]
     (simple-fragment (make-ui context tree)))
  ([view]
     (proxy [Fragment] []
       (onCreateView [inflater container bundle]
         (if (instance? View view)
           view
           (do
             (println "One-argument version is deprecated. Please use (simple-fragment context tree)")
             (make-ui view)))))))
