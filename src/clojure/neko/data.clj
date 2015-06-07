(ns neko.data
  "Contains utilities to manipulate data that is passed between
  Android entities via Bundles and Intents."
  (:refer-clojure :exclude [assoc!])
  (:import android.os.Bundle android.content.Intent
           android.content.SharedPreferences
           android.content.SharedPreferences$Editor
           android.content.Context
           neko.App))

(defprotocol GenericExtrasKey
  "If given a string returns itself, otherwise transforms a argument
  into a string."
  (generic-key [key]))

(extend-protocol GenericExtrasKey
  String
  (generic-key [s] s)

  clojure.lang.Keyword
  (generic-key [k] (.getName k)))

;; This type acts as a wrapper around Bundle instance to be able to
;; access it like an ordinary map.
;;
(deftype MapLikeBundle [^Bundle bundle]
  clojure.lang.Associative
  (containsKey [this k]
    (.containsKey bundle (generic-key k)))
  (entryAt [this k]
    (clojure.lang.MapEntry. k (.get bundle (generic-key k))))
  (valAt [this k]
    (.get bundle (generic-key k)))
  (valAt [this k default]
    (let [key (generic-key k)]
      (if (.containsKey bundle key)
        (.get bundle (generic-key key))
        default)))
  (seq [this]
    (map (fn [k] [k (.get bundle k)])
         (.keySet bundle))))

;; This type wraps a HashMap just redirecting the calls to the
;; respective HashMap methods. The only useful thing it does is
;; allowing to use keyword keys instead of string ones.
;;
(deftype MapLikeHashMap [^java.util.HashMap hmap]
  clojure.lang.Associative
  (containsKey [this k]
    (.containsKey hmap (generic-key k)))
  (entryAt [this k]
    (clojure.lang.MapEntry. k (.get hmap (generic-key k))))
  (valAt [this k]
    (.get hmap (generic-key k)))
  (valAt [this k default]
    (let [key (generic-key k)]
      (if (.containsKey hmap key)
        (.get hmap (generic-key key))
        default)))
  (seq [this]
    (map (fn [k] [k (.get hmap k)])
         (.keySet hmap))))

(defprotocol MapLike
  "A protocol that helps to wrap objects of different types into
  MapLikeBundle."
  (like-map [this]))

(extend-protocol MapLike
  Bundle
  (like-map [b]
    (MapLikeBundle. b))

  Intent
  (like-map [i]
    (if-let [bundle (.getExtras i)]
      (MapLikeBundle. bundle)
      {}))

  SharedPreferences
  (like-map [sp]
    (MapLikeHashMap. (.getAll sp)))

  nil
  (like-map [_] {}))

;; SharedPreferences utilities

(def ^:private sp-access-modes {:private Context/MODE_PRIVATE
                                :world-readable Context/MODE_WORLD_READABLE
                                :world-writeable Context/MODE_WORLD_WRITEABLE})

(defn get-shared-preferences
  "Returns the SharedPreferences object for the given name. Possible modes:
  `:private`, `:world-readable`, `:world-writeable`."
  ([name mode]
   (get-shared-preferences App/instance name mode))
  ([^Context context, name mode]
   {:pre [(or (number? mode) (contains? sp-access-modes mode))]}
   (let [mode (if (number? mode)
                mode (sp-access-modes mode))]
     (.getSharedPreferences context name mode))))

(defn ^SharedPreferences$Editor assoc!
  "Puts the value into the SharedPreferences editor instance. Accepts
  limited number of data types supported by SharedPreferences."
  [^SharedPreferences$Editor sp-editor, key value]
  (let [key (generic-key key)]
    (condp #(= (type %2) %1) value
      java.lang.Boolean (.putBoolean sp-editor key value)
      java.lang.Float  (.putFloat sp-editor key value)
      java.lang.Double (.putFloat sp-editor key (float value))
      java.lang.Integer (.putInt sp-editor key value)
      java.lang.Long    (.putLong sp-editor key value)
      java.lang.String (.putString sp-editor key value)
      ;; else
      (throw (Exception. (str "SharedPreferences doesn't support type: "
                              (type value)))))))

(defn ^SharedPreferences$Editor assoc-arbitrary!
  "Puts `value` of an arbitrary Clojure data type into given
  SharedPreferences editor instance. Data is printed into a string and
  stored as a string value."
  [^SharedPreferences$Editor sp-editor key value]
  (let [key (generic-key key)]
    (.putString sp-editor key (pr-str value))))

(defn get-arbitrary
  "Gets a string by given key from a SharedPreferences
  HashMap (wrapped with `like-map`) and transforms it into a data
  value using Clojure reader."
  [sp-map key]
  (when-let [val (get sp-map key)]
   (read-string val)))

(def sp "SharedPreferences manager for the application." (atom nil))
(def ^:private preferences "Set of atoms to keep track of." (atom #{}))

(defn- watch
  "Watch function for saving preferences whenever they are edited."
  [^SharedPreferences sp _key ref old new]
  (when-not (= old new)
    (when-not sp
      (throw (java.lang.Exception
              (str "shared-preferences not initialized: " sp))))
    (locking sp
      (-> (.edit sp)
          (neko.data/assoc-arbitrary! (:sp-key (meta ref)) new)
          .commit))))

(defn track-and-set!
  "Set the value of atom `a` according to the value stored in
  `shared-prefs` (if any), then add a watch to it so any changes in
  its value are updated in `shared-prefs`.
  shared-prefs defaults to `sp`.
  The atom must have an `:sp-key` meta property holding a string or a
  keyword."
  ([a]
   (if @sp
     (track-and-set! a @sp)
     (throw (java.lang.Exception "shared-preferences not initialized"))))
  ([a ^SharedPreferences sp]
   {:pre [(instance? clojure.lang.Atom a)
          (:sp-key (meta a))]}
   (let [key (:sp-key (meta a))]
     (locking sp
       (try (let [msp (neko.data/like-map sp)]
              (when (contains? msp key)
                (reset! a (neko.data/get-arbitrary msp key))))
            (catch java.lang.Exception e
              (neko.log/e "Preference" key "couldn't be read, disregarding"))))
     (add-watch a :shared-preferences-save-tracker
                (partial watch sp)))))

(defn initialize-preferences
  "Restore the recorded value of all preferences, and configure them
  to be saved when changed. Call this only once per application
  lifetime.

  This function can be invoked with either a SharedPreferences object,
  or a name and a Context (as per `get-shared-preferences`). In the
  latter case, this function will have no effect if preferences have
  already been initialized in this session, unless a third truthy
  argument is provided."
  ([^Context context name]
   (initialize-preferences context name false))
  ([^Context context name force]
   (when (or force (not @sp))
     (initialize-preferences (neko.data/get-shared-preferences context name :private))))
  ([^SharedPreferences shared-prefs]
   (reset! sp shared-prefs)
   (doseq [p @preferences]
     (track-and-set! p shared-prefs))))

(defmacro defpreference
  "Define a shared preference, i.e., an atom whose value is saved
  between sessions. Defines a var with `name`, whose value is an atom
  containing `value`, optionally including a docstring. The actual
  saving and restoring only takes place once the function
  initialize-preferences is called, which should happen only once per
  application lifetime, so call it in the onCreate of your main activity.

  Once the function is called, any previously saved value will
  override `value`, and a watcher will be used to always save the
  value again when the atom is changed.

  Additional keyword arguments accepted are:
  :version -- Change this number to ignore previously saved values.
  :key -- The key to use in `sp`, defaults to \"sp/namespace/name/version\".
          Setting this invalidates the version argument.
  :meta and :validator -- Passed to the atom."
  ([name value]
   `(defpreference ~name nil ~value))
  ([name doc value & rest]
   (if (odd? (count rest))
     `(defpreference ~name nil ~doc ~value ~@rest)
     (let [[& {:keys [key version meta validator]}] rest
           sp-key (or key (str "sp/" *ns* "/" name "/" version))]
       `(do (def ~name ~@(if doc [doc])
              (atom ~value
                    :meta ~(into {:sp-key sp-key} meta)
                    :validator ~validator))
            (swap! preferences conj ~name))))))
